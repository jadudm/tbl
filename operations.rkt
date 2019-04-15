#lang racket

(require db sql
         tbl/basics
         tbl/types
         tbl/util/util
         tbl/util/sqlite
         tbl/reading/gsheet
         tbl/util/csv
         )

(provide (contract-out
          [get-column                   (-> tbl? string? list?)]
          ;; Required arguments, optional arguments, the rest arguments, and the return type
          [get-columns                  (->* (tbl?) () #:rest (listof string?) (listof list?))]
          [new-tbl-from-columns         (->* (tbl?) () #:rest (listof string?) tbl?)]
          [copy-tbl                     (-> tbl? tbl?)]
          
          [get-rows                     (-> tbl? (listof list?))]
          [get-rows-with-id             (-> tbl? (listof list?))]

          [remove-columns               (->* (tbl?) () #:rest (listof string?) tbl?)]
          ))
(provide filter-rows compute)

;; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ;
;; get-column
;; Extracts a column as a list.
(define (get-column T column)
  
  (define Q (select (ScalarExpr:INJECT ,(->string column))
                    #:from (TableRef:INJECT ,(tbl-name T))))
  (query-list (tbl-db T) Q))

;; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ;
;; get-columns
;; Extracts multiple columns into a list of lists.
;; Given no columns, it returns an empty list.
(define (get-columns T . columns)
  (map (λ (col)
         (get-column T col))
       columns))

;; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ;
;; copy-table
;; Creates a copy of a tbl.
(define (copy-tbl T)
  (define newT (make-tbl (tbl-name T)
                         (tbl-columns T)
                         (tbl-types T)))  
  (for ([row (get-rows T)])
    (add-row newT row))
  newT)

;; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ;
;; new-table-from-columns
;; Returns a new table with the named columns.
(define (new-tbl-from-columns T . columns)
  (cond
    ;; If I get no columns, make a copy of the table.
    [(empty? columns)
     (copy-tbl T)]
    ;; Otherwise, create a new table, and copy only the columns given.
    [else
     (define Q (format "SELECT ~a FROM ~a"
                       (apply string-append (add-between (map ->string columns) ","))
                       (tbl-name T)))
     ;; (printf "~s~n" Q)
     (define rows (query-rows (tbl-db T) Q))
     ;; I want a table to come back. So, create a new table, and insert all the things.
     (define newT (make-tbl (format "~a_~a" (tbl-name T)
                                    (apply string-append (add-between columns "_")))
                            columns
                            (map (λ (col)
                                   (list-ref (tbl-types T)
                                             (index-of columns col)))
                                 columns)))
     (for ([row rows])
       (add-row newT row))
     newT
     ]))


(define (logop? s)
  (member s '(and AND & && or OR || ||||)))
(define (->logop s)
  (case s
    [(and & &&) 'AND]
    [(or || ||||) 'OR]))

(define (binop? s)
  (member s '(+ - * / > < >= <= = ==)))
(define (->binop s)
  (case s
    [(==) '=]
    [else s]))

(define (monop? op)
  (member op '(not !=)))

(define (->monop o)
  (case o
    [(not !=) 'NOT]
    [(not-na not-null) "IS NOT NULL"]
    ))

(define (->infix exp col-names)
  (match exp
    [(? number? n) n]
    ;; For columns, quote them differently.
    [(? string-or-symbol? s)
     (if (member (->string s) col-names)
         (->string s)
         (quote-sql (->string s))
         )
     ]
    [(list (or 'not-na 'not-null) rand)
     (format "(~a IS NOT NULL)" rand)]
    [(list (? monop? op) rand)
     (format "~a (~a)" (->monop op) (->infix rand col-names))]
    [(list (? binop? op) lhs rhs)
     (format "(~a ~a ~a)" (->infix lhs col-names) (->binop op) (->infix rhs col-names))]
    [(list (? logop? op) args ...)
     (apply string-append
      (add-between (map (λ (rand) (format "~a" (->infix rand col-names))) args)
                   (format " ~a " (->logop op))))]
    ))

;; FIXME
;; Is this
;; * filter-rows
;; * filter-table 
;; * select-rows
;; * subset
;; * select (which is already taken by the sql library)
;; * choose
;; dplyr uses "filter()", which is taken by Racket.
;; should I use "-table" on table operations?
;; pull-from-table, filter-table, ... ?

(define (filter-rows:Q T quotedQ)
  (define names (column-names T))
  ;; (printf "~s~n" names)
  (define infixQ (->infix quotedQ (column-names T)))
  ;; (printf "~s~n" infixQ)
  (define Q (select (.*)
                    #:from (TableRef:INJECT ,(tbl-name T))
                    #:where (ScalarExpr:INJECT ,infixQ)))
  ;;(printf "Q: ~s~n" Q)
  (define rows (query-rows (tbl-db T) Q))
  (define newT (make-tbl (format "~a_filtered" (tbl-name T))
                         (tbl-columns T)
                         (tbl-types T)))
  (for ([row rows])
    ;; Drop the ROWID when inserting.
    (add-row newT (rest (vector->list row))))
  newT)

(define-syntax-rule (filter-rows T Q)
  (filter-rows:Q T (quasiquote Q)))

;; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ;
;; get-rows
;; Gets all the rows as a list of lists.
(define (get-rows T)
  (define Q (select (.*) #:from (TableRef:INJECT ,(tbl-name T))))
  (define rows (query-rows (tbl-db T) Q))
  (map rest (map vector->list rows)))

;; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ;
;; get-rows-with-id
;; Gets all the rows as a list of lists, and
;; leaves the ROWID at the front of every row
(define (get-rows-with-id T)
  (define Q (select (.*) #:from (TableRef:INJECT ,(tbl-name T))))
  (define rows (query-rows (tbl-db T) Q))
  (map vector->list rows))

;; (define flavorsT (read-gsheet "https://pult.us/u/flavors"))


;; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ;
;; COMPUTE
;; Computes a new row based on other rows.

;; FIXME: Must handle case where column does not exist yet.
;; FIXME: Must check that the function parameters are
;; valid names of columns in the table.
;; FIXME: After a computation, the type of the column may have changed.
;; This is a problem. SQlite doesn't care, but at the least... well, at the
;; least the column types should be updated in the struct.


;; FIXME
;; THis happens in "reading/csv/util or somesuch.
;; This should be written in ONE PLACE ONLY.
(define (update-column-type T col)
  (define vals (get-column T col))
  (define type
    (cond
      [(mostly? (maybe integer?) vals) 'integer]
      [(mostly? (maybe number?) vals) 'real]
      [(mostly? string? vals) 'text]
      [else 'blob]))
  (define colndx (index-of (tbl-columns T) col))
  (define current-type (list-ref (tbl-types T) colndx))
  ;; If they're different, update the table.
  (when (not (equal? current-type type))
    (set-tbl-types! T (list-set (tbl-types T) colndx type)))
  T
  )

;; FIXME
;; There's a lot of errors I have to catch.
;; For example, what happens if they use a param that
;; is not a table column? It throws an error about the arg.
;; Also, if the column name given doesn't exist?
(define-syntax (compute stx)
  (syntax-case stx ()
    [(_c T new-row
         (function (args ...) body bodies ...))
     (with-syntax ([(ids ...) (generate-temporaries #'(args ...))])
       #`(let #,(for/list ([arg (syntax->list #'(args ...))])
                  (with-syntax ([arg arg])
                    #`[arg (λ (row)
                             (list-ref row
                                       (index-of (column-names T)
                                                 (format "~a" 'arg))))]))
           (define rows (get-rows-with-id T))
           (define (Q rowid v)
             (format "UPDATE ~a SET ~a = ~a WHERE ROWID = ~a"
                     (tbl-name T)
                     new-row (quote-sql v) rowid))
           (for ([row rows])
             (query-exec
              (tbl-db T)
              (Q (first row)
                 (apply (λ (args ...) body bodies ...)
                        (list
                         #,@(for/list ([arg (syntax->list #'(args ...))])
                              (with-syntax ([arg arg])
                                ;; Drop the ROWID when applying the lookup
                                #`(arg (rest row)))))
                        ))))
           (update-column-type T new-row)
           T))
     

     ]))
  
(define (remove-columns T . cols)
  (define tossers (map (λ (c) (index-of (column-names T) c)) cols))
  (define keepers (sort (set-subtract (range 0 (length (tbl-columns T))) tossers) <))
  (define new-columns (map (λ (ndx) (list-ref (tbl-columns T) ndx)) keepers))
  (define new-types   (map (λ (ndx) (list-ref (tbl-types   T) ndx)) keepers))
  ;; Create the new table.
  (define newT (make-tbl (tbl-name T)
                         new-columns
                         new-types))
  (for ([row (get-rows T)])
    (add-row newT (map (λ (ndx) (list-ref row ndx)) keepers)))

  ;; Disconnect from the old table.
  (disconnect (tbl-db T))
  newT)

(define (remove-column T col)
  (remove-columns T col))

(define (column-swap T orig new)
  (list-set (tbl-columns T)
            (index-of (tbl-columns T) orig)
            new))

(define (rename-column T old new)
  (define newT (make-tbl
                (tbl-name T)
                (column-swap T old new)
                (tbl-types T)))
  (for ([row (get-rows T)])
    (add-row newT row))
  (disconnect (tbl-db T))
  newT)
        
;; The data in the CSV is different from what is published in the paper.
;; I'm going to drop "genre" and "date.peaked" to be consistent.
;(set! B (remove-column B "genre"))
;(set! B (remove-column B "datepeaked"))
;(set! B (remove-column B "artistinverted"))
;(set! B (remove-columns B "genre" "datepeaked" "artistinverted"))

(define (melt T . ids)
  (define colvars (list->set ids))
  (define columnS (list->set (column-names T)))
  (define vars    (set-subtract columnS colvars))
  ;; Now, I need a new table. It needs to contain the ids as columns,
  ;; and a new column called... melted. (Why reuse "column"?)
  (define new-types
    (for/list ([id ids])
      (list-ref (tbl-types T)
                (index-of (tbl-columns T) id ))))
    
  (define newT (make-tbl (tbl-name T)
                         (append ids '("variable" "value"))
                         (append new-types '(text text))))
  (for ([row (get-rows T)])
    (define to-insert
      (for/list ([id ids])
        (list-ref row (index-of (tbl-columns T) id ))))
    (for ([v vars]) 
      (define value (list-ref row (index-of (tbl-columns T) v)))
      ;;(printf "ti: ~a~n" to-insert)
      ;;(printf "lvv: ~a~n" (list v value))
      (add-row newT
               (append to-insert (list v value))))
    )
  (disconnect (tbl-db T))
  newT)
    