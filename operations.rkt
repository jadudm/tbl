#lang racket

(require db sql
         tbl/basics
         tbl/types
         tbl/util/util
         tbl/util/sqlite
         tbl/reading/gsheet
         tbl/util/csv
         )

(provide (all-defined-out)
         (rename-out [pull get-column])
         )

;; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ;
;; PULL
(define (pull T column)
  (define Q (select (ScalarExpr:INJECT ,(->string column))
                    #:from (TableRef:INJECT ,(tbl-name T))))
  (query-list (tbl-db T) Q))

;; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ;
;; PICK
;; Returns a new table with the named columns.
(define (pick T . columns)
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
  )


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

;; Gets all the rows as a list of lists.
;; FIXME: This includes the ROWID. Should
;; the ROWID be included when communicating back to the user?
;; For now, I'm going to say NO, because the user didn't
;; choose to put that value in their data.
(define (get-rows T)
  (define Q (select (.*) #:from (TableRef:INJECT ,(tbl-name T))))
  (define rows (query-rows (tbl-db T) Q))
  (map rest (map vector->list rows)))

;; With ID
(define (get-rows-with-id T)
  (define Q (select (.*) #:from (TableRef:INJECT ,(tbl-name T))))
  (define rows (query-rows (tbl-db T) Q))
  (map vector->list rows))

(define flavorsT (read-gsheet "https://pult.us/u/flavors"))


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
  (define vals (pull T col))
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
  
(define (remove-ndx n ls)
  (cond
    [(empty? ls) empty]
    [(zero? n) (remove-ndx (sub1 n) (rest ls))]
    [else
     (cons (first ls)
           (remove-ndx (sub1 n) (rest ls)))]))

(define (remove-column T col)
  (define ndx (index-of (column-names T) col))
  (define new-columns (remove-ndx ndx (column-names T)))
  (define new-types   (remove-ndx ndx (tbl-types T)))
  (define newT (make-tbl (tbl-name T)
                         new-columns
                         new-types))
  ;; Create a new DB of data.
  (for ([row (get-rows T)])
    (add-row newT (remove-ndx ndx row)))
  ;; Close the old connection
  (disconnect (tbl-db T))
  ;; Set the old table to point to the new one.
  (set-tbl-db! T (tbl-db newT))
  ;; Update the columns.
  (set-tbl-columns! T new-columns)
  (set-tbl-types! T new-types)
  T)
                                
(define (remove-columns T . cols)
  (for ([c cols])
    (set! T (remove-column T c)))
  T)

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
    

;  ;;;;;;; ;;;;;;   ;;;;; ;;;;;;;  ;;;;; 
;     ;    ;       ;     ;   ;    ;     ;
;     ;    ;       ;     ;   ;    ;     ;
;     ;    ;       ;         ;    ;      
;     ;    ;;;;;;   ;;;;;    ;     ;;;;; 
;     ;    ;             ;   ;          ;
;     ;    ;       ;     ;   ;    ;     ;
;     ;    ;       ;     ;   ;    ;     ;
;     ;    ;;;;;;   ;;;;;    ;     ;;;;; 
;                                        
;                                        
;                                        

(module+ test
  (require rackunit/chk
           tbl/reading/gsheet
           tbl/reading/csv
           tbl/test/files
           tbl/test/data
           math)
  
  (define flavorsT (read-gsheet "https://pult.us/u/flavors"))
  (define citiesT (read-gsheet "http://bit.ly/cities-csv"))
 
  (chk
   ;; What are the ages in the flavors GSheet?
   (pull flavorsT 'age)  '(42 9 5)
   ;; What if I use a string for the column name?
   (pull flavorsT "age") '(42 9 5)

   ;; What are the cities in the tbl?
   ;; Placed the list of cities in a test/data file.
   (pull citiesT "City") all-cities
   

   ;; What is the sum of the latitude degrees?
   ;; This checks that the integers came in as integers.
   (apply + (pull citiesT "LatD")) 4969
   ;; How many times does Ohio show up in the tbl?
   (length (filter (λ (s) (equal? s "OH")) (pull citiesT "State"))) 6
   ;; What about Maine? Oh. Maine gets no love.
   (length (filter (λ (s) (equal? s "ME")) (pull citiesT "State"))) 0

   ;; Does pick return a table with fewer columns?
   (count-columns flavorsT) 3
   (count-columns (pick flavorsT "age" "name")) 2

   ;; All the rows where a condition holds true.
   ;; Note that ROWID is not included when we ask for all of the rows.
   (get-rows (filter-rows flavorsT (and (> age 3)
                                        (or (= flavor "Mint")
                                            (= flavor "Chocolate")))))
   '(("Matt" 42 "Chocolate") ("Matthew" 9 "Mint"))

   ;; Another check; essentially same as the previous
   (count-rows
    (filter-rows flavorsT (and (> age 3)
                               (or (= flavor "Mint")
                                   (= flavor "Chocolate")))))
   2
   
   ;; Check that we end up with a new table that only has
   ;; six rows in it. It should still have 10 columns.
   (count-rows (filter-rows citiesT (= State "OH"))) 6
   (count-columns (filter-rows citiesT (= State "OH"))) 10
   
   ) ;; end of chk

  ;; Try extending the table, and computing new values.
  (add-column flavorsT "double_age" Integer)
  (compute flavorsT
           "double_age"
           (function (age) (* age 2)))

  (chk
   ;; FIXME When I handle params that are not named correctly,
   ;; this test may need to be updated.
   #:exn (compute flavorsT
                  "double_age"
                  (function (agex) (* agex 2)))
   exn:fail?
   
   (count-columns flavorsT) 4
   ;; These do the same thing two different ways.
   (map fourth (get-rows flavorsT)) '(84 18 10)
   (pull flavorsT "double_age") '(84 18 10)
   )
    
  (define sdT (read-csv school-shootings))
  
  (chk
   (count-rows sdT) 221
   (count-rows (filter-rows sdT (> killed 0))) 59
   )
  
  ;; Using a bigger table
  #;(begin
      (define gunsT (read-csv gun-deaths-csv))
      (chk
     
       ;; What about a bigger tbl?
       (length (pull gunsT 'age)) 100798
       (count-rows gunsT) 100798
       ;; The columns in this table are
       ;; "col0","year","month","intent","police","sex","age","race","hispanic","place","education"
       ;; Check if we can pick three from the table
       (count-columns (pick gunsT "year" "sex" "race")) 3
       ))

    
  )

;; Deleting the nth row
;; https://stackoverflow.com/questions/23791239/sqlite-delete-nth-row-android