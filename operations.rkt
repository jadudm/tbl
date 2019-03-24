#lang racket

(require db sql
         tbl/basics
         tbl/types
         tbl/util/util
         tbl/util/sqlite
         tbl/reading/gsheet
         )

(provide (all-defined-out))

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
    (add-row! newT row))
  newT                           
  )

(define (binop? s)
  (member s '(+ - * / and AND & && or OR || |||| > < >= <= = ==)))
(define (->binop s)
  (case s
    [(and & &&) 'AND]
    [(or || ||||) 'OR]
    [(==) '=]
    [else s]))

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
    [(list (? binop? op) lhs rhs)
     (format "(~a ~a ~a)" (->infix lhs col-names) (->binop op) (->infix rhs col-names))]
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
  ;; (printf "Q: ~s~n" Q)
  (define rows (query-rows (tbl-db T) Q))
  (define newT (make-tbl (format "~a_filtered" (tbl-name T))
                         (tbl-columns T)
                         (tbl-types T)))
  (for ([row rows])
    ;; Drop the ROWID when inserting.
    (add-row! newT (rest (vector->list row))))
  newT)

(define-syntax-rule (filter-rows T Q)
  (filter-rows:Q T (quasiquote Q)))


(require (for-syntax syntax/parse))

(define-syntax (function stx)
  (syntax-parse stx
    [(_f (T args ...) body)
     #`(lambda (row)
         ;; Bind the args to the correct row locations?
         body)]))

;; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ;
;; COMPUTE
;; Computes a new row based on other rows.
(define (compute:Q T new-row fun)
  
  
  
  'pass
  )

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

;; FIXME: Must handle case where column does not exist yet.
;; FIXME: Must check that the function parameters are
;; valid names of columns in the table.
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
                     new-row v rowid))
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
              
           ))]))

;; Must handle situation where column exists.
(define (add-column T name type)
  ;; Add it to the list of column names and types
  (set-tbl-columns! T (append (tbl-columns T) (list name)))
  (set-tbl-types!   T (append (tbl-types T) (list type)))
  (define Q (format "ALTER TABLE ~a ADD COLUMN ~a ~a"
                    (tbl-name T)
                    name type))
  (query-exec (tbl-db T) Q))
  

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
   (column-count flavorsT) 3
   (column-count (pick flavorsT "age" "name")) 2

   ;; All the rows where a condition holds true.
   ;; Note that ROWID is not included when we ask for all of the rows.
   (get-rows (filter-rows flavorsT (and (> age 3)
                                        (or (= flavor "Mint")
                                            (= flavor "Chocolate")))))
   '(("Matt" 42 "Chocolate") ("Matthew" 9 "Mint"))

   ;; Another check; essentially same as the previous
   (row-count
    (filter-rows flavorsT (and (> age 3)
                               (or (= flavor "Mint")
                                   (= flavor "Chocolate")))))
   2
   
   ;; Check that we end up with a new table that only has
   ;; six rows in it. It should still have 10 columns.
   (row-count (filter-rows citiesT (= State "OH"))) 6
   (column-count (filter-rows citiesT (= State "OH"))) 10
   
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
   
   (column-count flavorsT) 4
   ;; These do the same thing two different ways.
   (map fourth (get-rows flavorsT)) '(84 18 10)
   (pull flavorsT "double_age") '(84 18 10)
   )
    
  (define sdT (read-csv school-shootings))
  
  (chk
   (row-count sdT) 221
   (row-count (filter-rows sdT (> killed 0))) 59
   )
  
  ;; Using a bigger table
  #;(begin
      (define gunsT (read-csv gun-deaths-csv))
      (chk
     
       ;; What about a bigger tbl?
       (length (pull gunsT 'age)) 100798
       (row-count gunsT) 100798
       ;; The columns in this table are
       ;; "col0","year","month","intent","police","sex","age","race","hispanic","place","education"
       ;; Check if we can pick three from the table
       (column-count (pick gunsT "year" "sex" "race")) 3
       ))

    
  )

;; Deleting the nth row
;; https://stackoverflow.com/questions/23791239/sqlite-delete-nth-row-android