#lang racket

(require table
         table/types
         table/util/util
         table/util/sqlite
         db sql
         table/reading/gsheet
         )

(provide (all-defined-out))

;; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ;
;; PULL
(define (pull T column)
  (define Q (select (ScalarExpr:INJECT ,(->string column))
                    #:from (TableRef:INJECT ,(table-name T))))
  (query-list (table-db T) Q))

;; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ;
;; SELECT
(define (pick T . columns)
  (define Q (format "SELECT ~a FROM ~a"
                    (apply string-append (add-between (map ->string columns) ","))
                    (table-name T)))
  ;; (printf "~s~n" Q)
  (define rows (query-rows (table-db T) Q))
  ;; I want a table to come back. So, create a new table, and insert all the things.
  (define newT (make-table (format "~a_~a" (table-name T)
                                   (apply string-append (add-between columns "_")))
                           columns
                           (map (λ (col)
                                  (list-ref (table-types T)
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

(define (->infix exp #:v [v false])
  (match exp
    [(? number? n) n]
    [(? string-or-symbol? s) (if v
                                 (quote-sql (->string s))
                                 (->string s))
                                 ]
    [(list (? binop? op) lhs rhs)
     (format "(~a ~a ~a)" (->infix lhs) (->binop op) (->infix rhs #:v true))]
    ))

(define flavorsT (read-gsheet "https://pult.us/u/flavors"))

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
  (define Q (select (.*)
                    #:from (TableRef:INJECT ,(table-name T))
                    #:where (ScalarExpr:INJECT ,(->infix quotedQ))))
  (define rows (query-rows (table-db T) Q))
  (define newT (make-table (format "~a_filtered" (table-name T))
                           (table-columns T)
                           (table-types T)))
  (for ([row rows])
    ;; Drop the ROWID when inserting.
    (add-row! newT (rest (vector->list row))))
  newT)

(define-syntax-rule (filter-rows T Q)
  (filter-rows:Q T (quasiquote Q)))

;; Gets all the rows as a list of lists.
;; FIXME: This includes the ROWID. Should
;; the ROWID be included when communicating back to the user?
;; For now, I'm going to say NO, because the user didn't
;; choose to put that value in their data.
(define (get-rows T)
  (define Q (select (.*) #:from (TableRef:INJECT ,(table-name T))))
  (define rows (query-rows (table-db T) Q))
  (map rest (map vector->list rows)))


(module+ test
  (require rackunit/chk
           table/reading/gsheet
           table/reading/csv
           table/test/files
           table/test/data
           math)
  
  (define flavorsT (read-gsheet "https://pult.us/u/flavors"))
  (define citiesT (read-gsheet "http://bit.ly/cities-csv"))
 
  (chk
   ;; What are the ages in the flavors GSheet?
   (pull flavorsT 'age)  '(42 9 5)
   ;; What if I use a string for the column name?
   (pull flavorsT "age") '(42 9 5)

   ;; What are the cities in the table?
   ;; Placed the list of cities in a test/data file.
   (pull citiesT "City") all-cities
   

   ;; What is the sum of the latitude degrees?
   ;; This checks that the integers came in as integers.
   (apply + (pull citiesT "LatD")) 4969
   ;; How many times does Ohio show up in the table?
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

  ;; Using a bigger table
  #;(begin
    (define gunsT (read-csv gun-deaths-csv))
    (chk
     
   ;; What about a bigger table?
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