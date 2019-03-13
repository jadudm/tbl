#lang racket
(require db sql
         table/types
         table/util/sqlite
         csv-reading
         net/url)

(provide (all-defined-out))

(define create-table
  (case-lambda
    [() (create-table "coffee" empty empty)]
    [(name) (create-table name empty empty)]
    [(name columns types)
     (define conn (sqlite3-connect #:database 'memory))
     ;; Create the backing table
     (define S (format "CREATE TABLE ~a (_index INTEGER PRIMARY KEY AUTOINCREMENT)"
                       (clean-sql-table-name name)))
     (define T (table (clean-sql-table-name name) 'sqlite3 empty empty conn))
     (printf "~s~n" S)
     (query-exec conn S)
     
     (for ([f columns] [t types])
       (add-column! T f t))
     T
     ]
    ))

;; FIXME
;; Make sure fields conform to SQL naming conventions
;; Make sure types are valid.
(define (add-column! T column type)
  (define S (format "ALTER TABLE ~a ADD COLUMN ~a ~a"
                    (table-name T)
                    column (table-type->sqlite-type type)))
  (set-table-fields! T (snoc column (table-fields T)))
  (set-table-types!  T (snoc type (table-types T)))
  (printf "~s~n" S)
  (query-exec (table-db T) S))

(define add-row!
  (match-lambda*
    [(list (? table? T)
           (? list? values))
     (define S (format "INSERT INTO ~a ~a VALUES ~a"
                       (table-name T)
                       (add-between (table-fields T) ",")
                       (add-between (map quote-sql values) ",")))
     (printf "~s~n" S)
     (query-exec (table-db T) S)]
    [(list (? table? T)
           values ...)
     (add-row! T values)]
    ))
     

;; TESTS

(module+ test
  (require rackunit/chk)

  ;; Creating an empty table.
  (define empty-table (create-table))

  ;; Setting the name of an empty table.
  (define T1 (create-table))
  (set-table-name! T1 "Transterpreter")

  ;; Creating a table with a name.
  (define T2 (create-table "concurrency.cc"))

  ;; Creating a table with columns and types
  (define T3 (create-table "jadud.com" '(a b c) '(number number text)))

  ;; Adding a column
  (define T4 (create-table))
  (add-column! T4 'bob 'integer)
  ;; Adding a row
  (add-row! T4 '(1))

  ;; A more table
  (define T5 (create-table "grocery" '(product qty cost) '(text integer real)))
  (add-row! T5 '(apple 10 1.35))
  (add-row! T5 '(kiwi  20 0.75))
  ;; Check that the .args version works.
  (add-row! T5 'nuts 100 0.02)

  (chk
   #:t (table? (create-table))
   (table-name T1) "Transterpreter"
   (table-name T2) "concurrencycc"
   (length (table-fields T3)) 3
   (length (table-types T3))  3

   ;; Check if the insert into T5 works.
   (query-rows (table-db T5) (select (.*) #:from (TableRef:INJECT ,(table-name T5))))
   '(#(1 "apple" 10 1.35) #(2 "kiwi" 20 0.75) #(3 "nuts" 100 0.02))
   
   ))
  