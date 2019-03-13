#lang racket
(require db sql
         table/types
         table/util/sqlite
         table/util/util
         csv-reading
         net/url)

(provide (all-defined-out))

(define make-table
  (match-lambda*
    ['() (make-table "coffee" empty empty)]
    [(list (? string-or-symbol? name))
     (make-table (~a name) empty empty)]
    [(list (? string-or-symbol? name)
           (? (λ (ls) (andmap string-or-symbol? ls)) columns)
           (? (λ (ls) (andmap string-or-symbol? ls)) types))
     (define conn (sqlite3-connect #:database 'memory))
     ;; Create the backing table
     (define S
       ;; https://www.sqlite.org/lang_createtable.html#rowid
       ;; ROWID is an INTEGER PRIMARY KEY automatically.
       ;; So... perhaps we do not need an index column?
       (format "CREATE TABLE ~a (rowid INTEGER PRIMARY KEY)"
               (clean-sql-table-name name)))
     (define T
       (table (clean-sql-table-name name) 'sqlite3 empty empty conn))
     ;; (printf "~s~n" S)
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
                    (tbl-name T)
                    column (tbl-type->sqlite-type type)))
  (set-table-columns! T (snoc column (tbl-columns T)))
  (set-table-types!  T (snoc type (tbl-types T)))
  ;; (printf "~s~n" S)
  (query-exec (tbl-db T) S))

(define add-row!
  (match-lambda*
    [(list (? table? T)
           (? list? values))
     (define S (format "INSERT INTO ~a ~a VALUES ~a"
                       (tbl-name T)
                       (add-between (tbl-columns T) ",")
                       (add-between (map quote-sql values) ",")))
     ;; (printf "~s~n" S)
     (query-exec (tbl-db T) S)]
    
    [(list (? table? T)
           (? vector? values))
     (add-row! T (vector->list values))]
    
    [(list (? table? T)
           values ...)
     (add-row! T values)]
    ))

(define (column-count T)
  (length (tbl-columns T)))

(define (row-count T)
  (query-value (tbl-db T) (format "SELECT count(*) FROM ~a" (tbl-name T))))

     

;; TESTS

(module+ test
  (require rackunit/chk)

  ;; Creating an empty table.
  (define empty-table (make-table))

  ;; Setting the name of an empty table.
  (define T1 (make-table))
  (set-table-name! T1 "Transterpreter")

  ;; Creating a table with a name.
  (define T2 (make-table "concurrency.cc"))

  ;; Creating a table with columns and types
  (define T3 (make-table "jadud.com" '(a b c) '(number number text)))

  ;; Adding a column
  (define T4 (make-table))
  (add-column! T4 'bob 'integer)
  ;; Adding a row
  (add-row! T4 '(1))

  ;; A more table
  (define T5 (make-table "grocery" '(product qty cost) '(text integer real)))
  (add-row! T5 '(apple 10 1.35))
  (add-row! T5 '(kiwi  20 0.75))
  ;; Check that the .args version works.
  (add-row! T5 'nuts 100 0.02)

  (chk
   #:t (table? (make-table))
   (tbl-name T1) "Transterpreter"
   (tbl-name T2) "concurrencycc"
   (length (tbl-columns T3)) 3
   (length (tbl-types T3))  3

   ;; Check if the insert into T5 works.
   (query-rows (tbl-db T5) (select (.*) #:from (TableRef:INJECT ,(tbl-name T5))))
   '(#(1 "apple" 10 1.35) #(2 "kiwi" 20 0.75) #(3 "nuts" 100 0.02))
   
   ))
  