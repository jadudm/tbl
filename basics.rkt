#lang racket
(require db sql
         tbl/types
         tbl/util/sqlite
         tbl/util/util
         tbl/util/sanity
         )

;; FIXME: Get contract-out in place.
(provide (all-defined-out))

(define (write-meta-file base-name name columns types)
  (define (hash-write of k v)
    (cond
      [(or (symbol? v) (string? v))
       (write `(hash-set! permanent-tbl (quote ,k) ,(format "~a" v)) of)]
      [else
       (write `(hash-set! permanent-tbl (quote ,k) (quote ,v)) of)])
    (newline of))
  
  (define of (open-output-file (format "~a.tbl" base-name)))
  (fprintf of "#lang racket~n~n")
  (write '(provide permanent-tbl) of)
  (newline of)
  
  (write '(define permanent-tbl (make-hash)) of)
  (hash-write of 'base-filename base-name)
  (hash-write of 'name name)
  (hash-write of 'columns columns)
  (hash-write of 'types types)
  (close-output-port of)
  )
  
(define (make-tbl* name columns types #:keep [keep false])
  (define conn (void))
  
  ;; Should these be temporary, or in-memory?
  (cond
    [keep
     (define tbl-base-filename (format "~a-~a" name (current-seconds)))
     (define sqlite-name (format "~a.sqlite" tbl-base-filename))
    
     (let ([p (open-output-file sqlite-name)])
       (close-output-port p))
     (write-meta-file tbl-base-filename name columns types)
     (set! conn (sqlite3-connect #:database sqlite-name))]
    [(not keep)
     (set! conn (sqlite3-connect #:database 'temporary))]
    )

  ;; Create the backing table
  ;; https://www.sqlite.org/lang_createtable.html#rowid
  ;; ROWID is an INTEGER PRIMARY KEY automatically.
  ;; So... perhaps we do not need an index column?
  (define S
    (format "CREATE TABLE ~a (rowid INTEGER PRIMARY KEY)"
            (clean-sql-table-name name)))
  (define T
    (tbl (clean-sql-table-name name) 'sqlite3 empty empty conn))
  ;; (printf "~s~n" S)
  (query-exec conn S)
  
  (for ([f columns] [t types])
    (add-column T f t))
  T
  )

(define make-tbl
  (match-lambda*
    ['() (make-tbl "coffee" empty empty)]
    [(list (? string-or-symbol? name))
     (make-tbl* (~a name) empty empty #:keep false)]
    [(list (? string-or-symbol? name)
           (? (λ (ls) (andmap string-or-symbol? ls)) columns)
           (? (λ (ls) (andmap string-or-symbol? ls)) types))
     (make-tbl* name columns types #:keep false)
     ]
    ))

(define (make-permanent-tbl name columns types)
  (make-tbl* name columns types #:keep true))

;; FIXME
;; Make sure fields conform to SQL naming conventions
;; Make sure types are valid.
;; Must handle situation where column exists.
(define (add-column T column type)
  (define cleaned (clean-column-name column))
  (set-tbl-columns! T (snoc cleaned (tbl-columns T)))
  (set-tbl-types!  T (snoc type (tbl-types T)))
  
  (define S (format "ALTER TABLE ~a ADD COLUMN ~a ~a"
                    (tbl-name T)
                    cleaned (tbl-type->sqlite-type type)))
  
  ;; (printf "~s~n" S)
  (query-exec (tbl-db T) S)
  T)

(define add-row
  (match-lambda*
    [(list (? tbl? T)
           (? list? values))
     ;; Need to find values that are empty, and convert them to SQL-NULL values.
     (set! values (map (λ (v) (if (or (equal? v "")
                                      (none? v)
                                      (equal? v 'none))
                                  "NULL"
                                  v)) values))
     
     (define S (format "INSERT INTO ~a ~a VALUES ~a"
                       (tbl-name T)
                       (add-between (tbl-columns T) ",")
                       (add-between (map quote-sql values) ",")))
     ;;(printf "~s~n" S)
     ;; Insert the value into the DB.
     (query-exec (tbl-db T) S)]

    ;; If I'm given a vector
    [(list (? tbl? T)
           (? vector? values))
     (add-row T (vector->list values))]

    ;; Or, if we have values passed in as parameters.
    [(list (? tbl? T)
           values ...)
     (add-row T values)]
    ))

(define (count-columns T)
  (length (tbl-columns T)))

(define (count-rows T)
  (query-value (tbl-db T) (format "SELECT count(*) FROM ~a" (tbl-name T))))

(define column-names tbl-columns) 



;; TESTS

(module+ test
  (require rackunit/chk)

  ;; Creating an empty table.
  (define empty-table (make-tbl))

  ;; Setting the name of an empty table.
  (define T1 (make-tbl))
  (set-tbl-name! T1 "Transterpreter")

  ;; Creating a table with a name.
  (define T2 (make-tbl "concurrency.cc"))

  ;; Creating a table with columns and types
  (define T3 (make-tbl "jadud.com" '(a b c) '(number number text)))

  ;; Adding a column
  (define T4 (make-tbl))
  (add-column T4 'bob 'integer)
  ;; Adding a row
  (add-row T4 '(1))

  ;; A more table
  (define T5 (make-tbl "grocery"
                       '(product qty cost)
                       '(text integer real)))
  (add-row T5 '(apple 10 1.35))
  (add-row T5 '(kiwi  20 0.75))
  ;; Check that the .args version works.
  (add-row T5 'nuts 100 0.02)

  (chk
   #:t (tbl? (make-tbl))
   (tbl-name T1) "Transterpreter"
   (tbl-name T2) "concurrencycc"
   (length (tbl-columns T3)) 3
   (length (tbl-types T3))  3

   ;; Check if the insert into T5 works.
   (query-rows (tbl-db T5) (select (.*)
                                   #:from (TableRef:INJECT ,(tbl-name T5))))
   '(#(1 "apple" 10 1.35) #(2 "kiwi" 20 0.75) #(3 "nuts" 100 0.02))

   ;;(printf "~s~n" (column-names T5))
   ))
