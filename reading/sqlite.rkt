#lang racket
(require db
         tbl/types
         tbl/basics
         tbl/util/util
         tbl/util/sqlite)

(provide read-tbl)

(define (read-tbl file)
  (define permanent-tbl (dynamic-require file 'permanent-tbl))
  (define conn (sqlite3-connect #:database (format "~a.sqlite" (hash-ref permanent-tbl 'base-filename))))
  (define table-name
    (clean-sql-table-name (hash-ref permanent-tbl 'name)))
  (define newT (make-tbl table-name
                         (map ->string (hash-ref permanent-tbl 'columns))
                         (map ->string (hash-ref permanent-tbl 'types))))
  (define Q (format "SELECT * FROM ~a" table-name))
  (for ([row (query-rows conn Q)])
    ;; Drop the rowid?
    (add-row! newT (rest (vector->list row))))
  newT)