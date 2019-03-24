#lang racket
(require tbl/util/util
         tbl/types)
                       
(provide (all-defined-out))

(define (clean-sql-table-name str)
  (regexp-replace* #px"[\\W]" str ""))

(define (tbl-type->sqlite-type t)
  (case (->symbol t)
    [(int integer number numeric) 'integer]
    [(real) 'real]
    [(text textual txt) 'text]
    [(blob any) 'blob]
    [(date) 'text]
    [else
     (error 'tbl-type
            "Table types must be one of [numeric, textual, date, blob]. You provided '~a'."
            t)]
    ))

(define (quote-sql o)
  (cond
    [(number? o) o]
    ;; Do not quote null.
    [(equal? o "NULL") "NULL"]
    [(none? o) "NULL"]
    [(symbol? o) (format "'~a'" (symbol->string o))]
    [(string? o) (format "'~a'" (regexp-replace* #px"'" o "''"))]))