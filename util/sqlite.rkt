#lang racket
(require table/util/util)
                       
(provide (all-defined-out))

(define (snoc o ls)
  (reverse (cons o (reverse ls))))

(define (clean-sql-table-name str)
  (regexp-replace* #px"[\\W]" str ""))

(define (table-type->sqlite-type t)
  (case (->symbol t)
    [(int integer number numeric) 'integer]
    [(real) 'real]
    [(text textual txt) 'text]
    [(blob any) 'blob]
    [(date) 'text]
    [else
     (error 'table-type
            "Table types must be one of [numeric, textual, date, blob]. You provided '~a'."
            t)]
    ))

(define (quote-sql o)
  (cond
    [(number? o) o]
    [(symbol? o) (format "'~a'" (symbol->string o))]
    [(string? o) (format "'~a'" (regexp-replace #px"'" o "''"))]))