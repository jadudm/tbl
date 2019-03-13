#lang racket

(provide (all-defined-out))

(define (snoc o ls)
  (reverse (cons o (reverse ls))))

(define (->symbol o)
  (string->symbol (~a o)))

(define (clean-sql-table-name str)
  (regexp-replace #px"[\\W]" str ""))

(define (table-type->sqlite-type t)
  (case (->symbol t)
    [(int integer number numeric) 'integer]
    [(real) 'real]
    [(text textual txt) 'text]
    [(blob any) 'blob]))