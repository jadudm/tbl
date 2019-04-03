#lang racket
(require db)

(provide (all-defined-out))

(struct tbl (name type columns types db)
  #:transparent
  #:mutable)

;; For ease.
;; And, this is the wrong way to do it.
;; I should use a struct.
;; FIXME
(define none            sql-null)
(define not-available   sql-null)

;; FIXME: Need to pick one.
(define na?             sql-null?)
(define NA?             sql-null?)
(define not-available?  sql-null?)
(define nothing?        sql-null?)
(define none?           sql-null?)

;; For use in expressions.
(define Integer 'integer)
(define Text 'text)
(define Blob 'blob)
(define Real 'real)
(define Date 'text)