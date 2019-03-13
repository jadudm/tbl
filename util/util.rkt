#lang racket

(provide (all-defined-out))

(define (string-or-symbol? o)
  (or (string? o) (symbol? o)))
  
(define (snoc o ls)
  (reverse (cons o (reverse ls))))

(define (->symbol o)
  (string->symbol (~a o)))

(define (->string o)
  (~a o))