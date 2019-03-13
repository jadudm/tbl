#lang racket

(provide (all-defined-out))


(define (->symbol o)
  (string->symbol (~a o)))

(define (->string o)
  (~a o))