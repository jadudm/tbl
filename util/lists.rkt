#lang racket

(provide (all-defined-out))

(require racket/set)

(define (unique ls)
  (set->list (list->set ls)))

(define (sum lon)
  (apply + lon))