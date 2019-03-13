#lang racket

(provide (all-defined-out))

(struct table (name type fields types db)
  #:transparent
  #:mutable)
