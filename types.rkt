#lang racket

(provide (all-defined-out))

(struct table (name type columns types db)
  #:transparent
  #:mutable)
