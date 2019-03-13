#lang racket

(provide (all-defined-out))

(struct tbl (name type columns types db)
  #:transparent
  #:mutable)
