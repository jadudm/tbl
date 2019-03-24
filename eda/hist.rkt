#lang racket

(require
  tbl
  tbl/operations
  tbl/util/util
  tbl/eda/types
  tbl/eda/base         
  plot
  plot/utils
  racket/draw)

(provide hist)

(define (hist T factC valC)
  ;; Use the aggregate package to build the histogram data.
  ;; The first column gives us the factors, the second the data.
  ;; We will aggregate by sum.
  (define hist-data (aggregate T sum factC valC))
  (discrete-histogram (map list->vector hist-data)))