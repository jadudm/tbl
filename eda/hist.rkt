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

(define (greater-than o1 o2)
  (cond
    [(and (string? o1) (string? o2)) (string<? o1 o2)]
    [(and (number? o1) (number? o2)) (< o1 o2)]
    [else (error 'greater-than "Cannot compare strings and numbers: ~a and ~a" o1 o2)]
    ))

(define (hist T factC valC
              #:compare [comp greater-than]
              #:order [order '()]) 
  ;; Use the aggregate package to build the histogram data.
  ;; The first column gives us the factors, the second the data.
  ;; We will aggregate by sum.
  (define hist-data (aggregate T sum factC valC))
  (define sorted (make-parameter '()))
  (cond
    [(not (empty? order))
     (sorted
       (for/list ([o order])
         (assoc o hist-data)))]
    [else
     (sorted (sort hist-data greater-than #:key first))])
  ;; Sort this.
  ;; (printf "SORTED: ~s~n" (sorted))
  (discrete-histogram (map list->vector (sorted))))
