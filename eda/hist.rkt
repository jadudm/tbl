#lang racket

(require
  tbl
  tbl/eda/base         
  plot
  )

(provide histogram histogram-renderer)

(define (greater-than o1 o2)
  (cond
    [(and (string? o1) (string? o2)) (string<? o1 o2)]
    [(and (number? o1) (number? o2)) (< o1 o2)]
    [else (error 'greater-than "Cannot compare strings and numbers: ~a and ~a" o1 o2)]
    ))

(define (histogram T factC valC)
  (define params plot-defaults)
  (printf "p: ~s~n" params)
  
  (plot (histogram-renderer T factC valC)
        #:title   (hash-ref params 'title)
        #:x-label factC
        #:y-label "Count"
        #:x-min   (hash-ref params 'x-min)
        #:x-max   (hash-ref params 'x-max)
        #:y-min   (hash-ref params 'y-min)
        #:y-max   (hash-ref params 'y-max)
        #:width   (hash-ref params 'width 600)
        #:height  (hash-ref params 'height 400)
        ))

(define (histogram-renderer T factC valC
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
