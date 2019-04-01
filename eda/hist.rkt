#lang racket

(require
  tbl
  tbl/eda/base2         
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
  (define paramsH plot-defaults)
  ;; (printf "p: ~s~n" paramsH)
  ;; (set-plot-limits! T xcol ycol params)
  

  (parameterize ([plot-x-tick-label-anchor  'top-right]
                 [plot-x-tick-label-angle   30])
    (plot (histogram-renderer T factC valC #:params paramsH)
          #:title   (hash-ref paramsH 'title)
          #:x-label factC
          #:y-label "Count"
          
          #:width   (hash-ref paramsH 'width 600)
          #:height  (hash-ref paramsH 'height 400)
          )))

(define (histogram-renderer T factC valC
                            #:compare [comp greater-than]
                            #:order [order '()]
                            #:params [paramsH plot-defaults]) 
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
  (define mx (apply max (get-column T valC)))
  (hash-set! paramsH 'y-max (+ mx (* mx
                                     (hash-ref paramsH 'plot-limit-percent-offset))))

  ;: I only want 10 labels.
  (define bars (map list->vector (sorted)))
  (when (> (length bars) 10)
    (for ([i (range (length bars))])
      #;(printf "i ~a r ~a b ~a~n"
                i (modulo i (quotient (length bars) 10))
                (and (not (zero? i)) (zero? (modulo i (quotient (length bars) 10)))))
      (unless (and (not (zero? i)) (zero? (modulo i (quotient (length bars) 10))))
        (vector-set! (list-ref bars i) 0 "")))
    #;(printf "~a~n" bars))
  
  (discrete-histogram 
                      #:y-max   (hash-ref paramsH 'y-max)
                      bars
                      ))
