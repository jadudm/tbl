#lang racket

(require plot
         tbl/types
         tbl/operations
         tbl/eda/base2
         )


(define scatter-plot
  (match-lambda*
    [(list (? tbl? T)
           (? string? xcol)
           (? string? ycol))
     (scatter-plot T xcol ycol (default-plot-params))]
    
    [(list (? tbl? T)
           (? string? xcol)
           (? string? ycol)
           (? hash? params))
     (set! params (merge-params params (default-plot-params)))
     (define renderer (scatter T xcol ycol params))
     
     (plot renderer
           #:title   (hash-ref params 'title)
           #:x-label (hash-ref params 'x-label)
           #:y-label (hash-ref params 'y-label)
           #:x-min   (hash-ref params 'x-min)
           #:x-max   (hash-ref params 'x-max)
           #:y-min   (hash-ref params 'y-min)
           #:y-max   (hash-ref params 'y-max)
           #:width   (hash-ref params 'width 600)
           #:height  (hash-ref params 'height 400)
     )]))
   
(define scatter
  (match-lambda*
    [(list (? tbl? T)
           (? string? xcol)
           (? string? ycol))
     (scatter T xcol ycol (default-plot-params))]
    [(list (? tbl? T)
           (? string? xcol)
           (? string? ycol)
           (? hash? params))
     ;; Merge the user params into the default params.
     ;; If we dupicate defaults, it's OK.
     (set! params (merge-params (default-plot-params) params))
     
     (define pts
       (map (λ (x y) (vector x y))
            (pull T xcol) (pull T ycol)))

     ;; This looks at the values, sets limits slightly outside
     ;; the values the user has in the table.
     (set-plot-limits! T xcol ycol params)
     
     (points pts
             #:color      (->tbl-color (hash-ref params 'color) #:solid? true)
             #:size       (hash-ref params 'point-size)
             #:fill-color (->tbl-color (hash-ref params 'fill-color)
                                       #:solid? (hash-ref params 'solid?))
             #:sym        (hash-ref params 'sym)
             #:alpha      (hash-ref params 'alpha)
             #:x-min      (hash-ref params 'x-min)
             #:x-max      (hash-ref params 'x-max)
             #:y-min      (hash-ref params 'y-min)
             #:y-max      (hash-ref params 'y-max)
             )
     ]))

#|
(require tbl)
(define T (read-gsheet "https://tinyurl.com/yx8nswkz"))
(scatter-plot T "age" "age"
              (params (x-min 0) (x-max 200) (y-min 0) (y-max 200)))
|#