#lang racket

(require plot
         tbl/types
         tbl/operations
         tbl/eda/base2
         )

(provide scatterplot scatter-renderer)

(define scatterplot
  (match-lambda*
    [(list (? tbl? T)
           (? string? xcol)
           (? string? ycol))
     (scatterplot T xcol ycol plot-defaults)]
    
    [(list (? tbl? T)
           (? string? xcol)
           (? string? ycol)
           (? hash? params))
     
     (set! params (merge-params plot-defaults params))
     (set-plot-limits! T xcol ycol params)
     
     (define renderer (scatter-renderer T xcol ycol params))
     
     
     (plot renderer
           #:title   (hash-ref params 'title)
           #:x-label xcol
           #:y-label ycol
           #:x-min   (hash-ref params 'x-min)
           #:x-max   (hash-ref params 'x-max)
           #:y-min   (hash-ref params 'y-min)
           #:y-max   (hash-ref params 'y-max)
           #:width   (hash-ref params 'width 600)
           #:height  (hash-ref params 'height 400)
     )]))
   
(define scatter-renderer
  (match-lambda*
    [(list (? tbl? T)
           (? string? xcol)
           (? string? ycol))
     (scatter-renderer T xcol ycol plot-defaults)]
    [(list (? tbl? T)
           (? string? xcol)
           (? string? ycol)
           (? hash? params))
     ;; Merge the user params into the default params.
     ;; If we dupicate defaults, it's OK.
     (set! params (merge-params plot-defaults params))
     
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
              ;;(params (x-min 0) (x-max 200) (y-min 0) (y-max 200))
              )
|#