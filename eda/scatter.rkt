#lang racket

(provide (all-defined-out))

(require tbl
         tbl/types
         tbl/operations
         tbl/util/util
         tbl/eda/base
         tbl/eda/types
         plot
         plot/utils
         racket/draw)

;; Expects a tidy-plot struct.
;; This has both params and a thunk.
(define (show tps)
  (define params (eda-params tps))
  (parameterize ([plot-decorations? (hash-ref params 'plot-decorations? true)]
                 [plot-font-family (hash-ref params 'font-family)])
  
   (plot (thaw (eda-thunk tps))
         #:x-label (hash-ref (eda-params tps) 'x-label)
         #:y-label (hash-ref (eda-params tps) 'y-label)
         #:x-min   (hash-ref (eda-params tps) 'x-min)
         #:x-max   (hash-ref (eda-params tps) 'x-max)
         #:y-min   (hash-ref (eda-params tps) 'y-min)
         #:y-max   (hash-ref (eda-params tps) 'y-max)
         #:width   (hash-ref (eda-params tps) 'width 600)
         #:height  (hash-ref (eda-params tps) 'height 400)
         )))

(define (setup-scatterplot-points T series-name)
  (pull T series-name))

(define (setup-monotonic-index T series-name)
  (range (length (pull T series-name))))

(define (guard-tidy-x/y T params)
  (define series-names (map ->string (tbl-columns T)))
  ;; (printf "series-names: ~s~n" series-names)
  
  (unless (hash-ref params 'x-series false)
    (error 'tidy-x/y "You must indicate which #:x-series to plot."))
  (unless (hash-ref params 'y-series false)
    (error 'tidy-x/y "You must indicate which #:y-series to plot."))
  (when (hash-ref params 'color-by false)
    (unless (member (hash-ref params 'color-by)  series-names)
      (error 'tidy-x/y "The column '~a' for #:color-by does not seem to be one of ~s~n"
             (hash-ref params 'color-by)
             series-names)))
  (when (hash-ref params 'symbol-by false)
    (unless (member (hash-ref params 'symbol-by) series-names)
      (error 'tidy-x/y "The column '~a' for #:symbol-by does not seem to be one of ~s~n"
             (hash-ref params 'color-by)
             series-names)))
  )

(define (eda-x/y T x-series y-series
                 #:color            [color           (hash-ref plot-defaults 'color)]
                  #:fill-color       [fill-color      (hash-ref plot-defaults 'fill-color)]

                  #:color-by         [color-by        (hash-ref plot-defaults 'color-by)]
                  #:category-colors  [category-colors (hash-ref plot-defaults 'category-colors)]

                  #:solid?           [solid?          (hash-ref plot-defaults 'solid?)]

                  #:x-label          [x-label         (hash-ref plot-defaults 'x-label)]
                  #:y-label          [y-label         (hash-ref plot-defaults 'y-label)]

                  #:width            [width (hash-ref plot-defaults 'width)]
                  #:height           [height (hash-ref plot-defaults 'height)]
                  )
  
  (define params (override-params x-series y-series color
                                  fill-color color-by category-colors solid?
                                  x-label y-label width height))
  (guard-tidy-x/y T params)
  
  ;; Load the xs, ys lists for plotting.
  ;; Anything we look up should be guarded.
  (hash-set! params 'xs (setup-scatterplot-points T (hash-ref params 'x-series)))
  (hash-set! params 'ys (setup-scatterplot-points T (hash-ref params 'y-series)))
  (when (not (empty? x-series))
    (hash-set! params 'x-label x-series))
  (when (not (empty? y-series))
    (hash-set! params 'y-label y-series))
  (when (not (equal? "Acrossness" x-label))
    (hash-set! params 'x-label x-label))
  (when (not (equal? "Upness" y-label))
    (hash-set! params 'y-label y-label))

  ;; Do we get by-reference or by-value on hash tables?
  ;; Hash tables seem to be by reference. This should work.
  (setup-point-colors T params)
  (set-plot-limits T params)
  (setup-point-symbols T params)
  (set-plot-limits T params)

  ;; Set pretties
  (plot-font-family (hash-ref params 'font-family))
  
  ;; Do the plot.
  (eda params (scatterplot T params)))
(define scatter eda-x/y)
(define x/y eda-x/y)

(define (scatterplot df params)
  (freeze
   (for/list ([x  (hash-ref params 'xs)]
              [y  (hash-ref params 'ys)]
              [lc (hash-ref params 'plc)]
              [fc (hash-ref params 'pfc)]
              [sym (hash-ref params 'ps)]
              )
      
     (points (list (vector x y)
                   (vector x y))
             #:color      (->tidy-color lc #:solid? true)
             #:size       (hash-ref params 'point-size)
             #:fill-color (->tidy-color fc #:solid? (hash-ref params 'solid?))
             #:sym        sym
             #:alpha      (hash-ref params 'alpha)
             #:x-min      (hash-ref params 'x-min)
             #:x-max      (hash-ref params 'x-max)
             #:y-min      (hash-ref params 'y-min)
             #:y-max      (hash-ref params 'y-max)
             
             ))))