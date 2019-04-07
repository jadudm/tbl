#lang racket

(require plot/utils
         tbl/operations)

(provide (all-defined-out))

(define possible-symbols
  (list 'fullcircle       'fullsquare
        
        'plus              'times            'asterisk
        'fulldiamond       'fulltriangle     'triangleup
        'triangledown      'triangleleft     'triangleright
        'fulltriangleup    'fulltriangledown 'fulltriangleleft
        'fulltriangleright
        
        '5asterisk         'odot             'oplus
        'otimes            'oasterisk        'o5asterisk
        'circle            'square           'diamond
        'triangle          
         'rightarrow       'leftarrow
        'uparrow           'downarrow        '4star
        '5star             '6star            '7star
        '8star             'full4star        'full5star
        'full6star         'full7star        'full8star
        'circle1           'circle2          'circle3
        'circle4           'circle5          'circle6
        'circle7           'circle8          'bullet
        'fullcircle1       'fullcircle2      'fullcircle3
        'fullcircle4       'fullcircle5      'fullcircle6
        'fullcircle7       'fullcircle8
        'dot               'point            'pixel))

(define (default-plot-params)
  (define h (make-hash))
  (hash-set! h 'color 1)
  (hash-set! h 'fill-color 0)
  
  (hash-set! h 'color-by false)
  (hash-set! h 'category-colors (range 127))
  
  (hash-set! h 'point-size 5)
  (hash-set! h 'solid? true)
  (hash-set! h 'sym 'fullcircle)

  (hash-set! h 'symbol-by false)
  (hash-set! h 'category-symbols possible-symbols)
  (hash-set! h 'default-symbol 'fullcircle)
  
  (hash-set! h 'alpha 0.7)
  (hash-set! h 'plot-limit-percent-offset 0.1)
  (hash-set! h 'x-label "Acrossness")
  (hash-set! h 'y-label "Upness")

  (hash-set! h 'title "Plotted Data")
  
  (hash-set! h 'font-family 'default)

  (hash-set! h 'width 400)
  (hash-set! h 'height 400)
 
  h)

(define plot-defaults (default-plot-params))
(define (reset-plot-defaults)
  (set! plot-defaults (default-plot-params)))
(define (set-plot-parameter k v)
  (hash-set! plot-defaults k v))

(define (limit ls extend percent)
  (define lo (apply min ls))
  (define hi (apply max ls))
  (define rng (- hi lo))
  ;; (printf "~a ~a ~a~n" lo hi rng)
  (cond
    [(equal? extend -)
     (extend lo (* rng percent))]
    [else
     (extend hi (* rng percent))]))

(define (set-plot-limits! T xcol ycol params)
  (hash-set! params 'x-min (limit (get-column T xcol) - (hash-ref params 'plot-limit-percent-offset)))
  (hash-set! params 'x-max (limit (get-column T xcol) + (hash-ref params 'plot-limit-percent-offset)))
  (hash-set! params 'y-min (limit (get-column T ycol) - (hash-ref params 'plot-limit-percent-offset)))
  (hash-set! params 'y-max (limit (get-column T ycol) + (hash-ref params 'plot-limit-percent-offset)))
  params
  )

(define (merge-params h1 h2)
  (define newH (make-hash))
  (for ([(k v) h1])
    (hash-set! newH k v))
  (for ([(k v) h2])
    (hash-set! newH k v))
  newH)

(define-syntax (params stx)
  (syntax-case stx ()
    [(_p (key val) ...)
     #`(let ([h (make-hash)])
         (begin (hash-set! h (quote key) val) ...)
         h)]))

(define (->tbl-color c #:solid? [solid? true])
    (cond
      [solid? (->pen-color c)]
      [else   (->brush-color c)]))