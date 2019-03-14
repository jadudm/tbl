#lang racket

(require tbl
         tbl/operations
         plot/utils)

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

(define (default-plot-params override)
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
  (hash-set! h 'font-family 'default)

  (hash-set! h 'width 500)
  (hash-set! h 'height 500)
  
  (for ([(k v) override])
    (hash-set! h k v))
  h)
(define plot-defaults (default-plot-params (make-hash)))


(define-syntax (override-params stx)
  (syntax-case stx ()
    [(_ cols ...)
     #'(let ([tmp (make-hash)])
         (for ([k (list (quote cols) ...)]
               [v (list cols ...)])
           ;;(printf "Setting: ~a ~a~n" k v)
           (hash-set! tmp k v))
         (default-plot-params tmp))]))


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

(define (set-plot-limits df params)
  (hash-set! params 'x-min (limit (hash-ref params 'xs) - (hash-ref params 'plot-limit-percent-offset)))
  (hash-set! params 'x-max (limit (hash-ref params 'xs) + (hash-ref params 'plot-limit-percent-offset)))
  (hash-set! params 'y-min (limit (hash-ref params 'ys) - (hash-ref params 'plot-limit-percent-offset)))
  (hash-set! params 'y-max (limit (hash-ref params 'ys) + (hash-ref params 'plot-limit-percent-offset)))
  params
  )

(define make-unique-labeler
  (λ (colors)
    (define unique-label-hash (make-parameter (make-hash)))
    (define unique-label-counter (make-parameter 0))
    (define (lookup label)
      (cond
        [(hash-ref (unique-label-hash) label false)
         (hash-ref (unique-label-hash) label)
         ]
        [else
         (define next (unique-label-counter))
         (hash-set! (unique-label-hash) label (list-ref colors (modulo next (length colors))))
         (unique-label-counter (add1 next))
         (lookup label)]
        ))
    lookup))

(define (setup-point-symbols T params)
  ;; Set up the color labels
  ;; If color-by, they have indicated a column to color things by.
  (define color-by (hash-ref params 'symbol-by false))
  ;; Setup colors for each point.
  (define color-labeler (make-unique-labeler (hash-ref params 'category-symbols)))
  (cond
    [color-by
     (hash-set! params
                'ps
                (for/list ([lab (pull T color-by)])
                  (color-labeler lab)))]
    [else
     (hash-set! params 'ps (for/list ([n (length (hash-ref params 'xs))]) (hash-ref params 'default-symbol)))
     ])
  
  params)


(define (setup-point-colors T params)
  ;; Set up the color labels
  ;; If color-by, they have indicated a column to color things by.
  (define color-by (hash-ref params 'color-by false))
  ;; Setup colors for each point.
  (define color-labeler (make-unique-labeler (hash-ref params 'category-colors)))
  (cond
    [color-by
     (hash-set! params
                'plc
                (for/list ([lab (pull T color-by)])
                  (color-labeler lab)))
     (hash-set! params 'pfc
                (for/list ([lab (pull T color-by)])
                  (color-labeler lab)))]
    [else
     (hash-set! params 'plc
                (for/list ([n (length (hash-ref params 'xs))]) (hash-ref params 'color)))
     (hash-set! params 'pfc
                (for/list ([n (length (hash-ref params 'xs))]) (hash-ref params 'fill-color)))])
  params)

(define-syntax-rule (freeze bodies ...)
  (λ ()
    bodies ...))


(define-syntax-rule (thaw body)
  (body))

(define (->tidy-color c #:solid? [solid? true])
    (cond
      [solid? (->pen-color c)]
      [else   (->brush-color c)]))