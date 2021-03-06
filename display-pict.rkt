#lang racket

(require tbl/types
         tbl/operations
         tbl/basics
         tbl/util/util
         raart
         math/statistics
         tbl/calculation)

(provide (all-defined-out))

(require racket/draw (prefix-in pct: pict))

(define (show-full-tbl-pict T)
  (show-tbl-pict T #:rows 0 #:columns 0))

(define (show-tbl-pict T #:rows [nrows 10] #:width [width 8] #:columns [ncols 0])
  (define all-rows (get-rows T))
  (when (zero? ncols)
    (set! ncols (length (tbl-columns T))))
  (when (zero? nrows)
    (set! nrows (count-rows T)))

  (define the-rows (append (list (tbl-columns T)
                                 (tbl-types T))
                           (take all-rows nrows)))
  (define column-count ncols)
  (define truncated
    (map (λ (row)
           (if (zero? ncols)
               row
               (take row (min (length row) ncols)))) the-rows))
  
  (define picts '())
  
  (for (([row index] (in-indexed truncated)))
    (define font
      (cond
        [(= index 0) ;; header
         (send the-font-list find-or-create-font 12 'default 'normal 'normal)]
        [(= index 1)
         (send the-font-list find-or-create-font 8 'default 'normal 'normal)]
        [else
         (send the-font-list find-or-create-font 10 'default 'normal 'normal)]))
    (define color
      (if (or (= index 0) (= index 1))
          (make-object color% #x77 #x88 #x99)
          (make-object color% #x2f #x4f #x4f)))
    (define face (cons color font))
    (for ([item (in-list row)])
      (set! picts (cons (pct:text (~a item) face) picts))))
  (let ((p0 (pct:table column-count (reverse picts) pct:lc-superimpose pct:cc-superimpose 15 3)))
    (pct:cc-superimpose
     (pct:filled-rounded-rectangle (+ (pct:pict-width p0) 20) (+ (pct:pict-height p0) 20) -0.1
                               #:draw-border? #f
                               #:color "LightYellow")
     p0)))