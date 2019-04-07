#lang racket

(require tbl/types
         tbl/operations
         tbl/basics
         tbl/util/util
         raart
         math/statistics
         tbl/calculation)

(provide (all-defined-out))

;; FIXME
;; This is crap. It does not do what I want it to do.
;; Also, there should be a "display.rkt" for this kind of thing,
;; not bury it under "checks".
(define (show-tbl T #:rows [nrows 10] #:width [width 8] #:columns [ncols 0])
  (define all-rows (get-rows T))
  (when (zero? ncols)
    (set! ncols (length (tbl-columns T))))
  (when (zero? nrows)
    (set! nrows (count-rows T)))
  (define truncated
    (map (λ (row)
           (map (λ (v h)
                  (define conv (->string v))
                  ;; Now, I want to either have the full length of the string, or
                  ;; I want to limit to the length of the width variable.
                  (define the-contents
                    (cond
                      ;; If the width is greater than the conv string, use the full
                      ;; conv string.
                      [(> width (string-length conv))
                       conv]
                      ;; If the width is less, then truncate the conv string
                      [(<= width (string-length conv))
                       (string-append conv " ...")]))
                  the-contents)
                (take row ncols)
                (take (tbl-columns T) ncols)))
         all-rows))
  (draw-here (table (text-rows
                     (append
                      (list
                       (take (tbl-columns T) ncols)
                       (take (tbl-types T) ncols))
                      (take truncated
                            (min nrows (length truncated)))))))
  (when (> (count-rows T) nrows)
    (printf "(only ~a of ~a rows shown)" nrows (count-rows T)))
  )

(define summary
  (match-lambda*  
  ;;(printf "Table: ~a~n" (tbl-name T))
  [(list (? tbl? T))
   (for ([col (tbl-columns T)]
         [type (tbl-types T)])
     (define the-column (get-column T col))
     (summarize-column col type the-column)
     )]
  [(list (? tbl? T) (? string? col))
   (define ndx (index-of (tbl-columns T) col))
   (summarize-column
    (list-ref (tbl-columns T) ndx)
    (list-ref (tbl-types T) ndx)
    (get-column T col))]
  ))

(define (->display s)
  (cond
    [(na? s) "N/A"]
    [else s]))

;; FIXME
;; Tables need metadata.
(define (summarize-column name type the-column)
  (cond
    [(empty? the-column)
     (printf "The column [ ~a ] is empty.~n" name)
     (newline)]
    [else
     (match (->string type)
      ["text"
       (newline)
       (printf "Representative strings in column [ ~a ]~n" name)
       (printf "\t- ~s~n" (->display (list-ref the-column (random (length the-column)))))
       (printf "\t- ~s~n" (->display (list-ref the-column (random (length the-column)))))
       (printf "\t- ~s~n" (->display (list-ref the-column (random (length the-column)))))
       (newline)
       ]
      [(or "integer" "real")
       (newline)
       (printf "Summary for integer column [ ~a ]~n" name)
       (define the-table
         `((Minimum ,(apply min the-column))
           (Maximum ,(apply max the-column))
           (Mode ,(mode the-column))
           (Mean ,(real->decimal-string (exact->inexact (mean the-column)) 5))
           (Variance ,(real->decimal-string (exact->inexact (variance the-column)) 5))
           ("Standard Deviation" ,(real->decimal-string (stddev the-column) 5))))
       (draw-here (table (text-rows the-table)))
       (newline)
       ]
      ['blob
       (newline)
       (printf "Column: ~a~nType: ~a~n" name type)]
      [else
       (printf "Something: ~s ~s~n" name type)])]
    ))

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