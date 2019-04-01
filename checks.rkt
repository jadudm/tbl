#lang racket

(require math/statistics
         tbl/basics
         tbl/types
         tbl/util/util
         tbl/operations
         tbl/calculation
         raart
         )

(provide (all-defined-out))

(define (type->pred? type)
  (define (any? v) true)
  (case (->symbol type)
    [(int integer) integer?]
    [(real double float) real?]
    [(numeric number) number?]
    [(text string) string?]
    [(blob bytes) bytes?]
    [(any) any?]
    ))

(define (check-column T col)
  (define type (list-ref (tbl-types T)
                         (index-of (tbl-columns T) col)))
  (andmap (type->pred? type) (pull T col)))

(define (sql-type->racket-type t)
  (case (->symbol t)
    [(text string txt) 'string]
    [(number numeric integer int real double float) 'number]
    [(blob bytes) 'bytes]
    [(any) 'any]
    [else 'unknown]
    ))

(define (λ:not f)
  (λ (v) (not (f v))))

(define (check-tbl T)
  (check-columns-are? T (tbl-types T)))
  
(define (check-columns-are? T lot)
  (define found (make-hash))
  (define errors? true)
  (for ([c (tbl-columns T)]
        [t lot])
    (define V* (pull T c))
    ;;(printf "v: ~a~n" V*)
    ;;(printf "t: ~a~n" t)
    (define outliers (filter (λ:not (type->pred? t)) V*))
    ;;(printf "OUT: ~s~n" outliers)
    (hash-set! found c (length outliers)))
  (for ([(col count) found])
    (when (> count 0)
      (set! errors? false)
      (fprintf (current-error-port)
               "The column '~a' has ~a values that are not ~as.~n"
               col count (sql-type->racket-type
                          (list-ref lot
                                    (index-of (tbl-columns T) col))))
      ))
  ;;(printf "~a~n" found)
  errors?)

(define (count-true ls)
  (apply + (map (λ (o) (if (and (boolean? o) (equal? o true)) 1 0)) ls)))

(define (count-false ls)
  (apply + (map (λ (o) (if (and (boolean? o) (equal? o false)) 1 0)) ls)))

(define (count-if T col pred?)
  (count-true (map pred? (pull T col))))

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
   

;; FIXME
;; This is crap. It does not do what I want it to do.
;; Also, there should be a "display.rkt" for this kind of thing,
;; not bury it under "checks".
(define (show-tbl T #:rows [nrows 10] #:width [width 8] #:cols [ncols 0])
  (define all-rows (get-rows T))
  (when (zero? ncols)
    (set! ncols (length (tbl-columns T))))
  (when (zero? nrows)
    (set! nrows (count-rows T)))
  (define truncated
    (map (λ (row)
           (map (λ (v h)
                  (define conv (->string v))
                  (define the-width (max width (min (string-length h) (string-length conv))))
                  (define the-string (substring conv 0 the-width #;(min the-width (string-length conv))))
                  (when (< (string-length h) (string-length conv))
                    (set! the-string (string-append the-string "..")))
                  the-string)
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

#;(module+ test
  (require rackunit/chk
           tbl
           tbl/types)
  (define (make-donut-table-1)
    (define T (make-tbl "donuts"
                        '(type topping cost)
                        '(text text number)))
    (add-row T (list "plain" "A" 0.75))
    (add-row T (list "cake" "A" 1.00))
    (add-row T (list "chocolate" "glazed" 1.25))
    T)
  (define (make-donut-table-2)
    (define T (make-tbl "donuts"
                        '(type topping cost)
                        '(text text number)))
    (add-row T (list "plain" none 0.75))
    (add-row T (list "cake" none 1.00))
    (add-row T (list "chocolate" "glazed" 1.25))
    T)

  (define T1 (make-donut-table-1))
  (define T2 (make-donut-table-2))
    
  ; (columns-should-be T '(string string number))
  
  (chk
   (row-count T1) 3
   (column-count T1) 3
   ;;(check-column T "type") true
   ;; "A" "B1T" 
   (check-tbl T1) true
   ;; "A" "B2T" 
   (check-columns-are? T1 '(any any any)) true
   ;; "A" "B3F" 
   (check-columns-are? T1 '(blob blob blob)) false
   ;; "A" "B4T" 
   (check-columns-are? T1 '(text text number)) true
   )
  )