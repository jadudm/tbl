#lang racket

(require tbl/basics
         tbl/types
         tbl/util/util
         tbl/operations
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

(define (summary T)
  (printf "Table: ~a~n" (tbl-name T))
  (for ([col (tbl-columns T)]
        [type (tbl-types T)])
    
    (match type
      ['text
       (printf "Column: ~a~nType: ~a~n" col type)]
      ['integer
       (printf "Column: ~a~nType: ~a~n" col type)]
      ['real
       (printf "Column: ~a~nType: ~a~n" col type)]
      ['blob
       (printf "Column: ~a~nType: ~a~n" col type)]
      [else
       (printf "Something...~n")])))

(define (show-table T #:rows [nrows 10] #:width [width 8])
  (define all-rows (get-rows T))
  (define truncated
    (map (λ (row)
           (map (λ (v h)
                  (define conv (->string v))
                  (define the-width (max width (min (string-length h) (string-length conv))))
                  (define the-string (substring conv 0 (min the-width (string-length conv))))
                  (when (< (string-length h) (string-length conv))
                    (set! the-string (string-append the-string "..")))
                  the-string)
                row (tbl-columns T)))
         all-rows))
  (draw-here (table (text-rows
                     (append
                      (list
                       (tbl-columns T)
                       (tbl-types T))
                      (take truncated (min nrows (length truncated)))))))
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
    (add-row! T (list "plain" "A" 0.75))
    (add-row! T (list "cake" "A" 1.00))
    (add-row! T (list "chocolate" "glazed" 1.25))
    T)
  (define (make-donut-table-2)
    (define T (make-tbl "donuts"
                        '(type topping cost)
                        '(text text number)))
    (add-row! T (list "plain" none 0.75))
    (add-row! T (list "cake" none 1.00))
    (add-row! T (list "chocolate" "glazed" 1.25))
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