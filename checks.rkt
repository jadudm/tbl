#lang racket

(require tbl/types
         tbl/util/util
         tbl/operations)

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
    [(number) 'number]
    [(blob bytes) 'bytes]
    [(any) 'any]))

(define (λ:not f)
  (λ (v) (not (f v))))

(define (check-table T)
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

(module+ test
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
   (check-table T1) true
   ;; "A" "B2T" 
   (check-columns-are? T1 '(any any any)) true
   ;; "A" "B3F" 
   (check-columns-are? T1 '(blob blob blob)) false
   ;; "A" "B4T" 
   (check-columns-are? T1 '(text text number)) true
   )
  )