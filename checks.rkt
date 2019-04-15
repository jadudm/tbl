#lang racket

(require tbl/basics
         tbl/types
         tbl/util/util
         tbl/operations
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
  (andmap (type->pred? type) (get-column T col)))

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
    (define V* (get-column T c))
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
  (count-true (map pred? (get-column T col))))

(define (check-has-columns? T . cols)
  (for ([col cols])
    (unless (member col (tbl-columns T))
      (error 'check-has-columns? "Table '~a' is missing column '~a'"
             (tbl-name T)
             col)))
  true)

(define (check-row-count? T n)
  (unless (equal? (count-rows T) n)
    (error 'check-row-count "Table has ~a rows, expected ~a" (count-rows T) n))
  true)

(define (check-column-count? T n)
  (unless (equal? (count-columns T) n)
    (error 'check-column-count "Table has ~a columns, expected ~a" (count-columns T) n))
  true)
   


