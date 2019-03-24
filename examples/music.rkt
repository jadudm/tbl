#lang racket

(require tbl
         tbl/util/util
         tbl/types
         tbl/reading/csv
         tbl/test/files
         tbl/operations
         tbl/checks
         )

(define mdbT (read-csv music-csv))
(row-count mdbT)

(define scT (read-csv supreme-court-csv))
(row-count scT)
(check-column scT "reserved0")

;; I used a hash so I could see the columns that had missing values.
(define (count-nulls T)
  (define counts (make-hash))
  (for ([col (tbl-columns T)])
    (define values (pull T col))
    (hash-set! counts
               col
               (length (filter true? (map (λ (v) (none? v)) values)))))
  ;; (printf "~a~n" counts)
  (apply + (for/list ([(k v) counts]) v)))
