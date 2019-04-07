#lang racket

(require tbl/reading/csv
         tbl/operations
         tbl/basics
         tbl/types
         racket/runtime-path)

;; The Titanic dataset comes from Dr. John Rasp
;; https://www2.stetson.edu/~jrasp/data.htm
;; It is of unknown provenance and license.
(provide titanicT
         boolean-titanicT
         tiny-titanicT)

(define-runtime-path titanic-csv "titanic.csv")

(define titanicT  (read-csv titanic-csv))
(define boolean-titanicT (read-csv titanic-csv))
(define tiny-titanicT (make-tbl "titanic"
                                '(class age sex survive)
                                '(text text text integer)))

;; The table comes in as nothing but numeric values.
;; These functions convert that data into strings.
(define (class->text c)
  (define lookup
    #hash((0 . "crew")
          (1 . "first")
          (2 . "second")
          (3 . "third")))
  (hash-ref lookup c not-available))

(define (age->text a)
  (define lookup
    #hash((0 . "child")
          (1 . "adult")))
  (hash-ref lookup a not-available))

(define (sex->text s)
  (define lookup
    #hash((0 . "F")
          (1 . "M")))
  (hash-ref lookup s not-available))

;; Recompute each of the columns in the table
;; so that the categorical data is based on strings rather
;; than on integers. Yes, in practice, it might be that we
;; convert categories/factors to numbers, but we're going to
;; start novices with things they can easily read.
(quietly
 (compute titanicT "class" (function (class) (class->text class)))
 (compute titanicT "age"   (function (age)   (age->text age)))
 (compute titanicT "sex"   (function (sex)   (sex->text sex)))
 )

;; This generates a small table. Instead of having every subject
;; from the Titanic be one row in the table, I've made each
;; category a row. So, a crew member, who is male, and an adult, will
;; have a single row, and we can count the number of survivors.

(quietly
 (add-column tiny-titanicT "total" Integer)
 (for ([class '("crew" "first" "second" "third")])
   (for ([sex '("M" "F")])
     (for ([age '("adult" "child")])
       (define total-count
         (count-rows (filter-rows titanicT
                                         (and (= class ,class)
                                              (= age   ,age)
                                              (= sex   ,sex)
                                              ))))
       (define survive-count
         (count-rows (filter-rows titanicT
                                         (and (= class ,class)
                                              (= age   ,age)
                                              (= sex   ,sex)
                                              (= survive 1)))))
       (when (> total-count 0)
         (add-row tiny-titanicT class age sex survive-count total-count))
       ))))
