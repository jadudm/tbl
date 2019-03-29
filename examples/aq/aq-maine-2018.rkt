#lang racket

(require tbl
         tbl/util/util
         racket/runtime-path)

;; https://www.epa.gov/outdoor-air-quality-data/download-daily-data
;; CC0 (Public Domain)
(define-runtime-path mdaq-csv "maine-daily-aq-2018.csv")
(define aq-maine-2018-T (read-csv mdaq-csv))
(set! aq-maine-2018-T
      (remove-columns aq-maine-2018-T
                      "CBSA_CODE"
                      "CBSA_NAME"))
(provide aq-maine-2018-T)

