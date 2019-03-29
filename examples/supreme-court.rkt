#lang racket

(require tbl
         racket/runtime-path)


;; https://www.epa.gov/outdoor-air-quality-data/download-daily-data
;; CC0 (Public Domain)
(define-runtime-path mdaq-csv "maine-daily-aq-2018.csv")
(define mdaqT (read-csv mdaq-csv))
(provide mdaqT)