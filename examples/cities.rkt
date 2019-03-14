#lang racket

(require tbl/reading/gsheet
         tbl/plot)

;; The source Google Sheet: http://bit.ly/cities-gsheet
(define T (read-gsheet "http://bit.ly/cities-csv"))
(show (scatter T "LonD" "LatD"))
