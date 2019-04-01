#lang racket

(require tbl
         tbl/plot)

;; The source Google Sheet: http://bit.ly/cities-gsheet
(define T (read-gsheet "http://bit.ly/cities-csv"))
(scatterplot T "LonD" "LatD")
