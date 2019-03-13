#lang racket

(require table/plot
         table/reading/gsheet
         table/reading/csv
         table/test/files)

(define T (read-gsheet "http://bit.ly/2ChP8HY"))
(define gunsT (read-csv gun-deaths-csv))

(show (scatter T "LonD" "LatD"))
(show (scatter gunsT "year" "age"))
