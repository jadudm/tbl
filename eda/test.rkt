#lang racket

(require table/plot
         table/reading/gsheet)

(define T (read-gsheet "http://bit.ly/2ChP8HY"))
(show (scatter T "LonD" "LatD"))