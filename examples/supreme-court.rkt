#lang racket

(require tbl)

(define mdbT (read-csv supreme-court-csv))

(row-count mdbT)

