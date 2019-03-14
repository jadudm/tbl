#lang racket

(require tbl
         tbl/reading/csv
         tbl/test/files
         tbl/operations)

(define mdbT (read-csv supreme-court-csv))

(row-count mdbT)

