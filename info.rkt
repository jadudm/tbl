#lang info

(define collection "table")

(define scribblings '(("scribblings/table.scrbl"
                       (multi-page)
                       ("Data Tables, Simplified"))))

(define deps '("base"
               "db"
               "data-lib"
               "csv-reading"
               "gregor"
               "rackunit"
               "rackunit-chk"
               "sql"))

(define pkg-desc "Documentation for \"table\"")

(define pkg-authors '(jadudm))