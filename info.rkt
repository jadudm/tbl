#lang info

(define collection "tbl")

(define scribblings '(("scribblings/tbl.scrbl"
                       (multi-page)
                       ("Data Tables, But Smaller"))))

(define deps '("base"
               "db"
               "data-lib"
               "csv-reading"
               "gregor"
               "rackunit"
               "chk-lib"
               "sql"))

(define pkg-desc "Documentation for \"tbl\"")

(define pkg-authors '(jadudm))