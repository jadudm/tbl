#lang info

(define collection "tbl")

;; 20191207 FIXME
;; The documentation needs a sync/cleanup
#;(define scribblings '(("scribblings/tbl.scrbl"
                         (multi-page)
                         ("Data Tables, an Exploration"))))

(define deps '("base"
               "db"
               "data-lib"
               "csv-reading"
               "gregor"
               "rackunit"
               "chk-lib"
               "sql"
               "threading"
               "ppict"
               "raart"
               "draw-lib"
               "math-lib"
               "pict-lib"
               "plot-gui-lib"
               "plot-lib"
               "scribble-lib"
               "slideshow-lib"
               "at-exp-lib"))

(define pkg-desc "Documentation for \"tbl\"")

(define pkg-authors '(jadudm))