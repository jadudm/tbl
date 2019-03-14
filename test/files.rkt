#lang racket

(provide (all-defined-out))

(require racket/runtime-path)

;; The guns.csv file is from
;; https://raw.githubusercontent.com/fivethirtyeight/guns-data/master/full_data.csv
;; which is part of the
;; https://github.com/fivethirtyeight/guns-data
;; repository which is part of the "Gun Deaths in America" project.
;; https://fivethirtyeight.com/features/gun-deaths/
(define-runtime-path gun-deaths-csv "guns.csv")

(define-runtime-path supreme-court-csv "supreme_court.csv")

  
  