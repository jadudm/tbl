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

;; From the WP
(define-runtime-path school-shootings "school-shootings-data.csv")


;; CORGIS
#|
The CORGIS Datasets Project seeks to make highly-motivating introductory computing experiences through simple, easy-to-pick-up datasets for beginners. We offer a wide range of libraries for many different programming languages and contexts.
|#
;; https://think.cs.vt.edu/corgis/csv/supreme_court/supreme_court.html
(define-runtime-path supreme-court-csv "supreme_court.csv")

;; https://think.cs.vt.edu/corgis/csv/music/music.html
(define-runtime-path music-csv "music.csv")

  
  