#lang racket

(require tbl
         tbl/plot
         ;tbl/reading/gsheet
         ;tbl/reading/csv
         ;tbl/test/files
         )

;; Cities
(define T (read-gsheet "http://bit.ly/2ChP8HY"))
;; Gun deaths
; (define gunsT (read-csv gun-deaths-csv))

;; Two ways to clean this large table.
#;(define cleanGT1
    (filter-rows
     (filter-rows gunsT (not (= "NA" year)))
     (not (= "NA" age))))

;; (define cleanGT2 (filter-rows gunsT (not (or (= "NA" year) (= "NA" age)))))
   

(show (scatter T "LonD" "LatD"))
;; (show (scatter cleanGT1 "year" "age"))
;; (show (scatter cleanGT2 "year" "age"))

(require tbl)
(define csv "http://bit.ly/wp-gun-deaths-csv")
(define gT (read-csv csv))
(check-table gT)

(define (weapon->type weapon)
  (cond
    [(not-available? weapon) "unknown"]
    [(ormap (λ (kind)
              (string-contains? weapon kind))
            '("XM-15" "XM15" "AR-15")) "semiauto"]
    [(string-contains? weapon "AK-47") "semiauto"]
    [(string-contains? weapon "shotgun") "shotgun"]
    [(string-contains? weapon "rifle") "rifle"]
    [(ormap (λ (choice)
              (string-contains? weapon choice))
            '("handgun" "revolver" "handgin" "gun" "officer")) "handgun"]
  
    [else
     not-available?(error 'weapon->kind "Unknown weapon: ~s" weapon)]))

(add-column gT "weapon_type" Text)
(compute gT "weapon_type" (function (weapon) (weapon->type weapon)))
(plot (hist gT "weapon_type" "killed")
      #:title "Weapons Used in School Schootings"
      #:x-label "Weapon Used"
      #:y-label "Deaths in schools since 1999")
