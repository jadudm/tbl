#lang racket

(require tbl
         tbl/plot
         ;tbl/reading/gsheet
         ;tbl/reading/csv
         ;tbl/test/files
         )

;; Cities
(define T (read-gsheet "http://bit.ly/2ChP8HY"))

(scatterplot T "LonD" "LatD")

(define csv "http://bit.ly/wp-gun-deaths-csv")
(define gT (read-csv csv))
(check-tbl gT)

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
     (error 'weapon->kind "Unknown weapon: ~s" weapon)]))

(quietly
 (add-column gT "weapon_type" Text)
 (compute gT "weapon_type" (function (weapon) (weapon->type weapon))))

(plot (histogram-renderer gT "weapon_type" "killed")
      #:title "Weapons Used in School Schootings"
      #:x-label "Weapon Used"
      #:y-label "Deaths in schools since 1999")
