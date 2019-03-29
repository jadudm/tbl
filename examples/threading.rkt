#lang racket

(require tbl tbl/plot
         threading)

#|
(define csv "http://bit.ly/wp-gun-deaths-csv")
(define T (read-csv csv))
(define dT (filter-rows T (> killed 0)))
(aggregate dT sum "day_of_week" "killed")
(plot (hist dT "day_of_week" "killed"))
|#

(~> "http://bit.ly/wp-gun-deaths-csv"
    read-csv
    (filter-rows (> killed 0))
    (hist "day_of_week" "killed")
    plot)

(define (one-of? w low)
  (ormap (λ (o) (string-contains? w o)) low))

(define (weapon->type weapon)
  (cond
    [(not-available? weapon) "unknown"]
    [(one-of? weapon '("XM-15" "XM15" "AR-15" "AK-47"))
     "semiautomatic rifle"]
    [(string-contains? weapon "shotgun")
     "shotgun"]
    [(string-contains? weapon "rifle")
     "rifle"]
    [(one-of? weapon '("handgun" "revolver" "handgin" "gun" "officer"))
     "handgun"]
    [else
     (error 'weapon->kind "Unknown weapon: ~s" weapon)]))

;; Does the threading macro impose a massive overhead?
;; (compute ...) is crazy slow when threaded.
;; It runs very quickly when not threaded.
(~> "http://bit.ly/wp-gun-deaths-csv"
    read-csv
    (filter-rows (> killed 0))
    (add-column "weapon_type" Text)
    (compute "weapon_type"
             (function (weapon) (weapon->type weapon)))
    (hist "weapon_type" "killed")
    plot)

