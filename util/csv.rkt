#lang racket

(provide (all-defined-out))


(define (check-for-redirect los)
  (cond
    [(empty? los) false]
    [(regexp-match "301 Moved" (first los)) true]
    [else
     (check-for-redirect (rest los))]))

(define (get-new-url los)
  (cond
    ;; FIXME
    ;; This is a bad condition to get to. An error, or something, should happen.
    ;; This is just a quiet death on bad data.
    [(empty? los) ""]
    [(regexp-match "Location: " (first los))
     (second (regexp-match "Location: (.*)" (first los)))]
    [else
     (get-new-url (rest los))]))

(define maybe
  (lambda (pred?)
    (lambda (o)
      (and (string? o)
           (string->number o)
           (pred? (string->number o))))))

(define (mostly? pred? ls)
  (define count (apply + (map (λ (o) (if (pred? o) 1 0)) ls)))
  (>= (/ count (length ls)) (/ 6 10)))


(define (cleanup-column-names lonames)
  (for/list ([name lonames]
             [ndx (range (length lonames))])
    ;; (printf "Looking at ~s~n" name)
    (cond
      ;; If we only have valid characters
      [(regexp-match #px"^[\\w]+$" name)
       name]
      ;; If we have bad names, clean them up.
      [(regexp-match #px"[\\W]" name)
       (regexp-replace* #px"[\\W]" name "")]
      ;; Otherwise, make up a name
      [else
       (format "col~a" ndx)])))
