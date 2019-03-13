#lang racket
(require table
         net/url
         csv-reading)

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

(define (read-gsheet url
                     #:header-row? [header-row? true])
  (define lines
    (port->lines (get-impure-port (string->url url))))

  (cond
    [(check-for-redirect lines)
     (define new-url (get-new-url lines))
     (read-gsheet new-url #:header-row? header-row?)
     ]
    [else
     (read-csv (get-pure-port (string->url url))
               #:header-row? header-row?
               #:name "gsheet")]
    ))

(define maybe
  (lambda (pred?)
    (lambda (o)
      (and (string? o)
           (string->number o)
           (pred? (string->number o))))))

(define (mostly? pred? ls)
  (define count (apply + (map (λ (o) (if (pred? o) 1 0)) ls)))
  (>= (/ count (length ls)) (/ 6 10)))
    
(define spec
  '((separator-chars            #\,)
       (strip-leading-whitespace?  . #t)
       (strip-trailing-whitespace? . #t)))

(define (read-csv csv
                  #:header-row? [header-row? true]
                  #:name [name "coffee"])
  (define csv-reader
    (make-csv-reader-maker spec))
  (define rows
    (csv->list (make-csv-reader csv spec)))
  ;; Guess the types
  (define types
    (for/list ([ndx (range (length (first rows)))])
      (define vals (map (λ (row) (list-ref row ndx))
                        (rest rows)))
      (cond
        [(mostly? (maybe integer?) vals) 'integer]
        [(mostly? (maybe number?) vals) 'real]
        [(mostly? string? vals) 'text]
        [else 'blob])))
  
  (define T (create-table name (first rows) types))
  (for ([row (rest rows)])
    (add-row! T row))
  T
  )

(module+ test
  (require table/types
           rackunit/chk
           db sql)
  
  (define T (read-gsheet "https://tinyurl.com/yx8nswkz"))
  (query-rows (table-db T) (select (.*) #:from (TableRef:INJECT ,(table-name T))))
  )
