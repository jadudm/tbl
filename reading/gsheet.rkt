#lang racket
(require tbl/reading/csv
         tbl/util/csv
         net/url
         )

(provide (all-defined-out))

(define (read-gsheet url
                     #:header-row? [header-row? true]
                     #:name [name "gsheet"])
  (define lines
    (port->lines (get-impure-port (string->url url))))

  (cond
    [(check-for-redirect lines)
     (define new-url (get-new-url lines))
     (read-gsheet new-url #:header-row? header-row? #:name name)
     ]
    [else
     (read-csv url
               #:header-row? header-row?
               #:name name)]
    ))    

(module+ test
  (require tbl/types
           rackunit/chk
           db sql)
  ;; FIXME
  ;; Most of this gets tested in the operations.
  ;; That is, the tbls are exercized by the operations on them.
  (define T (read-gsheet "https://tinyurl.com/yx8nswkz"))
  (query-rows (tbl-db T) (select (.*) #:from (TableRef:INJECT ,(tbl-name T))))
  )
