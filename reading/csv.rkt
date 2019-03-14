#lang racket

(require tbl
         csv-reading
         tbl/util/csv
         net/url
         )

(provide read-csv)


(define spec
  '((separator-chars            #\,)
    (strip-leading-whitespace?  . #t)
    (strip-trailing-whitespace? . #t)))

(define (read-csv csv
                  #:header-row? [header-row? true]
                  #:name [name "csv"])
  (cond
    ;; If we get a port
    [(port? csv)
     
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
     
     (define headers (cleanup-column-names (first rows)))
     (define T (make-tbl name headers types))
     (for ([row (rest rows)])
       (add-row! T row))
     T]

    ;; If they give us a file or a path
    [(and (or (string? csv) (path? csv))
          (file-exists? csv))
     (read-csv (open-input-file csv)
               #:header-row? header-row?
               #:name name)]

    ;; If they give us a URL
    [(and (string? csv)
          (regexp-match #px"^http" csv))

     (define lines
       (port->lines (get-impure-port (string->url csv))))

     (cond
       [(check-for-redirect lines)
        (define new-url (get-new-url lines))
        (read-csv new-url #:header-row? header-row? #:name name)
        ]
       [else
        (read-csv (get-pure-port (string->url csv))
                  #:header-row? header-row? #:name name)])
     ]))
