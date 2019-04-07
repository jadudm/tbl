#lang racket
(require tbl/basics
         tbl/types
         tbl/util/util
         tbl/operations
         (prefix-in ds: "data-science/main.rkt")
         )

(provide $ group-with aggregate)

(define ($ T ndx)
  ;; gvector
  (define s* (column-names T))
  (cond
    [(or (symbol? ndx) (string? ndx))
     (define the-ndx (index-of s* (~a ndx)))
     (cond
       [(number? the-ndx) ($ T the-ndx)]
       [else
        (error '$ "No column with name '~a'" ndx)]
       )]
    [(number? ndx)
     ;; Make sure we're within bounds
     (when (>= ndx (length s*))
       (error '$ "Index ~a larger than number of columns (~a)"
              ndx (length s*)))
     (define name (list-ref s* ndx))
     ;; Ultimately, we use the base function 'pull.'
     (get-column T name)]
    ))


;; ORIGINAL : http://bit.ly/2Cfg2Qp
(define group-with
  (match-lambda*
    ;; Given a table and two columns
    [(list (? tbl? T)
           (? string-or-symbol? factors-col)
           (? string-or-symbol? values-col))
     (define factors (get-column T (~a factors-col)))
     (define values  (get-column T (~a values-col)))
     (group-with factors values)]
    ;; Given two lists
    [(list (? list? factors) (? list? values))
     (ds:group-with factors values)]
    
    ))

;; ORIGINAL : http://bit.ly/2SYldKb
(define aggregate
  (match-lambda*
    [(list (? tbl? T)
           (? procedure? fun)
           (? string-or-symbol? factors-col)
           (? string-or-symbol? values-col))
     (ds:aggregate fun ($ T factors-col) ($ T values-col))]
    [(list (? procedure? fun)
           (? list? factors)
           (? list? values))
     (ds:aggregate fun factors values)]))