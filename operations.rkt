#lang racket

(require table/types
         table/util/util
         db sql
         )

(define (pull T column)
  (define Q (select (ScalarExpr:INJECT ,(->string column))
                    #:from (TableRef:INJECT ,(table-name T))))
  (query-list (table-db T) Q))
  
(module+ test
  (require rackunit/chk
           table/reading/gsheet)
  (define T (read-gsheet "https://pult.us/u/flavors"))

  (chk
   (pull T 'age)  '(42 9 5)
   (pull T "age") '(42 9 5)
   )
)