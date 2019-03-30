#lang racket

(require tbl/types
         tbl/util/util
         tbl/operations)
(provide save-csv)

(define (->save-string s)
  (cond
    [(na? s) ""]
    [else (->string s)]))

(define (save-csv T filename #:replace [ow false])
  (define exists 'error)
  (when ow (set! exists 'replace))
  (define of (open-output-file filename #:exists exists))
  (fprintf of "~a~n" (string-join (map ->string (tbl-columns T)) ", "))
  (for ([row (get-rows T)])
    (fprintf of "~a~n" (string-join (map ->save-string row) ", ")))
  (close-output-port of))
