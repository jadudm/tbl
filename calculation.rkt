#lang racket
(provide (all-defined-out))

#;(define (median lon)
  (list-ref (sort lon <) (exact-ceiling (/ (length lon) 2))))

(define (mode lon)
  (define h (make-hash))
  (for ([n lon])
    (hash-set! h n (add1 (hash-ref h n 0))))
  (define greatest-v -Inf.0)
  (define greatest-k -Inf.0)
  (for ([(k v) h])
    (when (> v greatest-v)
      (set! greatest-k k)
      (set! greatest-v v)))
  greatest-k)