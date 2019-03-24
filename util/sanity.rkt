#lang racket

(require tbl/util/util
         )

(provide (all-defined-out))


(define counter 0)
(define (clean-column-name s)
  ;; (printf "Checking '~a'~n" s)
  
  (define cleaned (->string s))
  
  ;; Make sure it is not a keyword.
  (when (member cleaned (sqlite-keywords))
    (set! cleaned (format "reserved~a" counter))
    (set! counter (add1 counter))
    (fprintf (current-error-port)
             (format "'~a' is a column name that cannot be used.~n"
                     (~a s)))
    )
  
  ;; Get rid of non-word characters.
  (set! cleaned (regexp-replace* #px"[\\W]+" cleaned ""))
  ;; Eliminate leading non-letters
  (set! cleaned (regexp-replace* #px"^[0-9]+" cleaned ""))
  ;; Eliminate spaces
  (set! cleaned (regexp-replace* #px"[[:space:]]+" cleaned "_"))
  
  (when (not (equal? (->string s) cleaned))
    (fprintf (current-error-port)
             (format "The column '~a' must become '~a'.~n" s cleaned)))
  cleaned)

(module+ test
  (require rackunit/chk)

  (chk
   (clean-column-name "3judgedc") "judgedc"
   (clean-column-name "case") "reserved0"
   )
  )

;; https://www.sqlite.org/lang_keywords.html
(define (sqlite-keywords)
  (map string-downcase
       (map ->string 
            '(ABORT
              ACTION
              ADD
              AFTER
              ALL
              ALTER
              ANALYZE
              AND
              AS
              ASC
              ATTACH
              AUTOINCREMENT
              BEFORE
              BEGIN
              BETWEEN
              BY
              CASCADE
              CASE
              CAST
              CHECK
              COLLATE
              COLUMN
              COMMIT
              CONFLICT
              CONSTRAINT
              CREATE
              CROSS
              CURRENT
              CURRENT_DATE
              CURRENT_TIME
              CURRENT_TIMESTAMP
              DATABASE
              DEFAULT
              DEFERRABLE
              DEFERRED
              DELETE
              DESC
              DETACH
              DISTINCT
              DO
              DROP
              EACH
              ELSE
              END
              ESCAPE
              EXCEPT
              EXCLUSIVE
              EXISTS
              EXPLAIN
              FAIL
              FILTER
              FOLLOWING
              FOR
              FOREIGN
              FROM
              FULL
              GLOB
              GROUP
              HAVING
              IF
              IGNORE
              IMMEDIATE
              IN
              INDEX
              INDEXED
              INITIALLY
              INNER
              INSERT
              INSTEAD
              INTERSECT
              INTO
              IS
              ISNULL
              JOIN
              KEY
              LEFT
              LIKE
              LIMIT
              MATCH
              NATURAL
              NO
              NOT
              NOTHING
              NOTNULL
              NULL
              OF
              OFFSET
              ON
              OR
              ORDER
              OUTER
              OVER
              PARTITION
              PLAN
              PRAGMA
              PRECEDING
              PRIMARY
              QUERY
              RAISE
              RANGE
              RECURSIVE
              REFERENCES
              REGEXP
              REINDEX
              RELEASE
              RENAME
              REPLACE
              RESTRICT
              RIGHT
              ROLLBACK
              ROW
              ROWS
              SAVEPOINT
              SELECT
              SET
              TABLE
              TEMP
              TEMPORARY
              THEN
              TO
              TRANSACTION
              TRIGGER
              UNBOUNDED
              UNION
              UNIQUE
              UPDATE
              USING
              VACUUM
              VALUES
              VIEW
              VIRTUAL
              WHEN
              WHERE
              WINDOW
              WITH
              WITHOUT))))
