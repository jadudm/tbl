#lang racket
(require tbl)

(define T (make-tbl "Table 3"
                    '("person" "treatment" "result")
                    '(text integer integer)))

(add-row! T "John Smith" "a" none)
(add-row! T "Jane Doe" "a" 16)
(add-row! T "Mary Johnson" "a" 3)
(add-row! T "John Smith" "b" 2)
(add-row! T "Jane Doe" "b" 11)
(add-row! T "Mary Johnson" "b" 1)


;; Variables and observations
;; A variable is the set of all measurements for the same
;; underlying attribute; eg "height".
;; An observation is all values measured on the same
;; unit (eg. person, day) across all attributes

;: Generally easier to describe relationships
;; between variables... which helps us know
;; what are variables vs. observations.

;; Tidy data:
;; Each variable forms a column.
;; Each observation forms a row.
;; Each type of observational unit forms a table.

;; The focus is on a single dataset, rather than
;; many connected datasets common in relational
;; databases. (But... I thought that was a single
;; dataset...)

;; Five common problems.
;; Column headers are values, not variable names.
;; Multiple variables are stored in one column.
;; Variables are stored in both rows and columns.
;; Multiple types of observational units are stored
;;   in the same table.
;; A single observational unit is stored in multiple tables.

(define M (make-tbl "Melt"
                    '("rw" "a" "b" "c")
                    '(text integer integer integer)))
(add-row! M '(A 1 4 7))
(add-row! M '(B 2 5 8))
(add-row! M '(C 3 6 9))


#|
BEGIN TRANSACTION;
CREATE TEMPORARY TABLE t1_backup(a,b);
INSERT INTO t1_backup SELECT a,b FROM t1;
DROP TABLE t1;
CREATE TABLE t1(a,b);
INSERT INTO t1 SELECT a,b FROM t1_backup;
DROP TABLE t1_backup;
COMMIT;
|#

(require db)
(define (remove-ndx n ls)
  (cond
    [(empty? ls) empty]
    [(zero? n) (remove-ndx (sub1 n) (rest ls))]
    [else
     (cons (first ls)
           (remove-ndx (sub1 n) (rest ls)))]))

(define (remove-column T col)
  (define ndx (index-of (column-names T) col))
  (define new-columns (remove-ndx ndx (column-names T)))
  (define new-types   (remove-ndx ndx (tbl-types T)))
  (define newT (make-tbl (tbl-name T)
                         new-columns
                         new-types))
  (for ([row (get-rows T)])
    (add-row! newT (remove-ndx ndx row)))
  ;; Close the old connection
  (disconnect (tbl-db T))
  ;; Set the old table to point to the new one.
  newT)
                                
(define (remove-columns T . cols)
  (for ([c cols])
    (set! T (remove-column T c)))
  T)

(define (column-swap T orig new)
  (list-set (tbl-columns T)
            (index-of (tbl-columns T) orig)
            new))

(define (rename-column T old new)
  (define newT (make-tbl
                (tbl-name T)
                (column-swap T old new)
                (tbl-types T)))
  (for ([row (get-rows T)])
    (add-row! newT row))
  (disconnect (tbl-db T))
  newT)
        
;; The data in the CSV is different from what is published in the paper.
;; I'm going to drop "genre" and "date.peaked" to be consistent.
;(set! B (remove-column B "genre"))
;(set! B (remove-column B "datepeaked"))
;(set! B (remove-column B "artistinverted"))
;(set! B (remove-columns B "genre" "datepeaked" "artistinverted"))

(define (melt T . ids)
  (define colvars (list->set ids))
  (define columnS (list->set (column-names T)))
  (define vars    (set-subtract columnS colvars))
  ;; Now, I need a new table. It needs to contain the ids as columns,
  ;; and a new column called... melted. (Why reuse "column"?)
  (define new-types
    (for/list ([id ids])
      (list-ref (tbl-types T)
                (index-of (tbl-columns T) id ))))
    
  (define newT (make-tbl (tbl-name T)
                         (append ids '("variable" "value"))
                         (append new-types '(text text))))
  (for ([row (get-rows T)])
    (define to-insert
      (for ([id ids])
      (list-ref row (index-of (tbl-columns T) id ))))
    (for ([v vars]) 
      (define value (list-ref row (index-of (tbl-columns T) v)))
      (add-row! newT
                (append to-insert (list v value))))
    )
  (disconnect (tbl-db T))
  newT)
    

;;(define mM (melt M "rw"))

(define (billboard)
  ;; This data changed from the version in the paper.
  (define B-csv "https://raw.githubusercontent.com/hadley/tidy-data/master/data/billboard.csv")
  (define B (read-csv B-csv))
  (set! B (remove-columns B "datepeaked"))
  (set! B (rename-column B "artistinverted" "artist"))
  (set! B (melt B "year" "artist" "time" "track" "genre" "dateentered"))
  (add-column! B "week" Integer)
  (printf "~s~n" (column-names B))
  (set! B
        (compute B "week" (function (variable)
                            (define found
                              (second (regexp-match #px"([0-9]+)" variable)))
                            (string->number found))))
  (set! B (remove-column B "variable"))
  B
  )
(define newB (billboard))
(row-count newB)
(column-count newB)
(take (get-rows newB) 5)
