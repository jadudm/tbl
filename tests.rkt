#lang racket
(require chk
         db sql
         tbl/types
         )



;                                                    
;                                                    
;   ;                                        ;       
;   ;                  ;                     ;       
;   ;                  ;                     ;     ; 
;   ;                                        ;     ; 
;   ; ;;    ;; ;  ;;;  ;   ;;;  ;;;      ;;;;;  ;;;;;
;   ;; ;;  ;; ;;  ; ;  ;  ;; ;  ; ;      ;;  ; ;; ;;;
;   ;   ;; ;   ;  ;;   ;  ;     ;;       ;   ;;    ; 
;   ;   ;; ;   ;   ;;  ;  ;      ;;      ;   ;;;   ; 
;   ;;  ;  ;   ;  ; ;  ;  ;     ; ;      ;   ; ;;  ; 
;   ;;;;;   ;;;;  ;;;  ;   ;;;  ;;;  ;;  ;   ;  ;; ; 
;                                                    
;                                                    
;                                                    
;
(require tbl/basics)
(let ()
;; Creating an empty table.
(define empty-table (make-tbl))

;; Setting the name of an empty table.
(define T1 (make-tbl))
(set-tbl-name! T1 "Transterpreter")

;; Creating a table with a name.
(define T2 (make-tbl "concurrency.cc"))

;; Creating a table with columns and types
(define T3 (make-tbl "jadud.com" '(a b c) '(number number text)))

;; Adding a column
(define T4 (make-tbl))
(add-column T4 'bob 'integer)
;; Adding a row
(add-row T4 '(1))

;; A more table
(define T5 (make-tbl "grocery"
                     '(product qty cost)
                     '(text integer real)))
(add-row T5 '(apple 10 1.35))
(add-row T5 '(kiwi  20 0.75))
;; Check that the .args version works.
(add-row T5 '(nuts 100 0.02))

(chk
 #:t (tbl? (make-tbl))
 (tbl-name T1) "Transterpreter"
 (tbl-name T2) "concurrencycc"
 (length (tbl-columns T3)) 3
 (length (tbl-types T3))  3

 ;; Check if the insert into T5 works.
 (query-rows (tbl-db T5) (select (.*)
                                 #:from (TableRef:INJECT ,(tbl-name T5))))
 '(#(1 "apple" 10 1.35) #(2 "kiwi" 20 0.75) #(3 "nuts" 100 0.02))

 ;;(printf "~s~n" (column-names T5))
 ))



;                                                        
;                                                        
;         ;                   ;                  ;       
;         ;                   ;                  ;       
;         ;                   ;                  ;     ; 
;         ;                   ;                  ;     ; 
;    ;;;  ; ;;    ;;;    ;;;  ;  ;; ;;;      ;;;;;  ;;;;;
;   ;; ;  ;; ;;  ;; ;;  ;; ;  ; ;;  ; ;      ;;  ; ;; ;;;
;   ;     ;  ;;  ;;;;;  ;     ;;    ;;       ;   ;;    ; 
;   ;     ;  ;;  ;      ;     ;;;    ;;      ;   ;;;   ; 
;   ;     ;  ;;  ;   ;  ;     ; ;;  ; ;      ;   ; ;;  ; 
;    ;;;  ;  ;;   ;;;    ;;;  ;  ;; ;;;  ;;  ;   ;  ;; ; 
;                                                        
;                                                        
;                                                        
;                                                        
(require tbl/checks)
(let ()
(define (make-donut-table-1)
  (define T (make-tbl "donuts"
                      '(type topping cost)
                      '(text text number)))
  (add-row T (list "plain" "A" 0.75))
  (add-row T (list "cake" "A" 1.00))
  (add-row T (list "chocolate" "glazed" 1.25))
  T)
(define (make-donut-table-2)
  (define T (make-tbl "donuts"
                      '(type topping cost)
                      '(text text number)))
  (add-row T (list "plain" none 0.75))
  (add-row T (list "cake" none 1.00))
  (add-row T (list "chocolate" "glazed" 1.25))
  T)

(define T1 (make-donut-table-1))
(define T2 (make-donut-table-2))
    
; (columns-should-be T '(string string number))
  
(chk
 (count-rows T1) 3
 (count-columns T1) 3
 ;;(check-column T "type") true
 ;; "A" "B1T" 
 (check-tbl T1) true
 ;; "A" "B2T" 
 (check-columns-are? T1 '(any any any)) true
 ;; "A" "B3F" 
 (check-columns-are? T1 '(blob blob blob)) false
 ;; "A" "B4T" 
 (check-columns-are? T1 '(text text number)) true
 ))

