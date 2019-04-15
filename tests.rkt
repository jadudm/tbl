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


;                                                                            
;                                                                            
;                                                                    ;       
;                                      ;                             ;       
;                                   ;  ;                             ;     ; 
;                                   ;                                ;     ; 
;    ;;;   ; ;;    ;;;   ;;;; ;; ; ;;; ;   ;;;   ; ;;   ;;;      ;;;;;  ;;;;;
;   ;; ;;  ;; ;;  ;; ;;  ;;  ;; ;; ;;; ;  ;; ;;  ;; ;;  ; ;      ;;  ; ;; ;;;
;   ;   ;; ;   ;; ;;;;;  ;   ;   ;  ;  ;  ;   ;; ;  ;;  ;;       ;   ;;    ; 
;   ;   ;; ;   ;; ;      ;   ;   ;  ;  ;  ;   ;; ;  ;;   ;;      ;   ;;;   ; 
;   ;   ;  ;;  ;  ;   ;  ;   ;   ;  ;  ;  ;   ;  ;  ;;  ; ;      ;   ; ;;  ; 
;    ;;;;  ;;;;;   ;;;   ;    ;;;;  ;  ;   ;;;;  ;  ;;  ;;;  ;;  ;   ;  ;; ; 
;          ;                                                                 
;          ;                                                                 
;          ;                                                                 
;                                                                            


;  ;;;;;;; ;;;;;;   ;;;;; ;;;;;;;  ;;;;; 
;     ;    ;       ;     ;   ;    ;     ;
;     ;    ;       ;     ;   ;    ;     ;
;     ;    ;       ;         ;    ;      
;     ;    ;;;;;;   ;;;;;    ;     ;;;;; 
;     ;    ;             ;   ;          ;
;     ;    ;       ;     ;   ;    ;     ;
;     ;    ;       ;     ;   ;    ;     ;
;     ;    ;;;;;;   ;;;;;    ;     ;;;;; 
;                                        
;                                        
;                                        

(require tbl/operations
         tbl/reading/gsheet
         tbl/reading/csv
         tbl/test/files
         tbl/test/data
         math)

(let ()
  
  (define flavorsT (read-gsheet "https://pult.us/u/flavors"))
  (define citiesT (read-gsheet "http://bit.ly/cities-csv"))
 
  (chk
   ;; What are the ages in the flavors GSheet?
   (get-column flavorsT 'age)  '(42 9 5)
   ;; What if I use a string for the column name?
   (get-column flavorsT "age") '(42 9 5)

   ;; What are the cities in the tbl?
   ;; Placed the list of cities in a test/data file.
   (get-column citiesT "City") all-cities
   

   ;; What is the sum of the latitude degrees?
   ;; This checks that the integers came in as integers.
   (apply + (get-column citiesT "LatD")) 4969
   ;; How many times does Ohio show up in the tbl?
   (length (filter (λ (s) (equal? s "OH")) (get-column citiesT "State"))) 6
   ;; What about Maine? Oh. Maine gets no love.
   (length (filter (λ (s) (equal? s "ME")) (get-column citiesT "State"))) 0

   ;; Does pick return a table with fewer columns?
   (count-columns flavorsT) 3
   (count-columns (new-tbl-from-columns flavorsT "age" "name")) 2

   ;; All the rows where a condition holds true.
   ;; Note that ROWID is not included when we ask for all of the rows.
   (get-rows (filter-rows flavorsT (and (> age 3)
                                        (or (= flavor "Mint")
                                            (= flavor "Chocolate")))))
   '(("Matt" 42 "Chocolate") ("Matthew" 9 "Mint"))

   ;; Another check; essentially same as the previous
   (count-rows
    (filter-rows flavorsT (and (> age 3)
                               (or (= flavor "Mint")
                                   (= flavor "Chocolate")))))
   2
   
   ;; Check that we end up with a new table that only has
   ;; six rows in it. It should still have 10 columns.
   (count-rows (filter-rows citiesT (= State "OH"))) 6
   (count-columns (filter-rows citiesT (= State "OH"))) 10
   
   ) ;; end of chk

  ;; Try extending the table, and computing new values.
  (quietly
   (add-column flavorsT "double_age" Integer)
   (compute flavorsT
            "double_age"
            (function (age) (* age 2))))

  (chk
   ;; FIXME When I handle params that are not named correctly,
   ;; this test may need to be updated.
   #:x (compute flavorsT
                  "double_age"
                  (function (agex) (* agex 2)))
   " "
   
   (count-columns flavorsT) 4
   ;; These do the same thing two different ways.
   (map fourth (get-rows flavorsT)) '(84 18 10)
   (get-column flavorsT "double_age") '(84 18 10)
   )
    
  (define sdT (read-csv school-shootings))
  
  (chk
   (count-rows sdT) 221
   (count-rows (filter-rows sdT (> killed 0))) 59
   )
  
  (define CT1 (read-gsheet "http://bit.ly/cities-csv"))
  (define CT2 (remove-columns citiesT "NS" "EW" "State"))
  (chk
   (count-columns CT1) (length '(LatD LatM LatS NS LonD LonM LonS EW City State))
   (count-columns CT2) (length '(LatD LatM LatS LonD LonM LonS City))
   (set=? (map string->symbol (column-names CT1))
               '(LatD LatM LatS NS LonD LonM LonS EW City State))
   (set=? (map string->symbol (column-names CT2))
               '(LatD LatM LatS LonD LonM LonS City)))
  
  ;; Using a bigger table
  #;(begin
      (define gunsT (read-csv gun-deaths-csv))
      (chk
     
       ;; What about a bigger tbl?
       (length (pull gunsT 'age)) 100798
       (count-rows gunsT) 100798
       ;; The columns in this table are
       ;; "col0","year","month","intent","police","sex","age","race","hispanic","place","education"
       ;; Check if we can pick three from the table
       (count-columns (pick gunsT "year" "sex" "race")) 3
       ))

    
  )

;; Deleting the nth row
;; https://stackoverflow.com/questions/23791239/sqlite-delete-nth-row-android