#lang racket

(require table
         table/types
         table/util/util
         db sql
         table/reading/gsheet
         )

(provide (all-defined-out))

;; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ;
;; PULL
(define (pull T column)
  (define Q (select (ScalarExpr:INJECT ,(->string column))
                    #:from (TableRef:INJECT ,(table-name T))))
  (query-list (table-db T) Q))

;; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ;
;; SELECT
(define (pick T . columns)
  (define Q (format "SELECT ~a FROM ~a"
                    (apply string-append (add-between (map ->string columns) ","))
                    (table-name T)))
  ;; (printf "~s~n" Q)
  (define rows (query-rows (table-db T) Q))
  ;; I want a table to come back. So, create a new table, and insert all the things.
  (define newT (make-table (format "~a_~a" (table-name T)
                                   (apply string-append (add-between columns "_")))
                           columns
                           (map (λ (col)
                                  (list-ref (table-types T)
                                            (index-of columns col)))
                                columns)))
  (for ([row rows])
    (add-row! newT row))
  newT                           
  )

(module+ test
  (require rackunit/chk
           table/reading/gsheet
           table/reading/csv
           table/test/files
           math)
  
  (define flavorsT (read-gsheet "https://pult.us/u/flavors"))
  (define citiesT (read-gsheet "http://bit.ly/cities-csv"))
  (define gunsT (read-csv gun-deaths-csv))
  
  (chk
   ;; What are the ages in the flavors GSheet?
   (pull flavorsT 'age)  '(42 9 5)
   ;; What if I use a string for the column name?
   (pull flavorsT "age") '(42 9 5)

   ;; What are the cities in the table?
   (pull citiesT "City")
   '("Youngstown" "Yankton" "Yakima" "Worcester" "Wisconsin Dells" "Winston-Salem" "Winnipeg" "Winchester" "Wilmington" "Wilmington" "Williston" "Williamsport" "Williamson" "Wichita Falls" "Wichita" "Wheeling" "West Palm Beach" "Wenatchee" "Weed" "Waycross" "Wausau" "Waukegan" "Watertown" "Watertown" "Waterloo" "Waterbury" "Washington" "Warren" "Walla Walla" "Waco" "Vincennes" "Victoria" "Vicksburg" "Vancouver" "Valley City" "Valdosta" "Utica" "Uniontown" "Tyler" "Twin Falls" "Tuscaloosa" "Tupelo" "Tulsa" "Tucson" "Trinidad" "Trenton" "Traverse City" "Toronto" "Topeka" "Toledo" "Texarkana" "Terre Haute" "Tampa" "Tallahassee" "Tacoma" "Syracuse" "Swainsboro" "Sumter" "Stroudsburg" "Stockton" "Stevens Point" "Steubenville" "Sterling" "Staunton" "Springfield" "Springfield" "Springfield" "Springfield" "Spokane" "South Bend" "Sioux Falls" "Sioux City" "Shreveport" "Sherman" "Sheridan" "Seminole" "Selma" "Sedalia" "Seattle" "Scranton" "Scottsbluff" "Schenectady" "Savannah" "Sault Sainte Marie" "Sarasota" "Santa Rosa" "Santa Fe" "Santa Barbara" "Santa Ana" "San Jose" "San Francisco" "Sandusky" "San Diego" "San Bernardino" "San Antonio" "San Angelo" "Salt Lake City" "Salisbury" "Salinas" "Salina" "Salida" "Salem" "Saint Paul" "Saint Louis" "Saint Joseph" "Saint Joseph" "Saint Johnsbury" "Saint Cloud" "Saint Augustine" "Saginaw" "Sacramento" "Rutland" "Roswell" "Rocky Mount" "Rock Springs" "Rockford" "Rochester" "Rochester" "Roanoke" "Richmond" "Richmond" "Richfield" "Rhinelander" "Reno" "Regina" "Red Bluff" "Reading" "Ravenna")

   ;; What is the sum of the latitude degrees?
   ;; This checks that the integers came in as integers.
   (apply + (pull citiesT "LatD")) 4969
   ;; How many times does Ohio show up in the table?
   (length (filter (λ (s) (equal? s "OH")) (pull citiesT "State"))) 6
   ;; What about Maine? Oh. Maine gets no love.
   (length (filter (λ (s) (equal? s "ME")) (pull citiesT "State"))) 0

   ;; Does pick return a table with fewer columns?
   (column-count flavorsT) 3
   (column-count (pick flavorsT "age" "name")) 2

   ;; What about a bigger table?
   (length (pull gunsT 'age)) 100798
   (row-count gunsT) 100798
   
   ) ;; end of chk

)