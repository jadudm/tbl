#lang racket

(provide (all-defined-out))

(define all-cities
  '("Youngstown" "Yankton" "Yakima" "Worcester" "Wisconsin Dells" "Winston-Salem" "Winnipeg" "Winchester" "Wilmington" "Wilmington" "Williston" "Williamsport" "Williamson" "Wichita Falls" "Wichita" "Wheeling" "West Palm Beach" "Wenatchee" "Weed" "Waycross" "Wausau" "Waukegan" "Watertown" "Watertown" "Waterloo" "Waterbury" "Washington" "Warren" "Walla Walla" "Waco" "Vincennes" "Victoria" "Vicksburg" "Vancouver" "Valley City" "Valdosta" "Utica" "Uniontown" "Tyler" "Twin Falls" "Tuscaloosa" "Tupelo" "Tulsa" "Tucson" "Trinidad" "Trenton" "Traverse City" "Toronto" "Topeka" "Toledo" "Texarkana" "Terre Haute" "Tampa" "Tallahassee" "Tacoma" "Syracuse" "Swainsboro" "Sumter" "Stroudsburg" "Stockton" "Stevens Point" "Steubenville" "Sterling" "Staunton" "Springfield" "Springfield" "Springfield" "Springfield" "Spokane" "South Bend" "Sioux Falls" "Sioux City" "Shreveport" "Sherman" "Sheridan" "Seminole" "Selma" "Sedalia" "Seattle" "Scranton" "Scottsbluff" "Schenectady" "Savannah" "Sault Sainte Marie" "Sarasota" "Santa Rosa" "Santa Fe" "Santa Barbara" "Santa Ana" "San Jose" "San Francisco" "Sandusky" "San Diego" "San Bernardino" "San Antonio" "San Angelo" "Salt Lake City" "Salisbury" "Salinas" "Salina" "Salida" "Salem" "Saint Paul" "Saint Louis" "Saint Joseph" "Saint Joseph" "Saint Johnsbury" "Saint Cloud" "Saint Augustine" "Saginaw" "Sacramento" "Rutland" "Roswell" "Rocky Mount" "Rock Springs" "Rockford" "Rochester" "Rochester" "Roanoke" "Richmond" "Richmond" "Richfield" "Rhinelander" "Reno" "Regina" "Red Bluff" "Reading" "Ravenna"))