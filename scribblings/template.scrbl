#lang scribble/doc
@(require scribble/manual
          scribble/eval
          scribble/struct
          racket/sandbox
          racket/runtime-path
          "config.rkt"
          (for-label tbl tbl/operations))

@title[#:tag "using-tbl"]{Using @racket[tbl]}

This section provides a brief look at some common use cases for @racket[tbl].

@section[#:tag "intro-basic"]{Working with CSVs}

A large amount of data ships around the world as comma-separated files. For example, I found a @hyperlink[dataset of trees in Fingal county, Ireland](https://data.gov.ie/dataset/trees). (It is just north of the city of Dublin.)

The CSV file looks like this:

Tree_ID,Address,Town,Tree_Species,Species_Desc,Common_Name,Age_Desc,Height,Spread,Trunk,Actual_Trunk,Condition,Lat,Long
27092.0000000000,"Clonard Court, Balbriggan",Balbriggan,ACSA,Acer saccharinum,Silver Maple,Semi-Mature,5.1-10 metres,3 to 6 metres,11cm to 20cm,18cm,Good,53.6063254525,-6.1864238469
29144.0000000000,"Ridgewood Avenue, Forrest Great",Swords South,TICO,Tilia cordata,Small-Leafed Lime,Young,Up to 5 metres,Up to 3 metres,1cm - 10cm,10cm,Fair - Poor,53.4474654446,-6.2476129128
29160.0000000000,"Ridgewood Avenue, Forrest Great",Swords South,TICO,Tilia cordata,Small-Leafed Lime,Young,Up to 5 metres,Up to 3 metres,11cm to 20cm,11cm,Good,53.4476953324,-6.2482576292