#lang scribble/doc
@(require scribble/manual
          scribble/eval
          scribble/struct
          racket/sandbox
          racket/runtime-path
          "config.rkt"
          (for-label tbl tbl/operations))

@(define my-evaluator
   (call-with-trusted-sandbox-configuration
    (λ ()
     (parameterize ([sandbox-output 'string]
                    [sandbox-error-output 'string]
                    [sandbox-memory-limit 50])
       (make-evaluator 'racket)))))
    
@title[#:tag "exploring-data-with-tbl"]{Exploring Data with @racket[tbl]}

@italic{This section provides a "worked example" for exploring data with @racket[tbl].}

The Washington Post has been compiling a @hyperlink["https://github.com/washingtonpost/data-school-shootings"]{database of school shootings in the United States} that have occurred since the Colombine massacre in 1999. The Second Ammendment to the US constitution, ratified in 1791, was written at a time when muskets were the representative firearms of the day; someone incredibly proficient with a musket might attain a rate of fire of 3, perhaps 4 shots per minute. An AR-15, used in every mass killing in schools and churches in the US since 2012, is capable of firing dozens---and hundreds, with modifications---of rounds per minute.

We can use @racket[tbl] to better understand the horrors that unfettered access to these weapons enable.

@section[#:tag "reading-the-data"]{Reading the Data}

@(define csv "https://raw.githubusercontent.com/washingtonpost/data-school-shootings/master/school-shootings-data.csv")

The Post makes their data available under a CC BY-SA license, and @hyperlink["https://github.com/washingtonpost/data-school-shootings"]{it is stored in Github}. The data is available in a CSV file, which stands for "comma-separated values." It is reasonable to envision a CSV file as a spreadsheet; the first row (often) contains the names of the columns in the data, and each line of the file is a single row of data.

Our first step, therefore, is to get the "raw" link to the data, and use that URL to load a copy of the CSV over the 'net and into a @racket[tbl] structure.

@; "https://raw.githubusercontent.com/washingtonpost/data-school-shootings/master/school-shootings-data.csv"

@examples[#:eval my-evaluator
             (require tbl)
             (define csv "http://bit.ly/wp-gun-deaths-csv")
             (define T (read-csv csv))
             ]

Did we succeed? A simple check would be to see how many rows are in our @racket[tbl].

@examples[#:eval my-evaluator
          (row-count T)]

If we don't know the data well, we might have to open it in Google Sheets, or Excel, or some other tool and check how many rows of data there are. A quick check on the command line might be to use the word count program @racketidfont{wc}:

@verbatim|{
jadudm$ wc -l school-shootings-data.csv 
     222 school-shootings-data.csv
}|

This tells us there are 222 lines in the file: one header row, and 221 rows of data. The @racket[row-count] function only counts rows of data, so our first ``sanity check'' in our work with this data suggests we're good to continue.

@section[#:tag "exploring-the-data"]{Exploring the Data}

Our next step is to begin exploring the data. This means several things. First, it means getting a sense for the kinds of information in the @racket[tbl]. Second, it might mean understanding some summary statistics about the data---the averages, the means, and so on.

We have already seen @racket[row-count]; we can also check the number of columns, the column names, and other properties of the data.

@examples[#:eval my-evaluator
          (column-count T)
          (column-names T)          
          ]

For any given column of data, we might want to know some things about the data in that column. For example, there is a column that records what year a given shooting took place in. What unique years did shootings take place in?

@examples[#:eval my-evaluator
          (define years (pull T "year"))
          (define uyears (unique years))
          (length uyears)
          (sort uyears <)
          ]

Every year since 1999---which is not the beginning of gun deaths in schools in the United States, but it is when this data set begins its accounting.

How many people have been killed in these shootings?

@examples[#:eval my-evaluator
          (define deaths (pull T "killed"))
          (sum deaths)
          ]

What was the average number of deaths per shooting?

@examples[#:eval my-evaluator
          (mean deaths)
          ]

Racket will always try and give you an exact number; we can ask for the inexact answer:

@examples[#:eval my-evaluator
          (exact->inexact (mean deaths))
          ]

This tells us that every school shooting did not necessarily result in deaths; in some cases, it was only the case that easy access to guns in the United States allowed some children to be injured or maimed. We can filter the table so that we get a new table that only contains rows where school violence resulted in death:

@examples[#:eval my-evaluator
          (define dT (filter-rows T (> killed 0)))
          (row-count dT)
          (exact->inexact (/ (row-count dT) (row-count T)))
          ]

By taking the ratio of the row counts of these two tables, we get the percentage of all school shootings that resulted in deaths. In this case, approximately 1 in 4. 

Is there any pattern in terms of the day of the week these events happen on?

@examples[#:eval my-evaluator
          (group-with dT "day_of_week" "killed")
          (aggregate dT sum  "day_of_week" "killed")
          ]

This data set will not tell us why, but it seems like Thursdays are the least likely day of the week for a school shooting to occur. Finally, if we want to understand what kinds of weapons were used, we would have some additional work to do. I'll perform this analysis with the full data set, and then show how I could abstract it to work with any subset of the data.

@examples[#:eval my-evaluator
          (define weapon (pull T "weapon"))
          (define uweapons (unique weapons))
          ]

To do this, I'm going to write a function that matches some text patterns, and simplifies the data in this column.

@(require tbl racket/string)
@(let ()
   (define csv "http://bit.ly/wp-gun-deaths-csv")
   (define T (read-csv csv))
   (define weapon (pull T "weapon"))
   (define uweapon (unique weapon))
   (define (weapon->kind weapon)
     (cond
       [(not-available? weapon) "unknown"]
       [(ormap (λ (kind)
                 (string-contains? weapon kind))
               '("XM-15" "XM15" "AR-15")) "semiautomatic rifle"]
       [(string-contains? weapon "AK-47") "semiautomatic rifle"]
       [(string-contains? weapon "shotgun") "shotgun"]
       [(string-contains? weapon "rifle") "rifle"]
       [(ormap (λ (choice)
                 (string-contains? weapon choice))
               '("handgun" "revolver" "handgin" "gun" "officer")) "handgun"]
       
       [else
        not-available?(error 'weapon->kind "Unknown weapon: ~s" weapon)]))
   (map weapon->kind uweapon)
   )

   