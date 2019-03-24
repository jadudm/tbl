#lang scribble/manual
@(require tbl
          ;;scribble/eval
          scribble/example
          ;;scribble/struct
          racket/sandbox
          racket/bool
          racket/runtime-path
          "config.rkt"
          (for-label tbl))

@(define my-evaluator
   (call-with-trusted-sandbox-configuration
    (λ ()
      (parameterize ([sandbox-output 'string]
                     [sandbox-error-output 'string]
                     [sandbox-memory-limit 4096])
        (make-evaluator 'racket)
        ))))
    
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
          #:label false
          (require tbl)
          (define csv "http://bit.ly/wp-gun-deaths-csv")
          (define T (read-csv csv))
          ]

Did we succeed? A simple check would be to see how many rows are in our @racket[tbl].

@examples[#:eval my-evaluator
          #:label false
          (row-count T)]

If we don't know the data well, we might have to open it in Google Sheets, or Excel, or some other tool and check how many rows of data there are. A quick check on the command line might be to use the word count program @racketidfont{wc}:

@verbatim|{
  jadudm$ wc -l school-shootings-data.csv 
  222 school-shootings-data.csv
  }|

This tells us there are 222 lines in the file: one header row, and 221 rows of data. The @racket[row-count] function only counts rows of data, so our first ``sanity check'' is only our first step in working with this new set of data. We might also check the table, to find out whether the data is relatively clean, or if we have some work to do.

@examples[#:eval my-evaluator
          #:label false
          (check-table T)]

When you import a table, the importer guesses what kind of information is in each column. The @racket[check-table] function then checks to see how many values do (or do not) match that guess. Anywhere that a value is missing (or @racket[not-available?]) it will count as not matching. So, this might be a first check to see how many values are missing from your dataset.

As can be seen, we have many columns that have missing data. This data set will require some work before any analysis we might enact. Focusing on just the "weapon" column, we can confirm what @racket[check-table] has already told us: there is data missing.

@examples[#:eval my-evaluator
          #:label false
          (count-if T "weapon" not-available?)
          ]

@racket[count-if] will count how many elements of a column match the question that is asked; in this case, I'm asking how many values in the column labeled "weapon" are @racket[not-available?]. This is how we check for missing values in a @racket[tbl]. It matches the value of 34 that we got from the warning above, which suggests that we may have some cleaning to do if we want to use this column in any calculations.

@section[#:tag "exploring-the-data"]{Exploring the Data}

Our next step is to begin exploring the data. This means several things. First, it means getting a sense for the kinds of information in the @racket[tbl]. Second, it might mean understanding some summary statistics about the data---the averages, the means, and so on.

We have already seen @racket[row-count]; we can also check the number of columns, the column names, and other properties of the data.

@examples[#:eval my-evaluator
          #:label false
          (column-count T)
          (column-names T)          
          ]

For any given column of data, we might want to know some things about the data in that column. For example, there is a column that records what year a given shooting took place in. What unique years did shootings take place in?

@examples[#:eval my-evaluator
          #:label false
          (define years (pull T "year"))
          (define uyears (unique years))
          (length uyears)
          (sort uyears <)
          ]

Every year since 1999---which is not the beginning of gun deaths in schools in the United States, but it is when this data set begins its accounting.

How many people have been killed in these shootings?

@examples[#:eval my-evaluator
          #:label false
          (define deaths (pull T "killed"))
          (sum deaths)
          ]

What was the average number of deaths per shooting?

@examples[#:eval my-evaluator
          #:label false
          (mean deaths)
          ]

Racket will always try and give you an exact number; we can ask for the inexact answer:

@examples[#:eval my-evaluator
          #:label false
          (exact->inexact (mean deaths))
          ]

This tells us that every school shooting did not necessarily result in deaths; in some cases, it was only the case that easy access to guns in the United States allowed some children to be injured or maimed. We can filter the table so that we get a new table that only contains rows where school violence resulted in death:

@examples[#:eval my-evaluator
          #:label false
          (define dT (filter-rows T (> killed 0)))
          (row-count dT)
          (exact->inexact (/ (row-count dT) (row-count T)))
          ]

By taking the ratio of the row counts of these two tables, we get the percentage of all school shootings that resulted in deaths. In this case, approximately 1 in 4. 

Is there any pattern in terms of the day of the week these events happen on?

@examples[#:eval my-evaluator
          #:label false
          (group-with dT "day_of_week" "killed")
          (aggregate dT sum  "day_of_week" "killed")
          ]

This data set will not tell us why, but it seems like Thursdays are the least likely day of the week for a school shooting to occur. We can plot this, if we want, for a visual representation of the same information.

@examples[#:eval my-evaluator
          #:label false
          (require tbl/plot)
          (plot (hist dT "day_of_week" "killed"
                      #:order '("Monday" "Tuesday" "Wednesday" "Thursday" "Friday")))
          ]



Finally, if we want to understand what kinds of weapons were used, we would have some additional work to do. I'll perform this analysis with the full data set, and then show how I could abstract it to work with any subset of the data.

To do this, I'm going to write a function that matches some text patterns, and simplifies the data in this column.

@examples[#:eval my-evaluator
          #:label false
          
          (define (one-of? w low)
            (ormap (λ (o) (string-contains? w o)) low))

          (define (weapon->type weapon)
            (cond
              [(not-available? weapon) "unknown"]
              [(one-of? weapon '("XM-15" "XM15" "AR-15" "AK-47"))
               "semiautomatic rifle"]
              [(string-contains? weapon "shotgun")
               "shotgun"]
              [(string-contains? weapon "rifle")
               "rifle"]
              [(one-of? weapon '("handgun" "revolver" "handgin" "gun" "officer"))
               "handgun"]
              [else
               (error 'weapon->kind "Unknown weapon: ~s" weapon)]))

          (weapon->type ".22-caliber handgun")
          (weapon->type "Bushmaster XM-15 rifle")
          ]

With this function, I can add a new column to my table, and create a simplified set of categories for describing the weapons that were used in these acts of violence.

@examples[#:eval my-evaluator
          #:label false
          (add-column T "weapon_type" Text)
          (compute T "weapon_type" (function (weapon) (weapon->type weapon)))
          ]

The code above adds a new column to the full dataset, and uses the @racket[compute] form to add new data to our table. It goes through every row of the data, extracting the @racketidfont{weapon} column, and it then uses that as an argument to a function that computes the weapon type, and the result is inserted into the @racketidfont{weapon_type} column.

When this is done, we have a table that now has 51 columns instead of 50.

@examples[#:eval my-evaluator
          #:label false
          (column-count T)
          (unique (pull T "weapon_type"))
          ]

As with earlier explorations, we can now aggregate data on the basis of the kind of weapon that was used in the shooting.

@examples[#:eval my-evaluator
          #:label false
          (define wdT (filter-rows T (> killed 0)))
          (aggregate wdT sum  "weapon_type" "killed")
          ]

At a glance, this tells us that weapons I have categorized as handguns account for 51 of the killings in this dataset, and semiautomatic weapons account for 47. Rifles---like those that might be used by farmers for culling pests from their fields, or by responsible game hunters---account for only 2 deaths in schools since 1999.

Numerical data is often best communicated in pictures. The @racket[tbl] library includes some simplified plotting tools for quickly exploring data visually. 

@examples[#:eval my-evaluator
          #:label false
          #:hidden
          (require plot/no-gui)
          (plot-file (hist wdT "weapon_type" "killed")
                     "hist-1.png"
                     'png
                     #:width 600)
          ]

@examples[#:eval my-evaluator
          #:label false
          #:no-result
          (plot (hist wdT "weapon_type" "killed"))
          ]

@centered{@(image "hist-1.png" #:scale 0.8)}

And, because we are leveraging Racket for our data analysis, we can do a lot more with our plots; at the least, we might apply some labeling.

@examples[#:eval my-evaluator
          #:label false
          #:hidden
          (plot-file (hist wdT "weapon_type" "killed")
                     "hist-2.png"
                     'png
                     #:title "Weapons Used in School Schootings"
                     #:x-label "Weapon Used"
                     #:y-label "Deaths in schools since 1999"
                     #:width 600
                     )]

@examples[#:eval my-evaluator
          #:label false
          #:no-result
          (plot (hist wdT "weapon_type" "killed")
                #:title "Weapons Used in School Schootings"
                #:x-label "Weapon Used"
                #:y-label "Deaths in schools since 1999"
                )
          ]

@centered{@(image "hist-2.png" #:scale 0.8)}
