#lang scribble/jfp
@(require scribble/manual
          scribble/example
          racket/sandbox
          )

@(define ev
   (call-with-trusted-sandbox-configuration
    (λ ()
      (parameterize ([sandbox-output 'string]
                     [sandbox-error-output 'string]
                     [sandbox-memory-limit 4096])
        (make-evaluator 'racket
                        #:requires
                        '())
        ))))

@((author/short "Matt Jadud")
  "Matthew C. Jadud"
  @affiliation[
 "Bates College"])

@title{Designing Programs with Data}

@abstract{
 What does it mean to systematically engage in the design programs that work with data? The word ``systematic'' suggests a process, clearly articulated and easily understood, that can be used to achieve our goal. My goal is to teach students programming from a principled perspective that they can apply regardless of the language they use; the design of programs that work with data are a subclass of this larger pedagogic challenge. This paper explores the dual notions of designing programs and designing data, so that we might begin to explore the idea of @italic{designing programs with data}.
}

@section{Designing Programs}

The text @italic{How to Design Programs} lays out a program design process that centers a systematic and intentional approach to writing small and medium-sized programs. The principles, in truth, scale quite well to surprisingly complex software systems (measuring teans of thousands of lines of code across a dozen files). Fundamentally, the text encourages the following of a @italic{design recipe}:

@itemlist[ @item{Define the data.}
          @item{Write a signature, purpose statement, and function header.}
          @item{Write functional examples.}
          @item{Write a function template.}
          @item{Define the function.}
          @item{Test.}]

The purpose of the design recipe, for a learner, is to provide a consistent process by which they can approach the design of programs and authoring of code. The data definition encourages them to think about the structure of their data. It may be atomic (like a number, lacking internal structure), compound (like a cartesian point or an address that might have a street, city, and zip code), or linked/self-referential (like the definition of a list, tree, or graph).

@#reader scribble/comment-reader
(racketblock
;; DATA CONTRACT
;; A list-of-numbers is EITHER
;;  - empty, OR
;;  - a number followed by a list-of-numbers
)

The signature encourages the student to think about what kind, or @italic{type}, of information is being passed to the function to be operated on, and what type of information is being returned. The purpose statement requires them to think about what the function is supposed to do in its computation, and the header demonstrates that the student can express the signature in code.

@#reader scribble/comment-reader
(racketblock
;; SIGNATURE
;; sum-list : list-of-numbers -> number
;; PURPOSE
;; To sum up the numbers in a list.
(define (sum-list lonum)
...)
)

Functional examples require the student to think about how @bold{actual} inputs will map to @bold{actual} outputs. It is one thing to say that a function will sum the numbers in a list; it is another thing to say that the list @racket[(list 1 2 3)] will sum to the number @racket[6]. Similarly, the programmer should ideally have examples that explore the entire data definition; in this case, they should have an example that demonstrates what happens when we attempt to sum all of the numbers in the empty list.

@(racketblock
  (check-equal? (sum-list empty) 0)
  (check-equal? (sum-list (list 0)) 0)
  (check-equal? (sum-list (list 0 0)) 0)
  (check-equal? (sum-list (list 1 2 3)) 6))

The template is a learned thing: for representative data types, the students learn how to write a template that follows the structure of the data. For compound data, they learn to extract all the parts. For data with conditional definitions (like a list), they learn to write a conditional that follows from the definition. In this way, they practice seeing the connection between the structure of their data and the structure of their code.

@#reader scribble/comment-reader
(racketblock
;; SIGNATURE
;; sum-list : list-of-numbers -> number
(define (sum-list lonums)
(cond
[(empty? lonums) ...]
[else
... (first lonums) ...
... (rest lonums)  ...
])))

For definitions that are both compound and self-referential, the students also learn to read what @italic{type} of data each extracted subpart is, and to trust their function signature in the authoring of the template. In this case, they need to begin reading @racket[(first lonums)] as a number, because the data definition says that a list is a number followed by a list-of-numbers. Therefore, @racket[(rest lonums)] must be a list-of-numbers, because the definition needs to be trusted. We are writing a function that sums lists of numbers. Given that @racket[(rest lonums)] is a list of numbers, we should probably sum it up. This suggests a template that looks like the following:

@#reader scribble/comment-reader
(racketblock
;; SIGNATURE
;; sum-list : list-of-numbers -> number
(define (sum-list lonums)
(cond
[(empty? lonums) ...]
[else
... (first lonums) ...
... (sum-list (rest lonums)) ...
])))

Now, the last step is for the student to author the function. They have thought through the kind of data they are operating on, the structure of that data, how it will go in-and-out of the function they are writing, examples of that process, and a template that will structure their solution based on the structure of the data itself. The student has already decided that an empty list must sum to zero (by their examples). The question then becomes: if you are given a number (the @racket[(first lonums)]) and a number (as given by @racket[(sum-list (rest lonums))]), how do you combine them into a sum? The answer becomes @racket[+].

@#reader scribble/comment-reader
(racketblock
;; SIGNATURE
;; sum-list : list-of-numbers -> number
(define (sum-list lonums)
(cond
[(empty? lonums) 0]
[else
(+ (first lonums) 
(sum-list (rest lonums)))
])))

This process works, with minor variation, for the design of classes (in object-oriented programs), as well as thinking about code in languages that espouse fundamentally different semantics (eg. parallel languages like Google's Go, or asynchronous functional languages like Javascript).  It serves as a basis for thinking about more complex program design, as it keeps the programmer focused on questions of what goes into and out of a system, as well as the authoring of tests that provide a modicum of consistency as the system is developed. This has many practical side-effects; in a programmer's daily practice, this is the kind of documentation that makes it possible to not only read, but run, check, and rely on (with confidence) changes to code weeks or months after the original code was authored.

The question I want to explore is what this means for the design and implementation of programs that fundamentally manipulate data. The design recipe assumes that any one function will operate on data of a specific type. Many introductions I have seen to working with data assume, from the start, that the inputs are ``messy,'' or otherwise will need substantial work and manipulation before the programmer can even begin their analysis. Often, the process of ``cleaning'' or ``tidying'' the data (which, in the much older literature of databases, would be called the @italic{normalization} of the data), the programmer is already aware of the analyses they wish to perform as they organize and clean their data. In other words, the process of tidying data presumes an understanding of the analysis, which suggests a level of expertise that novices almost certainly do not have.


Hence, the question: what is a principled approach to the design of programs that work with data, and what can we take from structured, pedagogic approaches to the learning of programming as we think about the teaching and learning around the design of data-oriented programs?

@section{Working with Tabular Data}

I have not defined what ``data'' is. Because @italic{anything} that contains information might be encoded and processed, in some way, as data, I am not interested in tackling the overlarge question of ``what is data?'' Instead, I will start with @italic{tabular data}, as it is (I believe) easier to define.

In the abstract, tabular data is ``two-dimensional data.'' It is the spreadsheet: rows and columns of data, where the variety of types of data contained in each cell is dictated in no small part by the language. For example, if a relational table is being declared in SQL, the types of data that can be contained within the database are constrained by the flavor of SQL the database implements. MySQL supports six different types of integers: @racketidfont{INTEGER}, @racketidfont{INT}, @racketidfont{SMALLINT}, @racketidfont{TINYINT}, @racketidfont{MEDIUMINT}, and @racketidfont{BIGINT}, each of which takes up a different amount of space within the database table, and each has its own limitations and performance implications. If using SQLite, a small, portable database common on many mobile platforms (it is the default database implementation on Android, for example}), there is only one integer type (@racketidfont{INTEGER}) and one floating point type (@racketidfont{REAL}).

This is to point out that while numerical work in mathematics may often choose to represent tabular data as matricies, and R may use tables (or tibbles, or perhaps other structures as well), there is no one ``cononical'' representation for tabular data. Mathematicians will use tabular data in different ways (and for different purposes) than artists, and likewise than linguists and historians. There is no ``one-size-fits-all'' in this space.

I am, however, postulating that we can choose how we introduce students to thinking about and working with tabular data. And, there may be different starting points when we approach this from different disciplinary perspectives... but, there may be common principles that emerge as we consider use cases.

@subsection{A Header and Rows}

I will begin by suggesting that students being introduced to tabular data should always be introduced to well-formed data. It is difficult enough to learn to write code correctly without worrying about whether the substrate over which you are computing is in error. Learning to write defensive code---code that assumes inputs may be malformed---is not easy.

For this reason, I am going to begin by assuming that when students begin learning about tabular data, it is well specified. A table will always have:


@itemlist[ @item{A header row,}
          @item{columns of equal length,}
          @item{values that conform to a common type for each column (eg. a column of integers contains only integers),}
          @item{and no missing values.}
          ]

The CORGIS project at Virginia Tech packages interesting data sets up for novices as libraries that can be imported via their programming language's native import mechanism. This is important for several reasons.

First, it is often the case that novices do not yet have a full, working knowledge of the filesystem on their computer (or, if they are using a web-based service, the filesystem as presented through a web-based interface). They therefore commonly place files in the wrong place, wrestle with naming files correctly, and regularly use spaces or punctuation that a programming language has a hard time parsing as part of a file open call.

Second, it is easy for files to be in the wrong format, depending on how students download or otherwise obtain the data they are supposed to be working with. It also remains the case that string terminators are inconsistent across platforms and languages, that character set differences can result in the mishandling of critical characters (like the `` for example). These kinds of problems can become sources of error in a students' code that lead to extremely opaque errors; sometimes, it is deep in the bowels of a library that an error is presented, leaving the student to be completely baffled as to what to do. On a bad day, a tired instructor might be caught up in the confusion as well.

For these (and possibly other) reasons, it is best when working with beginners to provide a way to import, or otherwise obtain, the data in a fairly automatic way. If students are using Jupyter Notebooks on a centralized service, then their initial datasets should be pre-installed, so that a single @racketidfont{import} (in Python) or @racketidfont{library} call (in R) will pull in the data the students need to do their work. In DrRacket, a programming environmemnt designed for novices working with Racket (a child of Scheme and grandchild of LISP), students can install packages directly in from the interface, either from a graphical package manager, or by pasting in a URL provided by the instructor.

INSERT IMAGE HERE.

@subsection{Foundational Operations}

When students are using @italic{How to Design Programs} as a text, they first learn about atomic data. The programs they write operate on numbers, strings (which are treated as atomic data without subparts), and booleans. For example, a student might practice working with numbers by writing a function that, given a radius, computes the area of a circle. Implementing this function only requires the use of numbers.

For tables, I want to propose a pedagogic ordering that students learn, practice, and internalize as they learn to work with tabular data of increasing complexity.

@subsubsection{Understanding the Table}

In Racket, I have implemented a library for working with tabular data called @racket[tbl]. This choice is because, in no small part, because the word ``table'' was taken by another library.

To begin using example data from the library, a programmer begins by importing the @racket[tbl] library, then importing the examples.

@(linebreak)
@racket[(require tbl tbl/examples)]
@(linebreak)

The @racket[require] form loads libraries of code in Racket. Once the library is loaded, students can begin working with any number of exemplar datasets. For example, we can look at the PM10 air quality data from the state of Maine in 2018. The first thing a student should learn to do is to do some basic exploration of the data table, perhaps beginning with checking the table for how many rows and columns the data has.

@examples[#:eval ev
          #:hidden
          (require (rename-in tbl
                              [show-tbl st])
                   tbl/examples)]

@examples[#:eval ev
          #:label #f
          (check-tbl aq-maine-2018-T)
          (count-rows aq-maine-2018-T)
          (count-columns aq-maine-2018-T)]

The first function students should learn is @racket[check-tbl]. This makes sure that there are no missing values in the table, and the data in every column is of the correct type. For example, it makes sure that if a column claims to contain integers, that all of the data in that column actually conforms to the type expectations. If everything checks, the function returns @racket[true]. If anything is amiss, errors are reported, and the function returns @racket[false]. After that, it is good to know how many rows and columns are in the table.

A student may want to just @italic{see} the table, before they go any further.

@examples[#:eval ev
          #:hidden
          (define show-tbl
            (lambda (T) (st T #:rows 3 #:cols 5)))]

@(require scribble-abbrevs/latex)
@examples[#:eval ev
          #:label #f
          (show-tbl aq-maine-2018-T)]

Here, I've truncated the table for display in a PDF. Along with a way to see the table's contents, many languages and environments that work with tabular data allow you to get a summary of the table. @racket[tbl] provides the @racket[summary] function for this purpose. Although some languages provide a very terse output form, the assumption is that we are working with novices, and allowing space for the information to breathe can be useful as they attempt to make sense of everything.

There are two forms of summary: one that only takes the name of the table (and outputs a summary for every column), and one that takes both the table and a column name, allowing the student to be specific about what column they want a summary of. Here, I demonstrate a summary of a string column and an integer column, primarily because the full summary of 18 columns is multiple pages long.


@examples[#:eval ev
          #:label #f
          (summary aq-maine-2018-T "SiteName")
          (summary aq-maine-2018-T "DailyMeanPM10Concentration")]

@(exact "\\newpage")
@subsection{Visualizing the Table}

The next thing we might want students to do is to develop a quick understanding of how some of the values in the table relate to each-other. There are several visualization tools included in the @racket[tbl] library for this purpose.

@examples[#:eval ev
          #:hidden
          (require tbl/plot)
          (set-plot-parameter 'width 120)
          (set-plot-parameter 'height 120)
          (plot-file (scatter-renderer aq-maine-2018-T
                                       "SITE_LONGITUDE"
                                       "SITE_LATITUDE"
                                       )
                     "aq-maine-2018.pdf" 'pdf)
          ]
          

@(racketblock
  (require tbl/plot)
  (scatterplot aq-maine-2018-T
               "SITE_LONGITUDE"
               "SITE_LATITUDE"
               )
  )

@(centered
  (image "aq-maine-2018.pdf" #:scale 0.8))

If one is familiar with the coast of Maine, it becomes (mostly) apparent that the latitude and longitude points (mostly) represent population centers in the state. I can also generate a histogram of PM10 counts for a given site.

@examples[#:eval ev
          #:hidden
          (define my-label-hash
            (hash 1 "AA"
                  10 "BB"))
          (set-plot-parameter 'width 120)
          (set-plot-parameter 'height 120)
          (define riversideT (filter-rows aq-maine-2018-T (= SiteName "RIVERSIDE")))
          (parameterize ([plot-x-tick-label-anchor  'top-right]
                         [plot-x-tick-label-angle   30])
            (plot-file (list (histogram-renderer
                              riversideT
                              "Date"
                              "DailyMeanPM10Concentration"
                              )
                             )
                        
                       "riverside-hist.pdf" 'pdf
                       ))
          ]

@(racketblock
  (define riversideT (filter-rows aq-maine-2018-T
                                  (= SiteName "RIVERSIDE")))
  (histogram riversideT "Date" "DailyMeanPM10Concentration")
  )

@(centered
  (image "riverside-hist.pdf" #:scale 0.8))


Using the column names, I can even generate a list of histograms for every site. To format for PDF, a bit more work had to be done, but in DrRacket, it would look like:

@(racketblock
  (define sites
    (unique (get-column aq-maine-2018-T "SiteName")))
  (for/list ([site sites])
    (histogram (filter-rows aq-maine-2018-T (= SiteName ,site))
               "Date" "DailyMeanPM10Concentration")))

@examples[#:eval ev
          #:hidden
          (define sites
            (unique (get-column aq-maine-2018-T "SiteName")))
          (set-plot-parameter 'width 120)
          (set-plot-parameter 'height 120)
          (for ([site sites]
                [ndx (length sites)])
            (define filteredT (filter-rows aq-maine-2018-T
                                           (= SiteName ,site)))
            (parameterize ([plot-x-tick-label-anchor  'top-right]
                           [plot-x-tick-label-angle   30])
              (plot-file (list (histogram-renderer
                                filteredT
                                "Date"
                                "DailyMeanPM10Concentration"
                                )
                               )
                         #:title site
                         (format "pm10-~a.pdf" ndx) 'pdf
                         )))
          ]

@tabular[#:sep @hspace[1]
         (list (for/list ([ndx (in-range 0 3)])
                 (image (format "pm10-~a.pdf" ndx) #:scale 0.3))
               (for/list ([ndx (in-range 3 6)])
                 (image (format "pm10-~a.pdf" ndx) #:scale 0.3))
               (append
                (for/list ([ndx (in-range 6 8)])
                  (image (format "pm10-~a.pdf" ndx) #:scale 0.3))
                (list "")))]

This trick of rendering multiple histograms from (in this case) the unique values in a single column is probably not something a novice programmer would do. However, it is not out of the realm of possibility for a student in their first semester to get to this point, depending on the content, outcomes, and goals for the course.

At this point, I have provided a ``sneak peek'' into some of the operations that can be carried out on data. This was necessary in order to (for example) work with a subset of the data in the table.

@subsection{Operations on Data}

The goal of introducing students to working with data should not be to introduce them to a particular syntax or language. For eaxmple, we could argue that SQL is the most-used language on the planet for working with data; this does not mean that a student needs to see SQL in the second week of learning to program. For this reason, I am suggesting that a functional approach---meaning function-oriented---approach to working with data is adviseable. In this way, students need to learn 1) sequencing (the idea that one thing happens after another), and 2) that functions operate on parameters and return a value as a result, and 3) the idea that functions sometimes modify the world around them (or, that functions can be @italic{side-effecting} as part of their execution).

Students working with data most likely need to develop proficiency with the following operations  (at some point):

@itemlist[ @item{@bold{Creation of data.} Students need to be able to bring data into existence, perhaps by loading it from a file or a URL.}
          @item{@bold{Filtering rows.} Students need to be able to rduce a set of data based on a query, resulting in a new (sometimes smaller) set of data.}
          @item{@bold{Selecting columns.} Students may want to select one or more columns of data, and in doing so, create a smaller set of data to work with.}
          @item{@bold{Add rows to the data.} A student may want to add new rows to the data.}
          @item{@bold{Add (computed) columns to the data.} Given information in any one row, a student may want to compute a new value, thus extending the data set.}
          @item{@bold{Combine or join data.} Given two data sets, a student may want to join those data sets together.}
          @item{@bold{Reshape the data.} It may be, as a student grows more advanced, that they wish to radically reshape the data in the table.}
          ]

In the context of learning to program and work with data, there are natural connections between these operations that suggest a pedagogic ordering.

Once students learn how to use functions and author conditional expressions (eg. @racket[if]), they are able to read in data and do conditional filtering on a data table. This is in keeping with a typical pedagogic ordering in The @racket[tbl] library provides functions for reading data from CSV files locally and on the WWW (@racket[read-csv]), as well as directly from Google Sheets (@racket[read-gsheet]). Here, we are pulling a set of data describing the location of cities around the US directly from a Google Spreadsheet that has been shared to the WWW as a CSV. The function @racket[read-gsheet] handles following the shortlink, the parsing of the CSV file (it is assumed CSV files have header rows unless otherwise specified), and bundles up the data into a @racket[tbl] structure for exploration and analysis.

@examples[#:eval ev
          #:label #f
          (define T (read-gsheet "http://bit.ly/cities-csv"))
          (count-rows T)
          (count-columns T)
          (column-names T)
          ]

The function @racket[pick] allows a student to extract one or more columns from a table, and get a new, smaller table. If I only want the latitude, longitude, city name, and state, that would look like:

@examples[#:eval ev
          #:hidden
          (require rackunit)]

@examples[#:eval ev
          #:label #f
          (define smallT (pick T "LonD" "LatD" "City" "State"))
          (check-equal? (count-rows smallT) 128)
          (check-equal? (count-columns smallT) 4)
          ]

Again, I'm using the row/column counts as a quick sanity check to see that my intended operation does what I expected. I can also use some functions that actually check that the outputs are exactly what I expect, in keeping with the ``functional tests'' step of the design recipe presented at the beginning of this document. If everything is equal, the @racket[check-equal?] function will say nothing; if the values are unequal, a big red error is presented so I know that something does not match up.

I can extract specific columns from tables, and ask some simple questions about the values in those columns. For example, I can find out how many unique states are in the column ``State'' by @racket[pull]ing a specific column from the table as a list of values, and asking what the unique values in that list are.

@examples[#:eval ev
          #:label #f
          (unique (pull smallT "State"))
          (length (unique (pull smallT "State")))]

The @racket[pull] function introduces a function that extracts a single column as a list of values. The @racket[filter-rows] function yields a new table (just like @racket[pick]) and allows a student to express a query as a boolean expression. When writing the boolean expression, any variables that exactly match the column names in the table will be replaced with actual values from each row when the query is executed. For example, it is possible to get a table containing only the cities in the state of Ohio.

@examples[#:eval ev
          #:label #f
          (define ohioT
            (filter-rows T (= State "OH")))
          (count-rows ohioT)
          (pull ohioT "City")
          ]

Given that there are only six cities in this data in Ohio, we can quickly see the names of those six cities.

Working with data provides more opportunities for students to practice the design recipe in writing functions of their own; this is especially true of functions that operate on lists of data. The notions of applying a function to every member of a list and producing a new list (a @racket[map]) or reducing the size of a list based on some predicate (a @racket[filter]) are deeply ingrained in functional programming, and are excellent practice problems for students. For example, we could ask a student to find all of the states in Ohio that begin with the letter S.

@examples[#:eval ev
          #:label #f
          (define (begins-with los letter)
            (cond
              [(empty? los) empty]
              [else
               (cond
                 [(equal? (string-ref (first los) 0) letter)
                  (cons (first los)
                        (begins-with (rest los) letter))]
                 [else
                  (begins-with (rest los) letter)])]
              ))
          
          (begins-with (pull ohioT "City") #\S)]

This problem is not the first list recursion that a student would write, but it is one that should become second nature when following the design recipe: a list of strings is either empty, or it contains a string followed by a list of strings. Because of the problem description, we have two possibilities: the first element might begin with the letter S, or it might not. It also assumes that students were introduced to string manipulation functions like @racket[string-ref], which consume a string and return a single character.

The same effect could be had by having a student write a predicate in Racket, and then use the built-in @racket[filter] function.

@examples[#:eval ev
          #:label #f
          (define (starts-with-S? str)
            (equal? (string-ref str 0) #\S))
          (filter starts-with-S? (pull ohioT "City"))]

Using @racket[map], a student can lowercase an entire column.

@examples[#:eval ev
          #:label #f
          (map string-downcase (pull ohioT "City"))]

And, perhaps to put an end to this exploration, a student can compute a new column from data in other columns. I will create a new column for city names where all of the data is lowercased.

@examples[#:eval ev
          #:label #f
          (quietly
            (add-column ohioT "Cities" Text)
            (compute ohioT "Cities" (function (City)
                                      (string-downcase City)))
            (remove-column ohioT "City"))
          (count-columns ohioT)
          (pull ohioT "Cities")]

I first add a new column to the table. Then, I compute a function that takes in the value of the City column (one at a time), downcase that city name, and store it back into the @racketidfont{"Cities"} column. Finally, I remove the "City" column (so that we get rid of the uppercased cities), count how many columns I have (there should be 10 still), and check finally that the cities have all been downcased in the table. The @racket[quietly] form suppresses any printing or return values from the functions that are wrapped in it.

@section{A Pedagogic Ordering}

The proceeding sections demonstrate a small number of operations on tabular data with a consistent syntax. The goal in the design of the @racket[tbl] library is to provide beginners with a consistent interface for working with the core concepts of data manipulation in a way that 1) integrates with an ordered approach to learning to design programs while 2) learning to work with tabular data. At some level, the @racket[tbl] library attempts to keep simple things simple, but to allow for pathways to more complex operations if necessary. There will always be operations that the library is not capable of supporting, either because of the intentional limitations of its design, or because of the choice of underlying data representations.

The @italic{How to Design Programs} text introduces students to the design of functions early, and focuses on operations over what the authors refer to as @italic{atomic} data. This means functions that operate on arguments that have no internal structure---data like the number @racket[5], or the string @racket["Hello"]. Students are then introduced to the notion of @italic{records} or @italic{structures}, which are representations of data that have internal structure of finite extent. For example, a Cartesian point with an @italic{x} and @italic{y} component would be an example of data in this class. The definition of such structs introduces a number of procedures into the environment, including predicates to test if something is an entity of that type, as well as tools to deconstruct these structures.

@examples[#:eval ev
          #:label #f
          (struct point (x y))
          (define pt1 (point 3 5))
          (define pt2 (point 0 0))
          (point? pt1)
          (point? "3, 5")
          (point-x pt1)
          (point-y pt2)]

At this point, a student is ready to think about data that has structure of finite extent. A table becomes a structure that has rows and columns. Although every table has a different number of rows and columns, a single row looks a great deal like a @racket[struct], because it has a fixed number of fields. The @racket[get-row] function takes a table and a row number, returning htat row. The @racket[lookup] function lets a student look values up in a row of a table by the name of the column.

@examples[#:eval ev
          #:label #f
          (define r1 (get-row ohioT 0))
          r1
          (lookup ohioT r1 "Cities")]

This allows students to begin writing simple functions that give names to useful data accessors, helping them think about what they might need to solve a problem as well as how to deconstruct the data in a table.

@examples[#:eval ev
          #:label #f
          (define (get-city T r)
            (lookup T (get-row T r) "Cities"))
          (get-city ohioT 0)
          (get-city ohioT 1)
          ]

Students are also able to operate on lists of data without having to write loops. Given functions that produce lists, they can take those outputs and pass them to other functions that consume lists. 

@examples[#:eval ev
          #:label #f
          (define city-lats (pull ohioT "LatD"))
          (stddev city-lats)
          (mean city-lats)
          ]

Learning to manipulate lists of data is a critical point of learning; progression on to trees, graphs, and other complex linked structures follow a common pattern of recognizing the structure of the data and writing code that follows that structure.

For example, we might ask students to write a function that, given a list of strings, it returns the longest string. This could be written as a list, or in the accumulator-passing style (which can always be unrolled to a loop).

The first implementation follows the naturally recursive definition (or structural definition) of a list.

@examples[#:eval ev
          #:label #f
          (define (find-longest los s)
            (cond
              [(empty? los) s]
              [(> (string-length (first los))
                  (string-length s))
               (find-longest (rest los) (first los))]
              [else
               (find-longest (rest los) s)]))
          (find-longest (pull T "City") "")
          (find-longest (pull ohioT "Cities") "")
          ]

This second example models how a Python programmer would write the same function.

@examples[#:eval ev
          #:label #f
          (define (find-longest los longest)
            (for ([s los])
              (when (> (string-length s)
                       (string-length longest))
                (set! longest s)))
            longest)
          (find-longest (pull T "City") "")
          (find-longest (pull ohioT "Cities") "")
          ]

The first can be taught to students by following the design recipe, defining the structure of the list, and then following the template for writing code that operates on lists. The second does nothing to acknowledge the structure of the data, and requires a student to mutate a variable, relying on changing the state of the environment to determine which string in a list of strings is the longest. For a theory that might provide guidance for the teaching of the second, teaching students to think about the @italic{roles} of variables may be useful (see @(hyperlink "http://www.cs.joensuu.fi/~saja/var_roles/stud_vers/stud_Python_eng.html"  "Roles of Variables") for more).
