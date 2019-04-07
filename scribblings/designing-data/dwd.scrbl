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

 In adhering to these principles, I am attempting to lay a foundation for learners that lets them transition from writing run-once, throw-away scripts that work with a data table into learners who are prepared to engage with the process of designing and building the systems of abstractions that are necessary to create new tools and software.

}

@section[#:tag "foundations"]{Foundations}

There are principles we can adhere to when introducing students to the practices of working with data. Those principles are inspired by scholarship and best practices in the teaching and learning of programming.

The first three principles I will introduce are:

@itemlist[
 @item{Start with small data that fits on a single screen.}
 @item{Never begin with a blank page.}
 @item{Always work from correct and complete data.}
 ]

These are in contrast to how students are often introduced to working with data. First, students are often expected to begin with a CSV file, and begin writing code to import and clean that data, as opposed to being provided with scaffolding (early) that models the patterns necessary for working with data. The pattern of importing and establishing the prima facie correctness of small data sets should be modeled at least a dozen times, so that students can establish the pattern.

The data that students work with should fit on a screen. For example, one data set provided by the @racket[tbl] library as an exemplar explores the passengers on the Titanic. There were over 1000 passengers on the Titanic; even though there are ``only'' four columns (class, age, sex, and survival), there are too many rows to take in at a glance. For this reason, a compressed table of 14 rows is provided.

@examples[#:eval ev
          #:hidden
          (require tbl/examples/titanic)
          (require (rename-in tbl (show-full-tbl-pict show-full-tbl)))]


@examples[#:eval ev
          #:label #f
          (show-full-tbl tiny-titanicT)]

Data is presented, often in arbitrary form and of arbitrarily large extent, and students are introduced to the process of cleaning or tidying this data. While this is a critical process, it is also an @italic{expert} process. It presumes experience that allows the data analyst to recognize what needs to be done, in what order, and with what checks to assure that the specified transformations took place correctly. The last part of that process---a systematic approach to checking that transformations carried out are correct---is often absent from the popular dialogue.

For this reason, a student should be started with a file (notebook, or whatever the environment calls its space for authoring code) that is not empty. The code that it is in the file should 1) load the data and 2) perform a series of checks on the data. Those checks should communicate useful information to the learner. For example, an initial program with the Titanic data might look like the follwoing:

@#reader scribble/comment-reader
(racketblock
;; Load both the tbl library and the
;; Titanic example table.
(require tbl tbl/examples/titanic)

;; This table should have fourteen rows.
;; If it doesn't, the next line will fail.
(check-equal? (count-rows tiny-titanicT) 14)

;; There should be five columns.
(check-equal? (count-columns tiny-titanicT) 5)
;; The five columns should all exist and have these names.
(check-has-columns? tiny-titanicT
                    "class" "age" "sex" "survive" "total")

;; Show the table.
(show-full-tbl tiny-titanicT)
)

When a student runs this code, it should ``just work,'' and it should look something like this:

@examples[#:eval ev
          #:no-prompt
          #:label #f
          (require tbl tbl/examples/titanic)
          (check-row-count? tiny-titanicT 14)
          (check-column-count? tiny-titanicT 5)
          (check-has-columns? tiny-titanicT
                              "class" "age" "sex" "survive" "total")
          (show-full-tbl tiny-titanicT)
          ]

As students begin working with other data sets, it should be the case that the amount of template provided begins to go away, and instead they are asked to begin following a data analysis recipe. The first maxim of their data analysis design recipe might look like:


@itemlist[
 @item{Check dimensions and contents, and visually confirm your checks.}]

This maxim should help them remember that they should check the number of rows and columns in their data set, assert what those values should be (in case the data is changed at some point in the future), and confirm that the names of the columns of their data are what they expect those columns to be. The purpose of these habits are to provide them with tools to develop consistent analyses when working in groups with other students, where someone might decide to edit some data (perhaps manually removing one or more rows or columns), rename a column, or otherwise make changes to the source data that could conceivably break the analysis that the students had developed as a group. These checks provide a minimal ``sanity check'' that the data coming in to their analysis is what they are expecting.

In this way, my first three principles of teaching with data are served: we have small data (that fits on a single screen or page), students are not started with a blank page, and the data is complete and correct (and the students are taught, through a standard template and practice, to always be able to assert that their data is complete and correct).

@section[#:tag "explorations"]{Exploration of Data}

The reason we organize information into a tabular form is often to assist with inquiry and discovery. This might be counting, or it might be modeling; regardless, this is work that is often carried out on data that has been ``cleaned'' of entries that are not, for some reason, germaine to the analysis being carried out. To facilitate learning to think about the kinds of operations that can be carried out on data, and the results of those operations, it is best for students to learn those operations on well-formed, small data sets.

The next FIXME principles that I want to adhere to are the following:

@itemlist[
 @item{Students should learn functional operations on data before sequential/stateful operations.}
 @item{Students should hypothesize the outcomes of operations on data, state their query in plain language, and then encode those outcomes as examples before beginning their work.}
 @item{Operations on data should verified, and be verifiable by hand.}
 @item{It should be assumed that students may have common-sense understandings of operations on data, but no prior/actual experience reasoning about or encoding those operations.}
 ]

Functional operations on data do not modify the underlying data table. The immutability of the data table provides students with a fixedpoint in their reasoning. If a student's explorations begin with a data table that has 10 rows, then the operations that they carry out on that table will not change the original table in any way---it began with, and will continue to have, 10 rows of data. While it is true that we can imagine, and ultimately carry out, operations that transform the data table, we do not begin with these operations, as they introduce uncertainty in the student's mental model of the data early, potentially before they even have a fully-formed model for the data.

By working with small tables, students are able to verify their work. If the source table has 10 rows, and one of the columns has either value ``A'' or value ``B'', it is possible for a student to assert that a query that asks for ``only items in category B'' will result in 6 rows of data. @italic{Student hypotheses should be encoded as examples before they write the query.} This serves multiple purposes, but the most important may be that it forces the student to understand their data, to develop a mental model of what the outcome of a query will be, and to express that model in an executable and testable form before they write a single line of code. 
 

Once a student has encoded their testable hypotheses, it is possible for the student to quickly verify if their encoding of a query statement has correctly extracted all of the elements of category ``B''. While the idea of saying that a table of data contains ``some things in category A, and some in category B'' is easily understood from a ``common-sense'' perspective, that does not mean that students necessarily know how to transform those queries into boolean expressions that can be carried out, stepwise, on every row of a dataset (and, in doing so, produce a new data set as a result).

The act of running their code now becomes an act of hypothesis testing. The student has read their data, expressed what they want to accomplish with a query or other operation on the data, encoded the expected outcome, @italic{then} written the code, and finally tested their hypothesis by executing their code. I will demonstrate this with the Titanic data.

I believe there are only 46 children who survived the Titanic's sinking. By looking at the data, I can add up 5, 11, 13, 13, and 14, and come to that conclusion by hand. Therefore, I can plan on having a variable called @racketidfont{num-kids-survived} that I expect to be the value 46. I would begin by adding a comment that expresses my hypothesis, and then write the example that will test that hypothesis.


@examples[#:eval ev
          #:no-prompt
          #:label #f
          (require tbl tbl/examples/titanic)
          (check-row-count? tiny-titanicT 14)
          (check-column-count? tiny-titanicT 5)
          (check-has-columns? tiny-titanicT
                              "class" "age" "sex" "survive" "total")
          ; HYPOTHESIS
          ; There are only 46 children who survived the sinking
          ; of the Titanic.
          ; OPERATION
          ; I will do a filtering operation, and get only
          ; the rows where 'age' is 'child'. Then, I will
          ; add up the values in the column 'survive'.

          ; TESTS
          ; (check-equal? num-kids-survived 46)
                                                                
          ]

Once the student has stated their hypothesis and the operation(s) they wish to undertake, they are ready to carry out those operations.

In the @italic{How to Design Programs} design recipe, it is important for students to think about the kinds, or types, of information that go into and are produced by functions. In this regard, I imagine students will be encouraged to think the same way. However, what is important is that initial learning of work on tables will only involve three kinds of functions:

@itemlist[
 @item{Functions that consume a @racket[tbl] and produce a @racket[tbl].}
 @item{Functions that consume a @racket[tbl] and produce a list.}
 @item{Functions that consume a list and produce a value.}]

In this regard, the students are encouraged to think about filtering queries (that reduce the number of rows or columns in the table), in terms of functions that extract a single column of data from a table (possibly the original table, or possibly a reduced table), and finally, operations that reduce a list from (say) a set of numbers to a sum, an average, or some other singleton value.

To determine how many children survived the Titanic, I can now engage in these operations. Note that I will apply previous principles along the way; for example, I know there should be six rows in this resulting table, so I will make sure that is true after my filtering operation.


@examples[#:eval ev
          #:no-prompt
          #:label #f
          (require tbl tbl/examples/titanic rackunit)
          (check-row-count? tiny-titanicT 14)
          (check-column-count? tiny-titanicT 5)
          (check-has-columns? tiny-titanicT
                              "class" "age" "sex" "survive" "total")
          ; HYPOTHESIS
          ; There are only 46 children who survived the sinking
          ; of the Titanic.
          ; OPERATION
          ; I will do a filtering operation, and get only
          ; the rows where 'age' is 'child'. Then, I will
          ; add up the values in the column 'survive'.
          (define just-kidsT
            (filter-rows tiny-titanicT (= age "child")))
          (check-row-count? just-kidsT 6)

          (define num-kids-survived
            (sum (pull just-kidsT "survive")))
          
          ; This new table should have only 
          ; TESTS
          (check-equal? num-kids-survived 46)
                                                                
          ]

Now, that's interesting! I thought only 46 kids survived! That is probably because I was distracted when I wrote this, and then running the code demonstrated to me that I was wrong. After going back, it turns out that @racket[(+ 5 1 11 13 13 14)] is 57, not 46. I'll modify that, and re-run my analysis.


@examples[#:eval ev
          #:no-prompt
          #:label #f
          (require tbl tbl/examples/titanic rackunit)
          (check-row-count? tiny-titanicT 14)
          (check-column-count? tiny-titanicT 5)
          (check-has-columns? tiny-titanicT
                              "class" "age" "sex" "survive" "total")
          ; HYPOTHESIS
          ; There are only 46 children who survived the sinking
          ; of the Titanic.
          ; OPERATION
          ; I will do a filtering operation, and get only
          ; the rows where 'age' is 'child'. Then, I will
          ; add up the values in the column 'survive'.
          (define just-kidsT
            (filter-rows tiny-titanicT (= age "child")))
          (check-row-count? just-kidsT 6)

          (define num-kids-survived
            (sum (pull just-kidsT "survive")))
          
          ; This new table should have only 
          ; TESTS
          (check-equal? num-kids-survived 57)
                                                                
          ]

This example showed three functions in the @racket[tbl] library: @racket[filter-rows], @racket[pull], and @racket[sum]. The first, @racket[filter-rows], take a table and a boolean expression, and returns a new table containing only rows that conform to that expression. When evaluating the expression, and identifiers that are the same as the names of the columns in the table will be replaced in the query by values from the table. The second function, @racket[pull], extracts a single column from the table. Finally, @racket[sum] is one of a number of functions provided by @racket[tbl] that take a list and compute an output given that list.

In terms of learning to manipulate structured data, this an excellent point for students to begin learning how to write functions that implement the @italic{How to Design Programs} design recipe for lists. Functions that reduce lists, filter lists, or generate new lists (possibly modifying values in one list and producing another list as output) are all natural programs to design and write at this stage.

