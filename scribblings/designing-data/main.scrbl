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
          #:label #f
          (require tbl
                   tbl/examples)
          (check-tbl aq-maine-2018-T)
          (count-rows aq-maine-2018-T)
          (count-columns aq-maine-2018-T)]

The first function students should learn is @racket[check-tbl]. This makes sure that there are no missing values in the table, and the data in every column is of the correct type. For example, it makes sure that if a column claims to contain integers, that all of the data in that column actually conforms to the type expectations. If everything checks, the function returns @racket[true]. If anything is amiss, errors are reported, and the function returns @racket[false]. After that, it is good to know how many rows and columns are in the table.

Many languages and environments that work with tabular data allow you to get a summary of the table. @racket[tbl] provides the @racket[summary] function for this purpose.

@examples[#:eval ev
          #:label #f
          (summary aq-maine-2018-T)]
