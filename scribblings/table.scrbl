#lang scribble/doc
@(require scribble/manual
          scribble/struct
          scribble/eval
          "config.rkt")

@title{tbl: Data Tables for Peoples}
@author[@author+email["Matt Jadud" "matt@jadud.com"]]

@section-index["tbl"]

@centered{@bold{@italic{A simplified interface for working with tabular data.}}}

@defmodule[tbl]

I wrote the table library to support first-year students in exploring data in Racket. May of the design decisions, therefore, were made with a mind towards the teaching and learning of programming (in the spirit of @italic{How to Design Programs}).

table was written with consideration for how R scripts typically interact with tabular data (in particular, the tidyverse and dplyr libraries), exploration of Pyret's data tables, as well as texts and literature on exploratory data analysis.

