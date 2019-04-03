#lang scribble/manual
@(require scribble/struct
          scribble/eval
          "config.rkt")

@title{tbl: A Simplified Interface for Tabular Data}
@author[@author+email["Matt Jadud" "matt@jadud.com"]]

@section-index["tbl"]

@defmodule[tbl]

I wrote the tbl library so that students new to programming would have a principled introduction to the analysis of data. May of the design decisions, therefore, were made with a mind towards the teaching and learning of programming (in the spirit of @italic{How to Design Programs}).

tbl was written with consideration for how R scripts typically interact with tabular data (in particular, the tidyverse and dplyr libraries), exploration of Pyret's data tables, as well as texts and literature on exploratory data analysis.


@include-section["creating.scrbl"]

@;@include-section["exploring-data-with-tbl.scrbl"]
@;@include-section["using-tbl.scrbl"]