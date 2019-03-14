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

