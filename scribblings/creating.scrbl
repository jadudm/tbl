#lang scribble/manual
@(require tbl
          scribble/example
          racket/sandbox
          racket/bool
          racket/runtime-path
          "config.rkt"
          (for-label tbl))

@(define ev
   (call-with-trusted-sandbox-configuration
    (λ ()
      (parameterize ([sandbox-output 'string]
                     [sandbox-error-output 'string]
                     [sandbox-memory-limit 4096])
        (make-evaluator 'racket
                        #:requires '(plot/pict plot/utils))
        ))))
    
@title[#:tag "creating"]{Creating Data}

It is possible to create a @racket[tbl] from scratch. More likely, data will be loaded from some external source.

@;{FIXME: This has a richer interface than documented.}
@defproc[(read-csv
          [filename-or-url string?]
          [#:header-row? header-row? boolean? true])
          tbl?]{Reads a CSV file, either locally or from a remote URL, and returns a @racket[tbl].}

@examples[#:eval ev
          #:label #f
          (require tbl)
          (define citiesT (read-csv "https://think.cs.vt.edu/corgis/csv/graduates/graduates.csv?forcedownload=1"))
          (show-tbl-pict citiesT #:columns 5)]