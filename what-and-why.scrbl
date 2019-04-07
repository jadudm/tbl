#lang racket

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
    
@title[#:tag "what-and-why"]{The What and Why of @racket[tbl]}

This section assumes you are new to the analsys of data. 
