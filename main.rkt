#lang racket

#|
Say the top of your tree is /path/to/project.

Once:

  raco pkg install /path/to/project

Thereafter your "make" is:

  raco setup --pkgs project
|#

(require "basics.rkt"
         "operations.rkt"
         "reading/csv.rkt"

         "types.rkt"
         
         "test/files.rkt"
         
         "util/lists.rkt"

         "analysis/data-science-wrapper.rkt"
         
         math/statistics
         )
(provide (all-from-out
          "basics.rkt"
          "operations.rkt"
          "reading/csv.rkt"
          "types.rkt"
          "test/files.rkt"
          
          "util/lists.rkt"

          "analysis/data-science-wrapper.rkt"

          math/statistics
          ))
