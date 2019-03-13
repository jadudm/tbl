#lang racket

#|
Say the top of your tree is /path/to/project.

Once:

  raco pkg install /path/to/project

Thereafter your "make" is:

  raco setup --pkgs project
|#

(require "tbl.rkt")
(provide (all-from-out
          "tbl.rkt"))
