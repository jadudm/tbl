#lang racket

#|
Say the top of your tree is /path/to/project.

Once:

  raco pkg install /path/to/project

Thereafter your "make" is:

  raco setup --pkgs project
|#

(require "table.rkt")
(provide (all-from-out
          "table.rkt"))
