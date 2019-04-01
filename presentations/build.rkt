#lang at-exp slideshow/widescreen
(require slideshow/text
         (only-in scribble/base centered)
         slideshow/code
         pict)

(current-main-font "Cabin Condensed Medium")
(current-font-size 36)
(current-titlet
 (lambda (s)
   (colorize (text s (current-main-font) 64)
             (current-title-color))))

(slide #:title "Building Worlds and Tools For Their Study"
       #:layout 'center
       (t (format "~a ~a~a" "(or," @italic{Teaching Programming for Learning} ")"))
       (t "Matt Jadud")
       (t "Sometime 2019"))

(slide #:title "Outline"
       #:layout 'top
       ;;(item "Goals")
       (item "Worlds")
       (item "The Toolkit")
       (item "Tools")
       (item "Questions"))

(slide #:title "Microworlds"
       #:layout 'top
       @para{In computing, we have a notion of @it{microworlds}.}
       @para{These are simplified, simulated universes.}
       (scale (bitmap "paddle-boids.png") 0.25)
       )

(slide #:title "Paddle: Boids"
       (scale (bitmap "paddle-boids.png") 0.25))

(require ppict/2 ppict/slideshow2)

(define (four-way title pat n
                  #:sc [sc 0.2]
                  #:clickback [cb (λ () 'pass)])
  (pslide #:title title
          #:go (coord 0.15 0.3 'lc)
          (clickback (scale (bitmap (format pat n)) sc)
                     cb)
          #:go (coord 0.6 0.3 'lc)
          (scale (bitmap (format pat (- n 1))) sc)
          #:go (coord 0.15 0.75 'lc)
          (scale (bitmap (format pat (- n 2))) sc)
          #:go (coord 0.6 0.75 'lc)
          (scale (bitmap (format pat (- n 3))) sc)))

(four-way "Paddle: Boids"     "boids-~a.png"     4 #:sc 0.15)
(four-way "Paddle: Termites"  "termites-~a.png"  4)
(four-way "Paddle: Diffusion" "diffusion-~a.png" 4 #:sc 0.15)

