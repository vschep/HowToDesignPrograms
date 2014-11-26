;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname MovingCar) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")))))
(define WIDTH-OF-WORLD 200)
(define HEIGHT-OF-WORLD 50)

; a tree for the background
(define tree
  (overlay/xy (circle 10 'solid 'green)
               9 15
               (rectangle 2 20 'solid 'brown)))
; the background
(define BACKGROUND (place-image/align tree WIDTH-OF-WORLD HEIGHT-OF-WORLD "right" "bottom"
                                      (empty-scene WIDTH-OF-WORLD HEIGHT-OF-WORLD)))

; car definition
; scale the car image by changing the WHEEL-RADIUS
(define WHEEL-RADIUS 5)
(define WHEEL-DISTANCE (* WHEEL-RADIUS 5))
(define WHEEL (circle WHEEL-RADIUS "solid" "black"))
(define SPACE (rectangle (- WHEEL-DISTANCE (* WHEEL-RADIUS 2)) 0 "solid" "white"))
(define BOTH-WHEELS (beside WHEEL SPACE WHEEL))
(define BODY (rectangle (+ WHEEL-DISTANCE (* WHEEL-RADIUS 4)) (* WHEEL-RADIUS 2) "solid" "red"))
(define BODY-WITH-WHEELS (overlay/offset BOTH-WHEELS 0 (- WHEEL-RADIUS) BODY))
(define CABIN (rectangle WHEEL-DISTANCE WHEEL-RADIUS "solid" "red"))
; the CAR
(define CAR (above CABIN BODY-WITH-WHEELS))

(define SWING-HEIGHT 4)
; the y coordinate of the CAR in the scene
(define (y-car ws)
  (-
   (- HEIGHT-OF-WORLD (/ (image-height CAR) 2))
   (+ 
    (* (sin (/ ws 10)) SWING-HEIGHT)
    SWING-HEIGHT)))

; WorldState -> WorldState
; launches the program from some initial state
(define (main ws)
  (big-bang ws
            [on-tick tock]
            [to-draw render]
            [stop-when end?]
            [on-mouse hyper]))

; WorldState -> WorldState
; moves the car by three pixels every time the clock ticks
(define (tock ws) 
  (+ ws 3))
; tests
(check-expect (tock 20) 23)
(check-expect (tock 78) 81)

; WorldState Number Number String -> WorldState
; places the car at position (x,y) 
; if the mouse event is "button-down" 
(define (hyper x-position-of-car x-mouse y-mouse me)
  (cond
    [(string=? "button-down" me) x-mouse]
    [else x-position-of-car]))
; tests
(check-expect (hyper 21 10 20 "enter") 21)
(check-expect (hyper 42 10 20 "button-down") 10)

; WorldState -> boolean
; ends the world
(define (end? ws)
  (>= ws
      (+ WIDTH-OF-WORLD 
         (/ (image-width CAR) 2))))
; tests
(check-expect (end? (+ WIDTH-OF-WORLD 
                       (/ (image-width CAR) 2)))
              true)
(check-expect (end? (- (+ WIDTH-OF-WORLD 
                          (/ (image-width CAR) 2))
                       1))
              false)

; WorldState -> Image
; places the car into a scene according to the given world state
(define (render ws)
  (place-image CAR ws (y-car ws) BACKGROUND))