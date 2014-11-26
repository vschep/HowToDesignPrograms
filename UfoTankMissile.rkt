;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname UfoTankMissile) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")))))
; constants: 
(define WIDTH 400)
(define HEIGHT 200)
 
; visual constants: 
(define MT (empty-scene WIDTH HEIGHT))
(define UFO
  (overlay (circle 10 "solid" "green")
           (rectangle 40 2 "solid" "green")))
(define UFO-LAND-EDGE (- HEIGHT (/ (image-height UFO) 2)))
(define TANK-WIDTH 40)
(define TANK-HEIGHT 20)
(define TANK (rectangle TANK-WIDTH TANK-HEIGHT "solid" "brown"))
(define TANK-CENTER-X (/ TANK-WIDTH 2))
(define TANK-BOTTOM (- HEIGHT (/ TANK-HEIGHT 2)))
(define MISSILE (triangle 10 "solid" "red"))
(define REACH-DIST 5)

; A UFO is Posn. 
; interpretation (make-posn x y) is the UFO's current location 
 
(define-struct tank [loc vel])
; A Tank is (make-tank Number Number). 
; interpretation (make-tank x dx) means the tank is at position
; (x, HEIGHT) and that it moves dx pixels per clock tick 
 
; A Missile is Posn. 
; interpretation (make-posn x y) is the missile's current location 

(define-struct aim [ufo tank])
(define-struct fired [ufo tank missile])

; A SIGS is one of: 
; – (make-aim UFO Tank)
; – (make-fired UFO Tank Missile)
; interpretation represents the state of the space invader game

; Location is Posn
; interpretation Posn are positions on the Cartesian plane

; SIGS -> Image
; adds TANK, UFO, and possibly the MISSILE to BACKGROUND
(define (si-render s)
  (cond
    [(aim? s)
     (tank-render (aim-tank s)
                  (ufo-render (aim-ufo s) MT))]
    [(fired? s)
     (tank-render (fired-tank s)
                  (ufo-render (fired-ufo s)
                              (missile-render (fired-missile s)
                                              MT)))]))

; Tank Image -> Image 
; adds t to the given image im
(check-expect (tank-render (make-tank 30 10) MT)
              (place-image TANK 40 TANK-BOTTOM MT))

(define (tank-render t im)
  (place-image TANK 
               (+ (tank-loc t) (tank-vel t))
               TANK-BOTTOM
               im))
 
; UFO Image -> Image 
; adds u to the given image im
(check-expect (ufo-render (make-posn 100 80) MT)
              (place-image UFO 100 80 MT))

(define (ufo-render u im)
  (place-image UFO (posn-x u) (posn-y u) im))

; Missile Image -> Image 
; adds m to the given image im
(check-expect (missile-render (make-posn 100 120) MT)
              (place-image MISSILE 100 120 MT))

(define (missile-render m im)
  (place-image MISSILE (posn-x m) (posn-y m) im))


; SIGS -> bool
; checks if the UFO lands or is hit by the missile
; hit examples
(check-expect (si-game-over? 
               (make-fired (make-posn 100 100)
                           (make-tank 80 10)
                           (make-posn 100 101))) 
              true)
(check-expect (si-game-over? 
               (make-fired (make-posn 100 100)
                           (make-tank 80 10)
                           (make-posn 20 120)))
              false)
(check-expect (si-game-over? 
               (make-aim (make-posn 100 100)
                         (make-tank 80 10))) 
              false)
; land examples
(check-expect (si-game-over? (make-aim (make-posn 100 0) 0)) false)
(check-expect (si-game-over? (make-aim (make-posn 100 (- UFO-LAND-EDGE 1)) 0)) false)
(check-expect (si-game-over? (make-aim (make-posn 100 UFO-LAND-EDGE) 0)) true)
(check-expect (si-game-over? (make-aim (make-posn 100 HEIGHT) 0)) true)
(check-expect (si-game-over? (make-fired (make-posn 100 UFO-LAND-EDGE) 0 (make-posn 100 UFO-LAND-EDGE)))
              true)

(define (si-game-over? s)
  (cond 
    [(aim? s) (landed? s)]
    [(fired? s) (or (landed? s) (in-reach? (fired-ufo s) (fired-missile s) REACH-DIST))]))

; Posn Posn -> bool
; determines whether the distance of location loc1 to location loc2 is strictly
; less than reach-distance
(define T-REACH-DIST 5)
(check-expect (in-reach? (make-posn 2 2) (make-posn 4 4) T-REACH-DIST) true)
(check-expect (in-reach? (make-posn 2 2) (make-posn 12 20) T-REACH-DIST) false)
(check-expect (in-reach? (make-posn 2 2) (make-posn 5 6) T-REACH-DIST) false)
(check-expect (in-reach? (make-posn 2 2) (make-posn 2 2) T-REACH-DIST) true)
(check-expect (in-reach? (make-posn 2 2) (make-posn 4 0) T-REACH-DIST) true)
(check-expect (in-reach? (make-posn 2 2) (make-posn 2 4) T-REACH-DIST) true)

(define (in-reach? loc1 loc2 reach-dist)
  (<
   (sqrt (+ 
          (sqr (- (posn-x loc1) (posn-x loc2))) 
          (sqr (- (posn-y loc1) (posn-y loc2)))))
   reach-dist))

; SIGS -> bool
; checks if the UFO has landed
(check-expect (landed? (make-aim (make-posn 100 UFO-LAND-EDGE) 0)) true)
(check-expect (landed? (make-aim (make-posn 100 HEIGHT) 0)) true)
(check-expect (landed? (make-aim (make-posn 100 (- UFO-LAND-EDGE 1)) 0)) false)
(check-expect (landed? (make-fired (make-posn 100 UFO-LAND-EDGE) 0 0)) true)
(check-expect (landed? (make-fired (make-posn 100 HEIGHT) 0 0)) true)
(check-expect (landed? (make-fired (make-posn 100 (- UFO-LAND-EDGE 1)) 0 0)) false)

(define (landed? s)
  (cond
    [(aim? s) (>= (posn-y (aim-ufo s)) UFO-LAND-EDGE)]
    [(fired? s) (>= (posn-y (fired-ufo s)) UFO-LAND-EDGE)]))

; SIGS -> Image
; renders the final scene
(define HIT-SCN (make-fired (make-posn 100 100)
                            (make-tank 80 10)
                            (make-posn 100 101)))
(check-expect (si-render-final HIT-SCN)
              (place-image (text "YOU WIN!" 24 "red")
                           (/ WIDTH 2)
                           (/ HEIGHT 2)
                           (si-render HIT-SCN)))
(define LAND-SCN (make-aim (make-posn 100 (+ UFO-LAND-EDGE 1)) (make-tank 80 10)))
(check-expect (si-render-final LAND-SCN)
              (place-image (text "YOU LOOSE!" 24 "red")
                           (/ WIDTH 2)
                           (/ HEIGHT 2)
                           (si-render LAND-SCN)))

(define (si-render-final s)
  (cond
    [(aim? s) (message "YOU LOOSE!" (si-render s))]
    [(fired? s) (if (landed? s) (message "YOU LOOSE!" (si-render s)) (message "YOU WIN!" (si-render s)))]))


; String Image -> Image
; renders msg in the middle of img
(define (message msg img)
  (place-image (text msg 24 "red") 
               (/ (image-width img) 2) 
               (/ (image-height img) 2) 
               img))

; SIGS -> SIGS
(define (si-move s)
  (si-move-proper s (create-random-number s)))


