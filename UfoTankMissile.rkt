;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname UfoTankMissile) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")))))
; constants: 
(define WIDTH 400)
(define HEIGHT 300)
 
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
(define REACH-DIST 10)
(define RAND-DIST 3)
(define TANK-VEL 3)
(define UFO-VEL 3)
(define MISSILE-VEL 6)

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
; move the UFO randomly
(define (si-move s)
  (si-move-proper s (create-random-number s)))

; SIGS Number -> SIGS
; move all objects according to their velocity and move the UFO horizontally to ufo-posn-x
(check-expect (si-move-proper 
               (make-aim (make-posn 100 100) (make-tank 100 TANK-VEL)) 110)
              (make-aim (make-posn 110 (+ 100 UFO-VEL)) (make-tank (+ 100 TANK-VEL) TANK-VEL)))
(check-expect (si-move-proper
               (make-fired (make-posn 100 100) 
                           (make-tank 100 TANK-VEL) 
                           (make-posn 100 150)) 110)
              (make-fired (make-posn 110 (+ 100 UFO-VEL)) 
                          (make-tank (+ 100 TANK-VEL) TANK-VEL) 
                          (make-posn 100 (- 150 MISSILE-VEL))))

(define (si-move-proper s ufo-posn-x)
  (cond
    [(aim? s) (make-aim 
               (move-ufo (aim-ufo s) ufo-posn-x) 
               (move-tank (aim-tank s)))]
    [(fired? s) (make-fired 
                 (move-ufo (fired-ufo s) ufo-posn-x) 
                 (move-tank (fired-tank s)) 
                 (move-missile (fired-missile s)))]))

; POSN Number -> POSN
; moves the UFO vertically according to its velocity and horizontally to ufo-posn-x
(define (move-ufo ufo ufo-posn-x)
  (make-posn ufo-posn-x (+ (posn-y ufo) UFO-VEL)))

; Tank -> Tank
; moves the tank according to its velocity
(define (move-tank tank)
  (make-tank (+ (tank-loc tank) (tank-vel tank)) (tank-vel tank)))

; POSN -> POSN
; moves the missile upwards according to its velocity
(define (move-missile missile)
  (make-posn (posn-x missile) (- (posn-y missile) MISSILE-VEL)))

; SIGS -> Number
; creates a random that is within the UFO posn x +- RAND-DIST
(check-within (create-random-number (make-aim (make-posn 100 0) 0)) 100 RAND-DIST)
(check-within (create-random-number (make-fired (make-posn 100 0) 0 0)) 100 RAND-DIST)
(check-within (create-random-number (make-aim (make-posn 0 0) 0)) 0 RAND-DIST)
(check-within (create-random-number (make-aim (make-posn WIDTH 0) 0)) (- WIDTH (/ RAND-DIST 2)) (/ RAND-DIST 2))
               
(define (create-random-number s)
  (limit-number
   (+ (random-sign (random RAND-DIST))
      (cond
        [(aim? s) (posn-x (aim-ufo s))]
        [(fired? s) (posn-x (fired-ufo s))]))
   0 WIDTH))

; Number -> Number
; set the sign randomly
(define (random-sign number)
  (if (= (random 2) 1) 
      (- number)
      number))

; Number Number Number -> Number
; limits value to max >= value >= min
(check-expect (limit-number 10 0 20) 10)
(check-expect (limit-number -1 0 20) 0)
(check-expect (limit-number 21 0 20) 20)

(define (limit-number value min max)
  (cond
    [(< value min) min]
    [(> value max) max]
    [else value]))

; SIGS String -> SIGS
; changes the game state according to these key events:
; - pressing the left arrow ensures that the tank moves left
; - pressing the right arrow ensures that the tank moves right
; - pressing the space bar fires the missile if it hasn’t been launched yet
(check-expect (si-control (make-aim 0 (make-tank 10 3)) "left") (make-aim 0 (make-tank 10 -3)))
(check-expect (si-control (make-aim 0 (make-tank 10 -3)) "right") (make-aim 0 (make-tank 10 3)))
(check-expect (si-control (make-aim 0 (make-tank 10 3)) "right") (make-aim 0 (make-tank 10 3)))
(check-expect (si-control (make-aim 0 (make-tank 10 -3)) "left") (make-aim 0 (make-tank 10 -3)))
(check-expect (si-control (make-fired 0 (make-tank 10 3) 0) "left") (make-fired 0 (make-tank 10 -3) 0))
(check-expect (si-control (make-aim 0 (make-tank 10 3)) " ") 
              (make-fired 0 (make-tank 10 3) (make-posn 10 HEIGHT)))
(check-expect (si-control (make-fired 0 (make-tank 10 3) (make-posn 10 0)) " ") 
              (make-fired 0 (make-tank 10 3) (make-posn 10 0)))

(define (si-control s ke)
  (cond
    [(string=? "left" ke) (ensure-dir s ke)]
    [(string=? "right" ke) (ensure-dir s ke)]
    [(string=? " " ke) (ensure-fired s)]
    [else s]))

; SIGS String -> SIGS
; ensures that that the velocity of the tank correponds to dir
(define (ensure-dir s dir)
  (make-same-with-tank s
                       (make-tank (tank-loc (aim-or-fired-tank s))
                                  (cond
                                    [(reverse-sign? (tank-vel (aim-or-fired-tank s)) dir) (- (tank-vel (aim-or-fired-tank s)))]
                                    [else (tank-vel (aim-or-fired-tank s))]))))

; Number String -> Boolean
; checks if the sign of vel shall be reversed
(check-expect (reverse-sign? 1 "left") true)
(check-expect (reverse-sign? -1 "left") false)
(check-expect (reverse-sign? -1 "right") true)
(check-expect (reverse-sign? 1 "right") false)

(define (reverse-sign? vel ke)
  (cond
    [(string=? "left" ke) (>= vel 0)]
    [(string=? "right" ke) (< vel 0)]))

; SIGS -> Tank
; get's the tank regardless if s is an Aim or a Fired
(define (aim-or-fired-tank s)
  (cond
     [(aim? s) (aim-tank s)]
     [(fired? s) (fired-tank s)]))

; SIGS Tank -> SIGS
; makes the same SIGS as aim-or-fired but with tank as tank
(define (make-same-with-tank original tank)
  (cond
    [(aim? original) (make-aim (aim-ufo original) tank)]
    [(fired? original) (make-fired (fired-ufo original) tank (fired-missile original))]))
    
; SIGS -> SIGS
; ensures that a missile is fired at the posn-x of the tank and posn-y 0
(check-expect (ensure-fired 
               (make-aim (make-posn 100 200) (make-tank 10 3)))
              (make-fired (make-posn 100 200) (make-tank 10 3) (make-posn 10 HEIGHT)))
(check-expect (ensure-fired
               (make-fired (make-posn 100 200) (make-tank 10 3) (make-posn 150 10)))
              (make-fired (make-posn 100 200) (make-tank 10 3) (make-posn 150 10)))

(define (ensure-fired s)
  (cond
    [(aim? s) (make-fired 
               (aim-ufo s) 
               (aim-tank s) 
               (make-posn (tank-loc (aim-tank s)) 
                          HEIGHT))]
    [(fired? s) s]))

(define (main ufo-posn-x)
  (big-bang (make-aim (make-posn ufo-posn-x 0) (make-tank (/ WIDTH 2) 3))
            [on-tick si-move]
            [to-draw si-render]
            [on-key si-control]
            [stop-when si-game-over? si-render-final]))



