;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname HappinessGauge) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")))))
(define MAX-HAPPINESS 100)
(define MIN-HAPPINESS 0)
(define DECREASE-STEP 0.1)

(define BAR-WIDTH 100)
(define BAR-HEIGHT 40)

(define (main ws)
  (big-bang MAX-HAPPINESS
            [on-tick tock]
            [to-draw render]
            [on-key key-pressed]))

; WorldState ws represents happiness between 0 and 100. 100 is the greates happiness, 0 the smallest.
; number -> number
; Decrease the happiness over time. With every tick happiness is decreased by 0.1.
(define (tock ws)
  (cond
    [(> ws MIN-HAPPINESS) (- ws DECREASE-STEP)]
    [else MIN-HAPPINESS]))

; Tests
(check-expect (tock 1) 0.9)
(check-expect (tock 0) 0)


(define (render ws)
  (place-image/align 
   (rectangle ws 20 "solid" "red") 
   1 1 "left" "top"
   (rectangle 102 22 "solid" "black")))

; number string -> number
; Changes the happiness value according to key presses,
; down array key increases by 1/5,
; up array key let's happiness jump by 1/3.
(define (key-pressed ws ke)
  (cond 
    [(string=? ke "down") (min (round (* ws 6/5)) 
                               100)]
    [(string=? ke "up")   (min (round (* ws 4/3))
                               100)]))

; Tests
(check-expect (key-pressed 10 "down") 12)
(check-expect (key-pressed 0 "down") 0)
(check-expect (key-pressed 99 "down") 100)
(check-expect (key-pressed 4 "down") 5)
(check-expect (key-pressed 0 "up") 0)
(check-expect (key-pressed 12 "up") 16)
(check-expect (key-pressed 99 "up") 100)