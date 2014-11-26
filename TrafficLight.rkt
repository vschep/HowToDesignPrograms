;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname TrafficLight) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")))))
; Graphic constants
(define MT (empty-scene 100 30))
(define PADDING (rectangle 5 10 "solid" "white"))

; TrafficLight -> TrafficLight
; simulates a traffic light that changes with each clock tick
(define (traffic-light-simulation initial-state)
  (big-bang initial-state
            [to-draw tl-render]
            [on-tick tl-next 1]))

; TrafficLight -> TrafficLight
; determines the next state of the traffic light, given current-state

(check-expect (tl-next "green") "yellow")
(check-expect (tl-next "yellow") "red")
(check-expect (tl-next "red") "green")

(define (tl-next current-state)
  (cond
    [(string=? "green" current-state)  "yellow"]
    [(string=? "yellow" current-state) "red"]
    [(string=? "red" current-state)    "green"]))
 
; TrafficLight -> Image
; renders the current state of the traffic light as a image

(check-expect (tl-render "green")
              (overlay (beside (circle 10 "outline" "red") PADDING
                               (circle 10 "outline" "yellow") PADDING 
                               (circle 10 "solid"   "green"))
                       MT))
(check-expect (tl-render "yellow")
              (overlay (beside (circle 10 "outline" "red") PADDING
                               (circle 10 "solid"   "yellow") PADDING 
                               (circle 10 "outline" "green"))
                       MT))
(check-expect (tl-render "red")
              (overlay (beside (circle 10 "solid"   "red") PADDING
                               (circle 10 "outline" "yellow") PADDING 
                               (circle 10 "outline" "green"))
                       MT))

(define (tl-render current-state)
  (overlay (beside (circle 10 
                           (if (string=? "red" current-state) "solid" "outline") 
                           "red") 
                   PADDING
                   (circle 10 
                           (if (string=? "yellow" current-state) "solid" "outline") 
                           "yellow") 
                   PADDING 
                   (circle 10
                           (if (string=? "green" current-state) "solid" "outline")
                           "green"))
           MT))
