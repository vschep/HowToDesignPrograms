;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname TimeSeconds) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")))))
(define MINS-OF-HOUR 60)
(define SECONDS-OF-MINUTE 60)

(define-struct time [hour minute second])
; Time is (make-time Integer Integer Integer)
; e.g. (make-time 1 2 3)
; hour is >= 0 and <= 23
; minute is >= 0 and <= 59
; second is >= 0 and <= 59

; Time -> PositiveInteger
; produces the number of seconds that have passed since midnight
(check-expect (time->seconds (make-time 0 0 0)) 0)
(check-expect (time->seconds (make-time 1 2 3)) 3723)
(check-expect (time->seconds (make-time 23 59 59)) (+ (* 23 60 60) (* 59 60) 59))

(define (time->seconds time)
  (+
   (* (time-hour time) MINS-OF-HOUR SECONDS-OF-MINUTE)
   (* (time-minute time) SECONDS-OF-MINUTE)
   (time-second time)))