;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname car_image) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")))))
(require 2htdp/image)

(define CARBODY (rectangle 200 50 "solid" "green"))
(define CNWIDTH 300)
(define CNHEIGHT 200)
(define SCN (empty-scene CNWIDTH CNHEIGHT))
(define CARBODYSCN (place-image CARBODY (/ CNWIDTH 2) 160 SCN))
(define WHEEL (circle 20 "solid" "blue"))
(place-image WHEEL 210 180 (place-image WHEEL 90 180 CARBODYSCN))