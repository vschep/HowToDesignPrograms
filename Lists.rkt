;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Lists) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")))))
; List-of-names -> Boolean
; determines whether "Flatt" occurs on a-list-of-names

(check-expect (contains-flatt? empty) false)
(check-expect (contains-flatt? (cons "Findler" empty)) false)
(check-expect (contains-flatt? (cons "Flatt" empty)) true)
(check-expect
  (contains-flatt? (cons "Mur" (cons "Fish"  (cons "Find" empty))))
  false)
(check-expect
  (contains-flatt? 
   (cons "Fagan"
         (cons "Findler"
               (cons "Fisler"
                     (cons "Flanagan"
                           (cons "Flatt"
                                 (cons "Felleisen"
                                       (cons "Friedman" empty))))))))
  true)

(define (contains-flatt? a-list-of-names)
  (cond
    [(empty? a-list-of-names) false]
    [(cons? a-list-of-names)
     (or (string=? (first a-list-of-names) "Flatt")
         (contains-flatt? (rest a-list-of-names)))]))


; A List-of-amounts is one of: 
; – empty
; – (cons PositiveNumber List-of-amounts)
; interpretation a List-of-amounts represents some amounts of money 

; List-of-amounts -> PositiveNumber
; Compute the sum of List-of-amounts

(check-expect (sum empty) 0)
(check-expect (sum (cons 3 empty)) 3)
(check-expect (sum (cons 3 (cons 4 (cons 5 empty)))) 12)

(define (sum amounts)
  (cond
    [(empty? amounts) 0]
    [(cons? amounts) (+ (first amounts) (sum (rest amounts)))]))


; A List-of-numbers is one of: 
; – empty
; – (cons Number List-of-numbers)

; List-of-numbers -> bool
; Checks if all numbers in list-of-numbers are positive

(check-expect (pos? empty) true)
(check-expect (pos? (cons 3 empty)) true)
(check-expect (pos? (cons 3 (cons 4 (cons 5 empty)))) true)
(check-expect (pos? (cons -3 empty)) false)
(check-expect (pos? (cons 3 (cons -4 (cons 5 empty)))) false)

(define (pos? list-of-numbers)
  (cond
    [(empty? list-of-numbers) true]
    [(cons? list-of-numbers) (and (>= (first list-of-numbers) 0)
                                  (pos? (rest list-of-numbers)))]))



