;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Editor) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")))))
(define BACKG (empty-scene 200 20))
(define CURSOR (rectangle 1 20 "solid" "red"))

(define-struct editor [pre post])
; Editor = (make-editor String String)
; interpretation (make-editor s t) means the text in the editor is
; (string-append s t) with the cursor displayed between s and t

; Editor -> Image
; produce an image
(check-expect (render (make-editor "hello" " world"))
              (overlay/align "left" "center"
                             (beside (text "hello" 11 'black)
                                     CURSOR
                                     (text " world" 11 'black))
                             BACKG))
(check-expect (render (make-editor "" "world"))
              (overlay/align "left" "center"
                             (beside CURSOR
                                     (text "world" 11 'black))
                             BACKG))
(check-expect (render (make-editor "hello" ""))
              (overlay/align "left" "center"
                             (beside (text "hello" 11 'black)
                                     CURSOR)
                             BACKG))

(define (render editor)
  (overlay/align "left" "center"
                 (beside (text (editor-pre editor) 11 'black)
                         CURSOR
                         (text (editor-post editor) 11 'black))
                 BACKG))

; Editor KeyEvent -> Editor
; Produces a new editor according to the key event
; Reactions to key events:
; - letter with 1 character: add the letter to editor-pre part
; - backspace "\b": remove the last letter of editor-pre part unless it's empty
; - "\t":    ignore
; - "\007F": ignore
; - "left":  move the last letter of editor pre to editor post unless editor-pre is empty
; - "right": move the first letter of editor post to editor pre unless editor-post is empty
(check-expect (edit (make-editor "hello" "world") " ") (make-editor "hello " "world"))
(check-expect (edit (make-editor "" "world") "x") (make-editor "x" "world"))
(check-expect (edit (make-editor "hello" "") "x") (make-editor "hellox" ""))

(check-expect (edit (make-editor "hello" "world") "\b") (make-editor "hell" "world"))
(check-expect (edit (make-editor "" "world") "\b") (make-editor "" "world"))
(check-expect (edit (make-editor "hello" "") "\b") (make-editor "hell" ""))

(check-expect (edit (make-editor "hello" "world") "\t") (make-editor "hello" "world"))
(check-expect (edit (make-editor "hello" "world") "\007F") (make-editor "hello" "world"))

(check-expect (edit (make-editor "hello" "world") "left") (make-editor "hell" "oworld"))
(check-expect (edit (make-editor "" "world") "left") (make-editor "" "world"))

(check-expect (edit (make-editor "hello" "world") "right") (make-editor "hellow" "orld"))
(check-expect (edit (make-editor "hello" "") "right") (make-editor "hello" ""))

(define (edit editor ke)
  (cond
    [(string=? "\b" ke) (remove-last-pre editor)] ; backspace
    [(string=? "\t" ke) editor]                   ; ignore
    [(1String? ke) (add-to-pre editor ke)]        ; letter with 1 character
    [(string=? "left" ke) (move-left editor)] ; left
    [(string=? "right" ke) editor]; right
    [else editor]))                ; ignore the rest


; Editor String -> Editor
; adds string to (editor-pre editor)
(check-expect (add-to-pre (make-editor "hello" "world") " ") (make-editor "hello " "world"))
                          
(define (add-to-pre editor string)
  (make-editor (string-append (editor-pre editor) string)
               (editor-post editor)))

; String -> Bool
; checks if a-string consists of exactly one letter
(check-expect (1String? "a") true)
(check-expect (1String? "ab") false)

(define (1String? a-string)
  (= (string-length a-string) 1))

; Editor -> Editor
; removes the last letter from (editor-pre editor)
(check-expect (remove-last-pre (make-editor "hello" "world")) (make-editor "hell" "world"))
(check-expect (remove-last-pre (make-editor "" "world")) (make-editor "" "world"))

(define (remove-last-pre editor)
  (make-editor
   (substring (editor-pre editor) 
              0 
              (if (> (string-length (editor-pre editor)) 0)
                  (- (string-length (editor-pre editor)) 1)
                  (string-length (editor-pre editor))))
   (editor-post editor)))

; Editor -> Editor
; moves the first character of (editor-post editor) to the last position of 
; (editor-pre editor)
(check-expect (move-left (make-editor "hello" "world")) (make-editor "hell" "oworld"))
(check-expect (move-left (make-editor "hello" "")) (make-editor "hell" "o"))
(check-expect (move-left (make-editor "" "world")) (make-editor "" "world"))

(define (move-left editor)
  (make-editor
   (without-last-letter (editor-pre editor))
   (string-append (last-letter (editor-pre editor)) (editor-post editor))))

; String -> String
; creates a copy of a string without the last letter
(check-expect (without-last-letter "hello") "hell")
(check-expect (without-last-letter "h") "")
(check-expect (without-last-letter "") "")

(define (without-last-letter string)
  (if (> (string-length string) 0)
      (substring string 0 (- (string-length string) 1))
      string))

; String -> 1String
; extracts the last letter of string
(check-expect (last-letter "hello") "o")
(check-expect (last-letter "h") "h")
(check-expect (last-letter "") "")

(define (last-letter string)
  (if (> (string-length string) 0)
      (string-ith string (- (string-length string) 1))
      string))


