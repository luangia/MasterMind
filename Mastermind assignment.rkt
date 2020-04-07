#lang racket/gui

(require racket/trace)

; Welcome to MasterMind.  This version has a graphical output and a textual input.
; The shell was written by Dr. Browning February 10, 2014.
;
; Assignment finished by Ben Zimmerman and Lucas Pham
; More enhancements pending

; Set up the initial frame for the output display
(define frame (new frame%
                   [label "MasterMind"]
                   [width 460]
                   [height 140]))

; Show the available colors in a white band at the top of the display

(define possiblecolors '("red" "orange" "yellow" "green" "SkyBlue" "blue" "purple" "brown"))

(define (display-colors dc)
  (send dc set-pen "black" 1 'solid )  
  (for ([i (in-range (length mycolors))])          ; display each of the eight colors
    (begin (send dc set-brush (get-item (+ i 1) mycolors) 'solid)
           (send dc draw-ellipse (+ 12.5 (* 15 i)) 5 10 10))))

(define (show-colors)
  (new canvas% [parent frame]  ;create a canvas in the frame
     [min-height 50]
       [paint-callback
        (lambda (canvas ac)
          (send ac set-background  "white") ;sets background to white
          (send ac clear)          ;causes new background color to appear
          (send ac set-scale 3 3)  ;makes the dots bigger by a factor of 3
          (display-colors ac)
          )]))

; Set up the secret code
;;;;;;;;;;;;;;
; Assignment 2 :  Make this a random secret code
;;;;;;;;;;;;;;

; Get random element from a list
(define (random-ele mylist)
  (list-ref mylist (random (length mylist))))

; Make a list with length len from random numbers from a list
(define (random-list mylist newlist len)
  (if (>= 0 len)
      newlist
      (random-list mylist
                   (cons (random-ele mylist) newlist)
                   (- len 1))))


; Helper function.  Returns the ith item from mylist
; NOT ROBUST. This assumes i is less than or equal to length of mylist
(define (get-item i mylist)
    (if (= i 1)
        (car mylist)
        (get-item (- i 1) (cdr mylist))))

; Display the colors of the guess and then the black and white pegs for the score
(define (display-myline dc roundguess rectscore) 
  (send dc set-pen "black" 1 'solid )   
  (for ([i (in-range pattern-len)])                        ; display guess
    (begin (send dc set-brush (get-item (+ i 1) roundguess) 'solid)
                      (send dc draw-ellipse (+ 5 (* 15 i)) 5 10 10)))
  (send dc set-pen "tan" 1 'solid ) 
  (for ([i (in-range pattern-len)])                        ; display score
    (begin(send dc set-brush (get-item (+ i 1) rectscore) 'solid)
                    (send dc draw-rectangle (+ (* 16 pattern-len) (* 10 i)) 5 6 9))))

; This is the function that we call to add a line to the display
(define (addaline guessin responsein)
  (new canvas% [parent frame]  ;create a canvas in the frame
     [min-height 50]
       [paint-callback
        (lambda (canvas dc)
          (send dc set-background  "tan") ;sets background to tan
          (send dc clear)          ;causes new background color to appear
          (send dc set-scale 3 3)  ;makes the dots bigger by a factor of 3
          (display-myline dc guessin responsein)
          )]))

(send frame show #t)  ; make the graphics visible

; Score a guess 
;;;;;;;;;;;;;;
; Assignment 1:  Return the correct string for a score.
;;;;;;;;;;;;;;
; Return a list of length 5 starting with a "black" entry for
; each time an element of mycode matches the element of myguess in the same position.
; The returned list then contains a "white" entry for each time an element of myguess
; also occurs in mycode (ignoring all items previously matched).
; The returned list then contains "tan" entries as needed to fill out the five entries.
(define (score mycode myguess)
  (combine (checkblack mycode myguess) (checkwhite mycode myguess) pattern-len))

; A complete combining of blacks and whites (with tans)
(define (combine blacks whites len)
  (addblanks (combineBW blacks whites) len))

; Add blank ("tan") to the pattern
(define (addblanks list len)
  (if (= len (length list))
      list
      (addblanks (append list '("tan")) len)))

; Combine blacks and whites (without tans)
(define (combineBW blacks whites)
  (if (<= (length whites) (length blacks))
      blacks
      (combineBW (append blacks (list (car whites))) (cdr whites))))

; Check for the number of blacks
(define (checkblack mycode myguess)
  (if (null? myguess)
      '()
      (if (eq? (car myguess) (car mycode))
          (append (list "black") (checkblack (cdr mycode) (cdr myguess)))
          (checkblack (cdr mycode) (cdr myguess)))))

; Check for the number of whites
(define (checkwhite mycode myguess)
  (if (null? myguess)
      '()
      (if (member (car myguess) mycode)
          (append (list "white")
                  (checkwhite (cdr myguess) (remove (car myguess) mycode)))
          (checkwhite (cdr myguess) mycode))))

; Display a guess and its score
(define (guess mylist)
  (addaline mylist (score pattern mylist)))


; Sample calls 
(show-colors)
;(guess '("red" "blue" "green" "yellow" "orange"))
;(addaline '("green" "purple" "SkyBlue" "brown" "red") '("black" "black" "black" "tan" "tan"))
;(addaline '("red" "brown" "blue" "skyblue" "red") '("black" "white" "tan" "tan" "tan"))
;(addaline '("yellow" "red" "yellow" "blue" "yellow") '("white" "tan" "tan" "tan" "tan"))
;(addaline '("orange" "purple" "green" "yellow" "orange") '("black" "white" "white" "tan" "tan"))
;(addaline '("red" "orange" "yellow" "green" "skyblue") '("black" "black" "black" "black" "black"))

; Prompt the user
(display "Welcome to MasterMind\n")
;(display "Guess a code of length five using these colors:\n")
;mycolors

; Get the length of the pattern from user
(display "How long is your pattern?\n")
(define pattern-len (read))

; Get the number of colors
(display "How many possible colors should there be in the pattern?\n")
(define colornum (read))

; Make the list of colors for the current game
(define mycolors (take possiblecolors colornum))

(display "\nMake a guess from these possible colors: ")
mycolors

; Make a random pattern from the list of pattern
(define pattern
  (random-list mycolors '() pattern-len))

(display "Here is a sample guess:\n")
(display "(guess '(\"yellow\" \"red\" \"yellow\" \"blue\" \"yellow\"))")
pattern