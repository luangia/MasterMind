#lang racket/gui

(require racket/trace)

; Welcome to MasterMind.  This version has a graphical output and a textual input.
; The shell was written by Dr. Browning February 10, 2014.
;  There are two functions that the student needs to write to complete this project:
;  First, write the code to determine the correct score for a guess.
;  Second, write the code to create a random pattern for the game.

; Set up the initial frame for the output display
(define frame (new frame%
                   [label "MasterMind"]
                   [width 460]
                   [height 140]))

; Show the available colors in a white band at the top of the display

(define mycolors '("red" "orange" "yellow" "green" "skyblue" "blue" "purple" "brown"))

(define (display-colors dc)
  (send dc set-pen "black" 1 'solid )  
  (for ([i (in-range 8)])          ; display each of the eight colors
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

; Define the pattern
(define pattern
  (random-list mycolors '() 5))
  

; Helper function.  Returns the ith item from mylist
; NOT ROBUST. This assumes i is less than or equal to length of mylist
(define (get-item i mylist)
    (if (= i 1)
        (car mylist)
        (get-item (- i 1) (cdr mylist))))

; Display the five colors of the guess and then the black and white pegs for the score
(define (display-myline dc roundguess rectscore) 
  (send dc set-pen "black" 1 'solid )   
  (for ([i (in-range 5)])                        ; display guess
    (begin (send dc set-brush (get-item (+ i 1) roundguess) 'solid)
                      (send dc draw-ellipse (+ 5 (* 15 i)) 5 10 10)))
  (send dc set-pen "tan" 1 'solid ) 
  (for ([i (in-range 5)])                        ; display score
    (begin(send dc set-brush (get-item (+ i 1) rectscore) 'solid)
                    (send dc draw-rectangle (+ 95 (* 10 i)) 5 6 9))))

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
  (combine (checkblack mycode myguess) (checkwhite mycode myguess) 5))

; A complete combining of blacks and whites
(define (combine blacks whites len)
  (addblanks (combineBW blacks whites) len))

; Add blank ("tan") to the pattern
(define (addblanks list len)
  (if (equal? len (length list))
      list
      (addblanks (append list '("tan")) len)))

; Combine blacks and whites
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

; Display a guess and its score, track the guess, and see if the user won
(define (guess mylist)
  (set! numguesses (+ numguesses 1))
  (let ((myscore (score pattern mylist)))
    (addaline mylist myscore)
    (judgewin myscore)
    )
  )

; Track number of guesses
(define numguesses 0)

;Indicate when the player has won
(define (judgewin pegs)
  (when (equal? pegs '("black" "black" "black" "black" "black"))
    (display "You win! Number of guesses: ")
    (display numguesses)
    )
  )

; Show available colors 
(show-colors)

; Show pattern for testing
;(display pattern)

; Prompt the user
(display "Welcome to MasterMind\n")
(display "Guess a code of length five using these colors:\n")
mycolors
(display "Here is a sample guess:\n")
(display "(guess '(\"yellow\" \"red\" \"green\" \"blue\" \"purple\"))")