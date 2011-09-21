;;; Copyright (c) 2010 Preston Skupinski
;;;
;;; This program is free software. It comes without any warranty, to
;;; the extent permitted by applicable law. You can redistribute it
;;; and/or modify it under the terms of the Do What The Fuck You Want
;;; To Public License, Version 2, as published by Sam Hocevar. See
;;; http://sam.zoy.org/wtfpl/COPYING for more details.
;;;
;;; matrix.scm
;;;
;;; My implementation of the matrix animation done in ncurses for chicken 
;;; scheme.
;;;

(require-extension ncurses srfi-18)

(define cols 0)
(define lines 0)

(define max-strips 50) ; Number of strips there will be at any one time.
(define max-characters 15) ; The max number of characters in a strip.

(define (main)
  (initscr)
  (cbreak)
  (nodelay (stdscr) #t)
  (noecho)
  (curs_set 0)
  (start_color)
  (init_pair 1 COLOR_WHITE COLOR_BLACK)
  (init_pair 2 COLOR_GREEN COLOR_BLACK)
  (set! cols (COLS))
  (set! lines (LINES))
  (let loop ((c (getch)) (strips (generate-initial-strips))
			 (s (current-milliseconds)))
    (unless (eq? c (integer->char 27))
      (when (>= (- (current-milliseconds) s) 40)
        (begin
    	  (clear)
	  (set! s (current-milliseconds))))
      (attron (COLOR_PAIR 1))
      (border 0 0 0 0 0 0 0 0)
      (mvaddstr 0 0 "Press 'Esc' to exit")
      (mvaddstr 0 (- (fx/ cols 2) 3) "Matrix")
      (for-each render-strip strips)
      (thread-sleep! 0.08)
      (loop (getch) (rebuild-strip-list strips) s)))
   (endwin))

;; generate-initial-strips is a function that generates the initial strips for
;; the matrix animation.
(define (generate-initial-strips)
  (let loop ((i 0) (strip-list '()))
    (if (< i max-strips)
      (loop (+ i 1) (append strip-list 
			    (list (move-to-random-y (generate-new-strip)))))
       strip-list))) 

;; move-to-random-y is a function that moves the strip's head to a random y 
;; location.
(define move-to-random-y
  (lambda (strip)
    (list (list-ref strip 0) (+ (random (- lines 2)) 1) 
	  (list-ref strip 2) (list-ref strip 3))))

;; move-strip-down is a function that moves a single strip down by one and 
;; changes each character it contains.
(define move-strip-down
  (lambda (strip)
    (list (list-ref strip 0) (+ (list-ref strip 1) 1) 
	  (generate-random-char-list (length (list-ref strip 2)))
	  (list-ref strip 3))))


;; rebuild-strip-list is a function that rebuilds the strip list,  discarding
;; any strips that are completely offscreen and adding a new strip if one is
;; completely off-screen. 
(define rebuild-strip-list
  (lambda (strip-list)
    (let loop ((i 0) (new-strip '()))
      (if (< i (length strip-list))
	(begin
          (if (strip-should-be-removed? (list-ref strip-list i))
  	    (loop (+ i 1) (append new-strip (list (generate-new-strip))))
	    (loop (+ i 1) (append new-strip 
	    			  (list (move-strip-down 
					  (list-ref strip-list i)))))))
      new-strip))))

;; generate-random-char is a function that generates a random character for
;; use in a new strip.
(define (generate-random-char)
  (integer->char (+ (random 89) 33)))

;; generate-random-char-list is a function that generates a list of random 
;; characters with the given length.
(define generate-random-char-list
  (lambda (len)
    (let char-loop ((char-list '()))
       (if (< (length char-list) len)
         (char-loop (append char-list (list (generate-random-char))))
	  char-list)))) 

;; generate-new-strip is a function that generates a new strip with random x
;; position within bounds of the screen and a random number of characters 
;; constrained by the global max-characters.
(define (generate-new-strip)
  (let ((x (+ (random (- cols 2)) 1)) 
	(y  1)
	(chars (generate-random-char-list (+ (random (- max-characters 2)) 3)))
	(white (random 3))) ; Give it a 1 in 3 chance of having a white head.
	(list x y chars white)))

;; strip-should-be-removed? is a function that checks to see if the matrix 
;; strip is offscreen completely and if so returns #t and if not returns #f.
(define strip-should-be-removed?
  (lambda (strip)
    (let ((strip-back (- (list-ref strip 1) (- (length (list-ref strip 2)) 1))))
      (and (is-offscreen? (list (list-ref strip 0) strip-back 
					 (list-ref strip 2) (list-ref strip 3)))
	       (is-offscreen? strip)))))
	

;; render-strip is a function for rendering a matrix strip.  Each strip is
;; represented as '(x y chars white) where x and y are points that represent 
;; where the head of the strip is(the bottom of it), chars is a list containing
;; the characters that make up the strip and white is a value that determines
;; if the strip will have a white head(a value of 1 denotes this).
(define render-strip
  (lambda (strip)
    (if (= (list-ref strip 3) 1)
      (attron (COLOR_PAIR 1))
      (attron (COLOR_PAIR 2)))
    (let loop ((matrix-strip strip) (first #t))
      (unless (is-offscreen? matrix-strip)
        (mvaddch (list-ref matrix-strip 1) (list-ref matrix-strip 0)
  	       (car (list-ref matrix-strip 2))))
      (if (= (list-ref strip 3) 1)
	(if first
	  (attron (COLOR_PAIR 2))))
      (unless (null? (cdr (list-ref matrix-strip 2)))
        (loop 
          (list (list-ref matrix-strip 0) (- (list-ref matrix-strip 1) 1)
          (cdr (list-ref matrix-strip 2)) (list-ref matrix-strip 3)) #f)))))

;; is-offscreen is a function that checks to see if the strip given's head is
;; offscreen or not.
(define is-offscreen?
 (lambda (strip)
    (or (or (>= (list-ref strip 0) (- cols 1)) (<= (list-ref strip 0) 0))
      (or (>= (list-ref strip 1) (- lines 1)) (<= (list-ref strip 1) 0)))))

(main)
