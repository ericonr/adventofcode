#lang racket

; part 1 vars
(define h 0)
(define d 0)
; part 2 vars
(define aim 0)
(define h2 0)
(define d2 0)

(define v 0)
(for ([t (in-lines)])
	  (if (string-prefix? t "up")
		 (begin (set! v (string->number (string-trim t "up ")))
		  (set! d (- d v))
		  (set! aim (- aim v)))
		 #f)
	  (if (string-prefix? t "down")
		 (begin (set! v (string->number (string-trim t "down ")))
		  (set! d (+ d v))
		  (set! aim (+ aim v)))
		 #f)
	  (if (string-prefix? t "forward")
		 (begin (set! v (string->number (string-trim t "forward ")))
		  (set! h (+ h v))
		  (set! h2 (+ h2 v))
		  (set! d2 (+ d2 (* aim v))))
		 #f))

(display (* h d))
(display " ")
(display (* h2 d2))
