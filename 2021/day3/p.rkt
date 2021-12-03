#lang racket

(require gtp-util)

; max supported length
(define len 16)

; part 1 vars
(define onecount (make-vector len))
; part 2 vars
(define slist null)

(define binary-len 0)
(for ([t (in-lines)])
	  (define l (string->list t))
	  (for ([v (enumerate (reverse l))])
			 (set! binary-len (+ 1 (car v))) ; will take the last index
			 (when (equal? #\1 (cdr v))
				(define index (- (- len (car v)) 1))
				(vector-set! onecount index (add1 (vector-ref onecount index)))))
	  (set! slist (cons (list->vector l) slist)))

(display " binary-len: ")
(display binary-len)
(display " v: ")
(display onecount)

; utility function
(define (binary-vector-to-num v)
  (define s (list->string (vector->list v)))
  (string->number s 2))

; part 2 utility functions
(define (bit-criteria lines pos bias)
  (define count (for/sum ([t lines] #:when (equal? (vector-ref t pos) #\1)) 1))
  (if (>= (* 2 count) (length lines))
	 (if (equal? 1 bias) #\1 #\0)
	 (if (equal? 1 bias) #\0 #\1)))
(define (select-lines lines pos value)
  (filter (lambda (x) (equal? value (vector-ref x pos))) lines))
  ;(for/list ([t lines] #:when (equal? value (vector-ref t pos))) t))

; part 1 variables
(define gamma (make-vector binary-len #\0))
(define epsilon (make-vector binary-len #\0))
; part 2 variables
(define oxygen slist)
(define co2 slist)

(for ([index (in-range binary-len)])
	  (define v (vector-ref onecount (+ index (- len binary-len))))
	  (if (>= (* 2 v) (length slist))
		 (vector-set! gamma index #\1)
		 (vector-set! epsilon index #\1))
	  (when (> (length oxygen) 1)
		 (define c (bit-criteria oxygen index 1))
		 (set! oxygen (select-lines oxygen index c)))
	  (when (> (length co2) 1)
		 (define c (bit-criteria co2 index 0))
		 (set! co2 (select-lines co2 index c))))

(display " res1: ")
(display (* (binary-vector-to-num gamma) (binary-vector-to-num epsilon)))
(display " res2: ")
(display (* (binary-vector-to-num (car oxygen)) (binary-vector-to-num (car co2))))
