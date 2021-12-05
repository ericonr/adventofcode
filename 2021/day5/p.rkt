#lang racket

; bingo table size
(define size 5)

; return element instead of list for cdr
(define (cdrm l)
  (car (cdr l)))

(define (make-line t)
  (define l (map string-trim (string-split t "->")))
  (map (lambda (x) (map string->number (string-split x ","))) l))

(define (equal-x p1 p2) (equal? (car p1) (car p2)))
(define (equal-y p1 p2) (equal? (cdrm p1) (cdrm p2)))

(define (hori-vert v)
  (define p1 (car v))
  (define p2 (cdrm v))
  (or (equal-x p1 p2) (equal-y p1 p2)))

(define (my-range x1 x2)
  (if (<= x1 x2) (in-range x1 (add1 x2)) (in-range x1 (sub1 x2) -1)))

(define (f-hori-vert l)
  (filter hori-vert l))

(define slist (for/list ([t (in-lines)]) (make-line t)))
(define list1 (f-hori-vert slist))

(define (process-v v)
  (define p1 (car v))
  (define p2 (cdrm v))
  (if (equal-x p1 p2)
	 (for/list ([i (my-range (cdrm p1) (cdrm p2))]) (cons (car p1) i))
	 (for/list ([i (my-range (car p1) (car p2))]) (cons i (cdrm p1)))))

(define count1 (for*/list ([v (in-list list1)] [p (in-list (process-v v))]) p))
(display "done creating count1\n")

(define already-count1 null)
(for ([p (in-list count1)] #:unless (member p already-count1))
	  (when (member p (cdr (member p count1)))
		 (set! already-count1 (append already-count1 (list p)))))

(display "res1: ") (display (length already-count1))
