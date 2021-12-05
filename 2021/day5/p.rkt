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

(define h1 (make-hash))
(for* ([v (in-list list1)] [p (in-list (process-v v))]) (hash-update! h1 p add1 0))
(display "done creating count1\n")

(display "res1: ")
(for/sum ([v (in-hash-values h1)] #:when (>= v 2)) 1)
