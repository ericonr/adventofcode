#lang racket

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

(define (iterate-y p1 p2)
  (for/list ([i (my-range (cdrm p1) (cdrm p2))]) (cons (car p1) i)))

(define (iterate-x p1 p2)
  (for/list ([i (my-range (car p1) (car p2))]) (cons i (cdrm p1))))

(define (iterate-xy p1 p2)
  (for/list ([i (my-range (car p1) (car p2))] [j (my-range (cdrm p1) (cdrm p2))]) (cons i j)))

(define (process-v v)
  (define p1 (car v))
  (define p2 (cdrm v))
  (if (equal-x p1 p2)
      (iterate-y p1 p2)
      (iterate-x p1 p2)))

(define (process-v2 v)
  (define p1 (car v))
  (define p2 (cdrm v))
  (if (equal-x p1 p2)
      (iterate-y p1 p2)
      (if (equal-y p1 p2)
          (iterate-x p1 p2)
          (iterate-xy p1 p2))))

(define h1 (make-hash))
(define h2 (make-hash))
(for* ([v (in-list list1)] [p (in-list (process-v v))]) (hash-update! h1 p add1 0))
(for* ([v (in-list slist)] [p (in-list (process-v2 v))]) (hash-update! h2 p add1 0))
(display "done creating hs\n")

(define (overlap v) (if (>= v 2) 1 0))

(display "res1: ")
(for/sum ([v (in-hash-values h1)]) (overlap v))
(display "res2: ")
(for/sum ([v (in-hash-values h2)]) (overlap v))
