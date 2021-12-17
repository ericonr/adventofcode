#lang racket/base
(require racket/string)

(define (make-line t)
  (let* ([t (string-trim t "target area: ")]
         [sl (string-split t ", ")]
         [x (string-trim (car sl) "x=")]
         [y (string-trim (cadr sl) "y=")])
    (values (map string->number (string-split x "..")) (map string->number (string-split y "..")))))

(define-values (xp yp) (make-line (read-line)))

(define (run-steps x y)
  (define (recu x y px py max-y number-of-recursions)
    (define (lower-abs v)
      (if (> v 0)
          (sub1 v)
          (if (< v 0)
              (add1 v)
              0)))

    (let* ([px (+ x px)]
           [py (+ y py)]
           [max-y (if (> y 0) py max-y)]
           [x (lower-abs x)]
           [y (sub1 y)])
      (if (and (<= (car xp) px (cadr xp)) (<= (car yp) py (cadr yp)))
          (cons 'hit max-y)
          (if (or (> number-of-recursions 500) (and (= x 0) (< px (car xp))))
              (cons 'fail 0)
              (recu x y px py max-y (add1 number-of-recursions))))))

  (recu x y 0 0 0 0))

(define result
  (for*/list ([x (in-range (add1 (cadr xp)))] [y (in-range (car yp) (* -4 (cadr yp)))])
    (run-steps x y)))
; part 1
(apply max (map cdr result))
; part 2
(length (filter (lambda (x) (equal? (car x) 'hit)) result))
