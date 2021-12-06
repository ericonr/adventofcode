#lang racket

(define l null)
(for ([t (in-lines)])
  (define new (string->number t))
  (set! l (cons new l)))

(define v (list->vector (reverse l)))

(define (vsum vec pos)
  (define p1 (vector-ref vec pos))
  (define p2 (vector-ref vec (+ pos 1)))
  (define p3 (vector-ref vec (+ pos 2)))
  (+ p1 (+ p2 p3)))

(define count 0)
(define len (vector-length v))
(for/list ([i (in-range len)])
  (if (and (<= (+ i 4) len) (< (vsum v i) (vsum v (+ i 1))))
      (set! count (+ count 1))
      #f))

(display count)
