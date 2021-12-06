#lang racket

(define first-initial-days 8)
(define initial-days 6)

(define (make-line t)
  (map string->number (string-split t ",")))

(define slist (make-line (read-line)))
(define sv (make-vector 9))
(for ([d slist]) (vector-set! sv d (add1 (vector-ref sv d))))

(define (total-fish vec)
  (for/sum ([v vec]) v))

; input desired days HERE
(for ([i (in-range 256)])
	  (define holder (vector-ref sv 0))
	  (for ([j (in-range 1 9)])
			 (vector-set! sv (sub1 j) (vector-ref sv j)))
	  (vector-set! sv 6 (+ (vector-ref sv 6) holder))
	  (vector-set! sv 8 holder))

(total-fish sv)
