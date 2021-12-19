#lang racket/base
(require racket/match)
(require graph)

(define (char->number c)
  (string->number (string c)))

(define (make-line t)
  (list->vector (map char->number (string->list t))))

(define sv (for/vector ([t (in-lines)]) (make-line t)))

(define v (vector-length sv))
(define h (vector-length (vector-ref sv 0)))
; part 1: extra=1
; part 2: extra=5
(define extra 5)

(define (access x y)
  (vector-ref (vector-ref sv y) x))

(define (new-pos x y ex ey)
  (vector (+ x (* h ex)) (+ y (* v ey))))

(define (fix-value x y)
  (let*-values ([(ex x) (quotient/remainder x h)] [(ey y) (quotient/remainder y v)] [(val) (+ (access x y) ex ey)])
    (if (> val 9)
        (remainder val 9)
        val)))

(define g (weighted-graph/directed null))
(match-let ([(vector h v) (new-pos h v (sub1 extra) (sub1 extra))])
  (for* ([x (in-range h)] [y (in-range v)])
    (let ([p (vector x y)])
      (when (> x 0)
        (add-directed-edge! g p (vector (sub1 x) y) (fix-value (sub1 x) y)))
      (when (> y 0)
        (add-directed-edge! g p (vector x (sub1 y)) (fix-value x (sub1 y))))
      (when (< x (sub1 h))
        (add-directed-edge! g p (vector (add1 x) y) (fix-value (add1 x) y)))
      (when (< y (sub1 v))
        (add-directed-edge! g p (vector x (add1 y)) (fix-value x (add1 y))))))
  (let-values ([(vs ed) (dijkstra g (vector 0 0))] [(pos) (vector (sub1 h) (sub1 v))])
    (define (explore pos)
      (display pos)
      (display "\n")
      (unless (equal? pos (vector 0 0)) (explore (hash-ref ed pos))))
    ;(explore pos)
    (hash-ref vs pos)))

; print expanded grid
(when #f
(match-let ([(vector h v) (new-pos h v (sub1 extra) (sub1 extra))])
  (for* ([y (in-range v)] [x (in-range h)])
    (when (= x 0)
      (display "\n"))
    (display (fix-value x y)))))
