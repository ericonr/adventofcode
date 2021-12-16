#lang racket/base
(require racket/string)

(require graph)

(define (char->number c)
  (string->number (string c)))

(define (make-line t)
  (list->vector (map char->number (string->list t))))

(define sv (for/vector ([t (in-lines)]) (make-line t)))

(define v (vector-length sv))
(define h (vector-length (vector-ref sv 0)))

(define (access x y)
  (vector-ref (vector-ref sv y) x))

(define g (weighted-graph/directed null))
(for* ([x (in-range h)] [y (in-range v)])
  (let ([p (cons x y)])
    (when (< x (sub1 h))
      (add-directed-edge! g p (cons (add1 x) y) (access (add1 x) y)))
    (when (< y (sub1 v))
      (add-directed-edge! g p (cons x (add1 y)) (access x (add1 y))))))
(let-values ([(vs ed) (dijkstra g (cons 0 0))])
  (hash-ref vs (cons (sub1 h) (sub1 v))))
