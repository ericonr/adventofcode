#lang racket/base
(require racket/string)
(require racket/list)

(define (make-line t)
  (let ([l (string-split t " -> ")])
    (values (car l) (second l))))

(define template (read-line))
(define throwaway (read-line))

(define transforms (for/hash ([t (in-lines)]) (make-line t)))

(define template-hash (make-hash))
(for ([index (in-range (sub1 (string-length template)))])
  (hash-update! template-hash (string (string-ref template index) (string-ref template (add1 index))) add1 0))

(define (apply-transfomation t chars)
  (define new-hash (make-hash))
  (for ([h (hash-keys t)])
    (define (updater v)
      (+ v (hash-ref t h)))

    (let
        ([c1 (string-ref h 0)] [c2 (string-ref h 1)] [nc (string-ref (hash-ref transforms h) 0)])
      (hash-update! new-hash (string c1 nc) updater 0)
      (hash-update! new-hash (string nc c2) updater 0)

      ; build the character count as we go
      (hash-update! chars nc updater 0)))

  new-hash)

(define char-hash (make-hash))

(for ([c (string->list template)])
  (hash-update! char-hash c add1 0))

; doesn't blow up at 40 iterations!
(for ([i (in-range 40)])
  (set! template-hash (apply-transfomation template-hash char-hash)))

(- (apply max (hash-values char-hash)) (apply min (hash-values char-hash)))
