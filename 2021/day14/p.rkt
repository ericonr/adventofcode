#lang racket/base
(require racket/string)
(require racket/list)

(define (make-line t)
  (let ([l (string-split t " -> ")])
    (values (car l) (second l))))

(define template (read-line))
(define throwaway (read-line))

(define transforms (for/hash ([t (in-lines)]) (make-line t)))

(define (apply-transfomation t)
  (define new-string "")
  (for ([index (in-range (sub1 (string-length t)))])
    (set! new-string (string-append new-string (string (string-ref t index)) (hash-ref transforms (string (string-ref t index) (string-ref t (add1 index))) ""))))
  (set! new-string (string-append new-string (string (string-ref t (sub1 (string-length t))))))
  new-string)

; works for 10 iterations, blows up at 40!
(for ([i (in-range 10)])
  (set! template (apply-transfomation template)))

; part 1
(define template-list (sort (string->list template) char<?))
(define count-hash-table (for/hash ([c (remove-duplicates template-list)])
                           (values c (for/sum ([nc template-list] #:when (equal? c nc)) 1))))
count-hash-table
(- (apply max (hash-values count-hash-table)) (apply min (hash-values count-hash-table)))
