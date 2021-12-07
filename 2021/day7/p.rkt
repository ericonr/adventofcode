#lang racket/base
(require racket/string)

(define (make-line t)
  (map string->number (string-split t ",")))

(define slist (make-line (read-line)))
(define maxv (apply max slist))

; part 1
(apply min (for/list ([v (in-range maxv)])
             (for/sum ([s (in-list slist)])
               (abs (- v s)))))

; part 2
(apply min (for/list ([v (in-range maxv)])
             (for/sum ([s (in-list slist)])
               (define steps (abs (- v s)))
               (/ (* (+ steps 1) steps) 2))))
;(for/sum ([i (in-range (add1 steps))]) i))))
