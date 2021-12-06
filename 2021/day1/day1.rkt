#lang racket/base

(define (go)
  'yep-it-works)
(go)

(define count 0)
(define old 5000)
(for ([t (in-lines)])
  (define new (string->number t))
  (if (> new old)
      (set! count (+ count 1))
      #f)
  (set! old new))

(display count)
