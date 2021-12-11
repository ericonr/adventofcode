#lang racket/base
(require racket/match)

(define (char->number c)
  (string->number (string c)))

(define (make-line t)
  (list->vector (map char->number (string->list t))))

(define sv (for/vector ([t (in-lines)]) (make-line t)))

(define v (vector-length sv))
(define h (vector-length (vector-ref sv 0)))

(define (access pv ph)
  (vector-ref (vector-ref sv pv) ph))
(define (change pv ph v)
  (vector-set! (vector-ref sv pv) ph v))

(define (apply-step i)
  ; keep track of octopuses that have flashed
  (define index 0)
  (define nines (make-vector 300 null)) ; arbitrary length
  (define (try-add-nine pv ph)
    (when (= (access pv ph) 9)
      (vector-set! nines index (cons pv ph))
      (set! index (add1 index)))
    (change pv ph (add1 (access pv ph))))

  (for* ([vi (in-range v)] [hi (in-range h)])
    (when (> (access vi hi) 9) (change vi hi 0)) ; optimization to make reset simpler
    (try-add-nine vi hi))

  ; loop through nines but being able to add more flashes
  (for ([j (in-naturals)] #:break (= j index))
    (match-define (cons pv ph) (vector-ref nines j))

    (define (touch-point pv ph)
      (when (and (and (>= pv 0) (< pv v)) (and (>= ph 0) (< ph h)))
        (try-add-nine pv ph)))

    (for* ([dv (in-range -1 2)] [dh (in-range -1 2)] #:unless (= 0 dv dh))
      (touch-point (+ pv dv) (+ ph dh))))

  index)

; part 1
(for/sum ([i (in-range 100)])
  (apply-step i))
; part 2
(for/last ([i (in-naturals)])
  #:break (= (apply-step i) (* v h))
  (+ i 100 2))
