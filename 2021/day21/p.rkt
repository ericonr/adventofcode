#lang racket/base
(require racket/match)

; puzzle input (position . score)
(define starting-positions-scores '((6 . 0) (2 . 0)))

; utility functions
(define (switch-pos p) (match p [0 1] [1 0]))
(define (bind-value v limit)
  (add1 (remainder (sub1 v) limit)))

; part 1
(define max-score 1000)

; stateful dice
(define deterministic-dice 1)
(define dice-rolls 0)
(define (roll-deterministic-dice)
  (set! dice-rolls (add1 dice-rolls))
  (begin0 deterministic-dice
          (set! deterministic-dice (bind-value (add1 deterministic-dice) 100))))

(define (roll-three-times)
  (for/sum ([_ 3]) (roll-deterministic-dice)))

(define (run-round positions-scores)
  (define breaking #f)
  (for/list ([ps positions-scores])
    (if breaking
        ps
        (match-let* ([(cons p s) ps]
                     [v (roll-three-times)]
                     [np (bind-value (+ p v) 10)]
                     [ns (+ np s)])
          (set! breaking (>= ns max-score))
          (cons np ns)))))

(define (check-for-max positions-scores score)
  (define pos #f)
  (for ([ps positions-scores] [i (in-naturals)])
    (when (>= (cdr ps) score)
      (set! pos i)))
  pos)

(define (run-until positions-scores)
  (let* ([new-positions-scores (run-round positions-scores)]
         [pos (check-for-max new-positions-scores max-score)])
    (if pos
        (cdr (list-ref new-positions-scores (switch-pos pos)))
        (run-until new-positions-scores))))

; part 1 answer
(* (run-until starting-positions-scores) dice-rolls)

; part 2
(define max-score-2 21)

; how many ways there are to roll value v
(define (weight-of-roll v)
  (match v
    [3 1]
    [4 3]
    [5 6]
    [6 7]
    [7 6]
    [8 3]
    [9 1]))

(define (run-round-2-individual positions-scores roll pos)
  (let ([out #f])
    (values
     (for/list ([ps positions-scores] [i (in-naturals)])
       (if (not (= pos i))
           ps
           (match-let* ([(cons p s) ps]
                        [np (bind-value (+ p roll) 10)]
                        [ns (+ np s)])
             (set! out (>= ns max-score-2))
             (cons np ns))))
     out)))

(define rv (vector 0 0))
(define (run-until-2 positions-scores value current-roll)
  (for ([r (in-range 3 10)])
    (let-values ([(new-positions-scores pos) (run-round-2-individual positions-scores r current-roll)]
                 [(weight) (* value (weight-of-roll r))])
      (if pos
          (vector-set! rv current-roll (+ (vector-ref rv current-roll) weight))
          (run-until-2 new-positions-scores weight (switch-pos current-roll))))))

(run-until-2 starting-positions-scores 1 0)
; part 2 answer
(apply max (vector->list rv))
