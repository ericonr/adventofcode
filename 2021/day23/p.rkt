#lang racket/base
(require racket/list)
(require racket/match)
(require racket/math)
(require racket/vector)

; A=0 ... D=3
;(define original-org '( (1 . 0) (2 . 3) (1 . 2) (3 . 0) )) ; test input
(define original-org '( (0 . 2) (3 . 2) (0 . 3) (1 . 1) )) ; puzzle input

(define indexes '(2 4 6 8))
(define anti-indexes (remove* indexes (range 11)))

(define (index-value x)
  (* (add1 x) 2))

(define dim-x 11)
(define dim-y 3)

(define (valid-position x y)
  (or (= y 0) (member x indexes)))

(define pos (for/vector ([_ dim-y]) (make-vector dim-x 'empty)))

(define (copy-maps maps)
  (for/vector ([vec maps]) (vector-copy vec)))

(define (access maps x y)
  (vector-ref (vector-ref maps y) x))
(define (change maps x y v)
  (vector-set! (vector-ref maps y) x v))
(define (empty? maps x y)
  (equal? (access maps x y) 'empty))

; populate pos with initial values
(for ([p original-org] [i (in-naturals)])
  (let ([index (index-value i)])
    (change pos index 1 (car p))
    (change pos index 2 (cdr p))))

(define (check-path maps from to)
  (match-let*
      ([(cons x y) from]
       [(cons nx ny) to]
       [valid-path #t]
       [dx (sgn (- nx x))])
    (for ([xi (in-range (+ x dx) nx dx)] #:break (not valid-path))
      (unless (empty? maps xi 0)
        (set! valid-path #f)))
    valid-path))

(define (move-fish maps from to)
  (match-let*
      ([(cons x y) from]
       [(cons nx ny) to]
       [v (access maps x y)])
    (change maps x y 'empty)
    (change maps nx ny v)
    (* (expt 10 v) (+ (abs (- x nx)) (abs (- y ny))))))

(define (map-done maps)
  (for/and ([i 4])
    (let ([index (index-value i)])
      (= i (access maps index 1) (access maps index 2)))))

(define (available-moves maps)
  (filter (lambda (a) (not (null? (cdr a))))
          (for*/list ([x dim-x] [y dim-y] #:when (and (valid-position x y) (not (empty? maps x y))))
            (cons
             (cons x y)
             (filter (lambda (a) a)
                     (let ([v (access maps x y)])
                       (if (= y 0)
                           (list
                            (if (empty? maps (index-value v) 2)
                                (cons (index-value v) 2)
                                (if (and (empty? maps (index-value v) 1) (equal? (access maps (index-value v) 2) v))
                                    (cons (index-value v) 1)
                                    #f)))
                           (if (or
                                (and (= y 2) (not (= (index-value v) x)) (empty? maps x 1))
                                (and (= y 1) (or
                                              (not (= (index-value v) x))
                                              (let ([nv (access maps x 2)]) (not (= (index-value nv) x))))))
                               (for/list ([xi anti-indexes])
                                 (if (empty? maps xi 0)
                                     (cons xi 0)
                                     #f))
                               null))))
             ))))

(define (find-solution maps)
  (define min-cost +inf.0)
  (define (rec m total-cost)
    (when (< total-cost min-cost)
      (let ([moves (available-moves m)])
        (if (null? moves)
            (begin
              (when (and (map-done m) (< total-cost min-cost))
                (set! min-cost total-cost)))
            (for ([move moves])
              (let ([from (car move)])
                (for ([to (cdr move)])
                  (when (check-path m from to)
                    (let* ([nm (copy-maps m)]
                           [move-cost (move-fish nm from to)])
                      (rec nm (+ total-cost move-cost)))))))))))
  (rec maps 0)
  min-cost)

(find-solution pos)
