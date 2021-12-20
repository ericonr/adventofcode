#lang racket/base
(require racket/string)
(require racket/list)
(require racket/set)
(require racket/match)
(require racket/vector)
(require math/matrix)

; using 3 instead of 12 still worked, thankfully!
(define common-beacons-min 3)
(define common-distances-min (/ (* common-beacons-min (sub1 common-beacons-min)) 2))

(define slist (for/list ([t (in-lines)])
                ; consumed scanner list
                (for/list ([t (in-lines)] #:break (< (string-length t) 2))
                  (list->matrix 3 1 (map string->number (string-split t ","))))))

(define (matrix-variation m x y z)
  (match m
    [0 (matrix [[x 0 0] [0 y 0] [0 0 z]])]
    [1 (matrix [[x 0 0] [0 0 y] [0 z 0]])]
    [2 (matrix [[0 x 0] [y 0 0] [0 0 z]])]
    [3 (matrix [[0 x 0] [0 0 y] [z 0 0]])]
    [4 (matrix [[0 0 x] [0 y 0] [z 0 0]])]
    [5 (matrix [[0 0 x] [y 0 0] [0 z 0]])]
    ))

(define (vector-s v)
  (sqrt (apply + (sort (map (lambda (x) (expt x 2)) (matrix->list v)) <))))

(define (vector-distances l)
  (for*/hash ([v1 l] [v2 l] #:unless (equal? v1 v2))
    (let ([v (matrix- v1 v2)])
      (values
       v
       (cons v1 v2)))))

(define (vector-dm l)
  (define out (make-hash))
  (for* ([v1 l] [v2 l] #:unless (equal? v1 v2))
    (let ([v (matrix- v1 v2)])
      (hash-update! out (vector-s v) add1 0)))
  out)

(define shash (map vector-distances slist))
(define sdhash (map vector-dm slist))
(define mainl (car slist))

(define sl (cdr slist))
(define sh (cdr shash))
(define sdh (cdr sdhash))

(for ([h shash] [dh sdhash])
  (printf "lengths ~a ~a ~n" (hash-count h) (hash-count dh)))

(define info-vector (make-vector (length slist) null))
(vector-set! info-vector 0 (cons (identity-matrix 3) (list->matrix 3 1 '[ 0 0 0 ])))

(define (run-through i)
  (let ([l (list-ref sl i)] [h (list-ref sh i)] [dh (list-ref sdh i)])
    (for ([ml slist] [mh shash] [mdh sdhash] [pi (in-naturals)]
                     #:unless (null? (vector-ref info-vector pi))
                     #:break (not (null? (vector-ref info-vector (add1 i)))))

      (let ([distance-matches (set-intersect (hash-keys mdh) (hash-keys dh))])
        (when (>= (set-count distance-matches) common-distances-min)

          (printf "starting ~a ~a~n" i pi)
          (let* ([mlist
                  (for*/list ([x (in-range -1 2 2)] [y (in-range -1 2 2)] [z (in-range -1 2 2)] [m 6])
                    (printf "iter: ~a ~a ~a ~a~n" x y z m)
                    (let ([ma (matrix-variation m x y z)])
                      (define (multiply v)
                        (matrix* ma v))
                      (let ([matches (set-intersect (hash-keys mh) (map multiply (hash-keys h)))])
                        (list matches x y z m))))]
                 [main-matches (sort mlist (lambda (x y) (> (set-count (car x)) (set-count (car y)))))])
            (define offset #f)
            (for/last ([main-match main-matches]
                       #:break offset)
              (match-let ([(list value-matches x y z m) main-match])
                (for ([v value-matches]
                      #:break offset)
                  (let* ([ma (matrix-variation m x y z)]
                         [ms (hash-ref mh v)]
                         [m1 (car ms)]
                         [m2 (cdr ms)]
                         [ps (hash-ref h (matrix* (matrix-inverse ma) v))]
                         [p1 (car ps)]
                         [p2 (cdr ps)])
                    (define (test-offset o)
                      (define (apply-offset p)
                        (matrix+ o (matrix* ma p)))
                      (>= (set-count (set-intersect ml (map apply-offset l))) common-beacons-min))
                    (define (calculate-offset m p)
                      (matrix- m (matrix* ma p)))
                    (set! offset
                          (if (test-offset (calculate-offset m1 p1))
                              (calculate-offset m1 p1)
                              (if (test-offset (calculate-offset m1 p2))
                                  (calculate-offset m1 p2)
                                  #f)))))
                (when offset
                  (printf "matrix ~a ~a ~a ~a offset ~a~n" m x y z offset)
                  (match-let ([(cons pma po) (vector-ref info-vector pi)])
                    (let ([ma (matrix-variation m x y z)])
                      (vector-set! info-vector (add1 i) (cons (matrix* pma ma) (matrix+ po (matrix* pma offset)))))))))
            ))
        ))))

(define (recurse)
  (printf "info-vector ~a~n" info-vector)
  (for-each run-through (range (length sl)))
  (when (vector-member null info-vector)
    (recurse)))
(recurse)

info-vector

; part 1
(let ([position-set (mutable-set)])
  (for ([p mainl])
    (set-add! position-set p))
  (for ([l sl] [i (in-naturals 1)])
    (match-let ([(cons ma offset) (vector-ref info-vector i)])
      (for ([p l])
        (set-add! position-set (matrix+ offset (matrix* ma p))))))
  (printf "set ~a~n" position-set)
  (set-count position-set))

; part 2
(let ([manhattans
       (for*/list ([i info-vector] [j info-vector] #:unless (equal? i j))
         (match-let ([(cons ima io) i]
                     [(cons jma jo) j])
           (for/sum ([c (matrix->list (matrix- io jo))]) (abs c))))])
  (apply max manhattans))
