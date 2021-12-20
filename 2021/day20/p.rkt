#lang racket/base
(require racket/match)

(define algo (read-line))
(define _ (read-line))

(define shash (make-hash))
(define max-dimension 0)

(for ([t (in-lines)] [i (in-naturals)])
  (set! max-dimension (add1 i))
  (for ([c t] [j (in-naturals)])
    (hash-set! shash (cons j i) c)))

(define number-of-pixels 9)
(define (pixel-positions n)
  (match n
    [8 (values -1 -1)]
    [7 (values 0 -1)]
    [6 (values 1 -1)]
    [5 (values -1 0)]
    [4 (values 0 0)]
    [3 (values 1 0)]
    [2 (values -1 1)]
    [1 (values 0 1)]
    [0 (values 1 1)]))

(define (replicant-finding sh start size other-positions)
  (for*/hash ([x (in-range start (+ start size))] [y (in-range start (+ start size))])
    (let ([index
           (for/sum ([i number-of-pixels])
             (let-values ([(dx dy) (pixel-positions i)])
               (arithmetic-shift
                (if (char=? (hash-ref sh (cons (+ x dx) (+ y dy)) other-positions) #\#)
                    1
                    0)
                i)))])
      (values (cons x y) (string-ref algo index)))))

(define (display-image sh start size)
  (for ([y (in-range start (+ start size))])
    (for ([x (in-range start (+ start size))])
      (display (hash-ref sh (cons x y))))
    (printf "~n")))

(define (enhance sh times start size other-positions)
  (let* ([start (- start 2)]
         [size (+ size 4)]
         [new-hash (replicant-finding sh start size other-positions)])
    ;(display-image new-hash start size)
    (if (> times 1)
        (enhance new-hash (sub1 times) start size (if (char=? other-positions #\#) (string-ref algo #x1ff) (string-ref algo 0)))
        new-hash)))

(define (lit-pixels sh)
  (length (remove* '(#\.) (hash-values sh))))

; part 1
(lit-pixels (enhance shash 2 0 max-dimension #\.))
; part 2
(lit-pixels (enhance shash 50 0 max-dimension #\.))
