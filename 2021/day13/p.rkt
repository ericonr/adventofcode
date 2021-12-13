#lang racket/base
(require racket/string)
(require racket/list)
(require racket/set)

(define (make-line t)
  (let ([l (map string->number (string-split t ","))])
    (cons (car l) (second l))))

(define sset (mutable-set))
(define folds null)
(for ([t (in-lines)])
  (when (> (string-length t) 1)
    (if (string-prefix? t "fold along")
        (let ([l (string-split (string-trim t "fold along ") "=")])
          (set! folds (append folds (list (cons (car l) (string->number (second l)))))))
        (let ([l (make-line t)])
          (set-add! sset l)))))

(define (perform-fold dots fold)
  (define p (cdr fold))
  (define (fix-up pos)
    (- (* 2 p) pos))
  (for/set ([f dots])
    (if (equal? (car fold) "y")
        (if (< (cdr f) p)
            f
            (if (= (cdr f) p)
                null
                (cons (car f) (fix-up (cdr f)))))
        (if (< (car f) p)
            f
            (if (= (car f) p)
                null
                (cons (fix-up (car f)) (cdr f)))))))

; part 1
(set-count (perform-fold sset (car folds)))

; part 2
(for ([fold folds])
  (set! sset (perform-fold sset fold)))

(define mx (apply max (for/list ([f sset]) (car f))))
(define my (apply max (for/list ([f sset]) (cdr f))))
(for ([y (in-range (add1 my))])
  (display "\n")
  (for ([x (in-range (add1 mx))])
    (if (set-member? sset (cons x y))
        (display "#")
        (display "."))))
