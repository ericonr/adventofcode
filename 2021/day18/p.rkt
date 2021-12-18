#lang racket/base
(require racket/string)
(require racket/match)

(define slist
  (let-values ([(in out) (make-pipe)])
    (for/list ([t (in-lines)])
      (display (string-replace t "," " . ") out)
      (read in))))

(define (reduce-number v)
  (define (get-explosion vl depth position)
    (define explode (= depth 4))
    (define (recurse fn p)
      (let* ([nc (fn vl)]
             [np (add1 p)])
        (if (pair? nc)
            (get-explosion nc (add1 depth) p)
            (begin
              ;(printf "nc np ~a ~a~n" nc np)
              (values explode np nc (cdr vl))))))
    (match/values (recurse car position)
                  [(#t p a b) (values #t p a b)]
                  [(#f p a b) (recurse cdr p)]))

  (define-values (explode where-explode v1 v2) (get-explosion v 0 0))
  ;(printf "explosion ~a ~a~n" explode where-explode)

  (define (traverse-number vl position)
    ;(printf "~a ~a~n" vl position)
    (define (recurse fn p)
      (let* ([nc (fn vl)]
             [np (add1 p)])
        ;(printf "pair ~a~n" nc)
        (if (pair? nc)
            (traverse-number nc p)
            (values
             (if (not explode)
                 nc
                 (if (= (- where-explode 2) p)
                     (+ v1 nc)
                     (if (= (+ 1 where-explode) p)
                         (+ v2 nc)
                         nc)))
             np))))

    (if (and explode (not (pair? (car vl))) (not (pair? (cdr vl))) (= (add1 position) where-explode))
        (values 0 (+ 2 position))
        (let*-values ([(r p) (recurse car position)]
                      [(l p) (recurse cdr p)])
          ;(printf "new cons ~a ~a ~n" r l)
          (values (cons r l) p))))

  (define can-do #t)
  (define (split-number vl)
    (define (recurse fn)
      (let ([nc (fn vl)])
        (if (pair? nc)
            (split-number nc)
            (if (and can-do (>= nc 10))
                (let ([rdiv (/ nc 2)])
                  (set! can-do #f)
                  (cons (floor rdiv) (ceiling rdiv)))
                nc))))
    (cons
     (recurse car)
     (recurse cdr)))

  (let-values ([(nv p) (traverse-number v 0)])
    (if (equal? nv v)
        (let ([nv (split-number v)])
          (if (equal? nv v)
              v
              (reduce-number nv)))
        (reduce-number nv))))

(define (add-values v l)
  (if (null? l)
      v
      (add-values (reduce-number (cons v (car l))) (cdr l))))

(define (magnitude-number v)
  (define (recurse fn)
    (let ([nc (fn v)])
      (if (pair? nc)
          (magnitude-number nc)
          nc)))
  (+ (* 3 (recurse car)) (* 2 (recurse cdr))))

; part 1
(magnitude-number (add-values (car slist) (cdr slist)))

; part 2
(apply max (for*/list ([v1 slist] [v2 slist] #:unless (equal? v1 v2))
             (magnitude-number (add-values v1 (list v2)))))
