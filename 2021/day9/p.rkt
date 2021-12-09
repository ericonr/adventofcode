#lang racket/base
(require racket/set)

(define (char->number c) (string->number (string c)))

(define (make-line t)
  (list->vector (map char->number (string->list t))))

(define sv (for/vector ([t (in-lines)]) (make-line t)))

(define v (vector-length sv))
(define h (vector-length (vector-ref sv 0)))

(define (access pv ph)
  (vector-ref (vector-ref sv pv) ph))

(define (can-decrease p) (> p 0))
(define (can-increase-v pv) (< pv (sub1 v)))
(define (can-increase-h ph) (< ph (sub1 h)))

(define (lowest-point pv ph)
  (define me (access pv ph))
  (and
   (if (can-decrease pv)
       (< me (access (sub1 pv) ph))
       #t)
   (if (can-increase-v pv)
       (< me (access (add1 pv) ph))
       #t)
   (if (can-decrease ph)
       (< me (access pv (sub1 ph)))
       #t)
   (if (can-increase-h ph)
       (< me (access pv (add1 ph)))
       #t)))

; part 1
(for*/sum ([vi (in-range v)] [hi (in-range h)] #:when (lowest-point vi hi))
  (add1 (access vi hi)))

; part 2
(define sl (sort (for*/list ([vi (in-range v)] [hi (in-range h)] #:when (lowest-point vi hi))
                   (define pursued-set (mutable-set))

                   (define (recurse pv ph)
                     (define pos (cons pv ph))
                     (unless (or (set-member? pursued-set pos) (= (access pv ph) 9))
                       (set-add! pursued-set pos)
                       (when (can-decrease pv) (recurse (sub1 pv) ph))
                       (when (can-increase-v pv) (recurse (add1 pv) ph))
                       (when (can-decrease ph) (recurse pv (sub1 ph)))
                       (when (can-increase-h ph) (recurse pv (add1 ph)))))

                   (recurse vi hi)
                   (set-count pursued-set))
                 >))
(* (list-ref sl 0) (* (list-ref sl 1) (list-ref sl 2)))
