#lang racket/base
(require racket/string)
(require racket/set)

(define (cdrm l)
  (car (cdr l)))

(define (make-line t)
  (sort (string-split t "-") string<?))

(define sset (for/set ([t (in-lines)]) (make-line t)))

(define caves (for/mutable-set ([s sset]) (car s)))
(set-union! caves (for/set ([s sset]) (cdrm s)))
(set-subtract! caves (set "start" "end"))

(define (is-small s)
  (char-lower-case? (string-ref s 0)))

(define (valid-path p1 p2)
  (set-member? sset (sort (list p1 p2) string<?)))

(define (cannot-use-cave c used)
  ;(hash-has-key? used c)) ; part 1
  (and (hash-has-key? used c) (hash-has-key? used 0))) ; part 2

(define (hash-handle! used c)
  (hash-update! used c add1 0)
  (when (= (hash-ref used c) 2)
    (hash-set! used 0 #t)))

(for/sum ([cave caves])
  (define (recurse-path path used c)
    (when (is-small c) (hash-handle! used c))
    (set! path (append path (list c)))
    (+ (if (valid-path "end" c) 1 0)
       (for/sum ([nc caves]
                 #:unless (or (equal? nc c) (cannot-use-cave nc used) (not (valid-path c nc))))
         (recurse-path path (hash-copy used) nc))))

  (if (valid-path "start" cave)
      (recurse-path null (make-hash) cave)
      0))
