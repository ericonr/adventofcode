#lang racket/base
(require racket/string)
(require racket/list)
(require racket/set)

(define (cdrm l)
  (car (cdr l)))

(define (make-line t)
  (define r (map string-split (string-split t " | ")))
  (define (string->set s) (list->set (string->list s)))
  (cons (map string->set (car r)) (map string->set (cdrm r))))

(define slist (for/list ([t (in-lines)]) (make-line t)))

(define (is-1-4-7-8 v)
  (define l (set-count v))
  (or (= l 2) (or (= l 4) (or (= l 3) (or (= l 7))))))

; part 1
(for/sum ([s slist])
  (define digits (cdr s))
  (for/sum ([v digits] #:when (is-1-4-7-8 v)) 1))

; part 2
(define (correspond-length v)
  (define l (set-count v))
  (if (= l 2) 1 (if (= l 4) 4 (if (= l 3) 7 8))))

(for/sum ([s slist])
  (define pattern (car s))
  (define number-string (make-hash))
  (define string-number (make-hash))

  (define (add-number value number)
    (hash-set! string-number value number)
    (hash-set! number-string number value))

  (define (unionize value number)
    (set-union (hash-ref number-string number) value))

  ; pick out 1, 4, 7 and 8
  (for ([v pattern] #:when (is-1-4-7-8 v)) (define number (correspond-length v)) (add-number v number))

  ; pick out nine
  (for ([v pattern] #:when (= (set-count v) 6))
    (define union-proto-nine (set-union (hash-ref number-string 4) (hash-ref number-string 7) v))
    (when (= (set-count union-proto-nine) (set-count v))
      (add-number v 9)))
  ; pick out three
  (for ([v pattern] #:when (= (set-count v) 5))
    (when (= (set-count (unionize v 1)) (set-count v))
      (add-number v 3)))
  ; pick out five and two
  (for ([v pattern] #:when (and (= (set-count v) 5) (not (hash-has-key? string-number v))))
    (define four-set (hash-ref number-string 4))
    (when (= (set-count (set-subtract four-set v)) 1)
      (add-number v 5))
    (when (= (set-count (set-subtract four-set v)) 2)
      (add-number v 2)))
  ; pick out zero and six
  (for ([v pattern] #:when (and (= (set-count v) 6) (not (hash-has-key? string-number v))))
    (if (= (set-count (unionize v 7)) (set-count v))
        (add-number v 0)
        (add-number v 6)))

  ; final sum
  (for/sum ([v (cdr s)] [multiplier '(1000 100 10 1)])
    (* multiplier (hash-ref string-number v))))
