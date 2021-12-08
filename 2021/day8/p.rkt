#lang racket/base
(require racket/string)
(require racket/list)

(define (cdrm l)
  (car (cdr l)))

(define (make-line t)
  (map string-split (string-split t " | ")))

(define slist (for/list ([t (in-lines)]) (make-line t)))

(define (is-1-4-7-8 v)
  (define l (string-length v))
  (or (= l 2) (or (= l 4) (or (= l 3) (or (= l 7))))))

; part 1
(for/sum ([s slist])
  (define digits (cdrm s))
  (for/sum ([v digits] #:when (is-1-4-7-8 v)) 1))

; part 2
(define (normalize s)
  (list->string (remove-duplicates (sort (string->list s) char<?) char=?)))

(define (unionize-list l)
  (normalize (string-join l "")))

(define (iterate-chars s)
  (string->list (normalize s)))

(define (correspond-length v)
  (define l (string-length v))
  (if (= l 2) 1 (if (= l 4) 4 (if (= l 3) 7 8))))

(for/sum ([s slist])
  (define pattern (car s))
  (define number-string (make-hash))
  (define string-number (make-hash))

  (define (add-number value number)
    (define s (normalize value))
    (hash-set! string-number s number)
    (hash-set! number-string number s))

  (define (unionize value number)
    (unionize-list (list (hash-ref number-string number) value)))

  ; pick out 1, 4, 7 and 8
  (for ([v pattern] #:when (is-1-4-7-8 v)) (define number (correspond-length v)) (add-number v number))

  (define proto-nine (unionize-list (list (hash-ref number-string 4) (hash-ref number-string 7))))
  ; pick out nine
  (for ([v pattern] #:when (= (string-length v) 6))
    (define union-proto-nine (unionize-list (list (hash-ref number-string 4) (hash-ref number-string 7) v)))
    (when (= (string-length union-proto-nine) (string-length v))
      (add-number v 9)))
  ; pick out three
  (for ([v pattern] #:when (= (string-length v) 5))
    (when (= (string-length (unionize v 1)) (string-length v))
      (add-number v 3)))
  ; pick out five and two
  (for ([v pattern] #:when (and (= (string-length v) 5) (not (hash-has-key? string-number (normalize v)))))
    (define four-list (string->list (hash-ref number-string 4)))
    (for ([c (iterate-chars v)])
      (set! four-list (remove c four-list)))
    (when (= (length four-list) 1)
      (add-number v 5))
    (when (= (length four-list) 2)
      (add-number v 2)))
  ; pick out zero and six
  (for ([v pattern] #:when (and (= (string-length v) 6) (not (hash-has-key? string-number (normalize v)))))
    (if (= (string-length (unionize v 7)) (string-length v))
        (add-number v 0)
        (add-number v 6)))

  ; final sum
  (for/sum ([v (cdrm s)] [multiplier '(1000 100 10 1)])
    (* multiplier (hash-ref string-number (normalize v)))))
