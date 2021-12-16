#lang racket/base
(require racket/match)

(define ss (read-line))
(define slen (* (string-length ss) 4))
(define sn (string->number ss 16))

; part 1
(define version-sum 0)

; l=list p=initial_position n=number_of_bits
(define (get-value-bits l p n)
  (let* ([base (- slen p)] [base-start (- base n)])
    (bitwise-bit-field l base-start base)))
(define (is-bit-set? l p)
  (bitwise-bit-set? l (- slen p 1)))

; l=number p=initial_position
; returns (values new_position return_value)
(define (advance-in-list l p)
  (define (advance-p! n)
    (set! p (+ p n)))

  ; get version
  (set! version-sum (+ version-sum (get-value-bits l p 3)))
  (advance-p! 3)

  ; get type ID
  (define type-id (get-value-bits l p 3))
  (advance-p! 3)

  (define v (match type-id
              ; literal value
              [4
               (define sl (reverse (for/list ([i (in-naturals)] #:final (not (is-bit-set? l p)))
                                     (define r (begin0 (get-value-bits l p 5) (advance-p! 5)))
                                     (if (>= r 16)
                                         (- r 16)
                                         r))))
               (for/sum ([r sl] [i (in-naturals)])
                 (* r (expt 2 (* i 4))))
               ]
              ; one of the operators
              [operator
               (define is-bits #t)
               (define out-len
                 (let ([c (is-bit-set? l p)])
                   (advance-p! 1)
                   (if (not c)
                       ; total length in bits
                       (let ([len (get-value-bits l p 15)])
                         (advance-p! 15)
                         len)
                       ; number of subpackets
                       (let ([num (get-value-bits l p 11)])
                         (advance-p! 11)
                         (set! is-bits #f)
                         num))))

               (define r
                 (if is-bits
                     (let ([pp p])
                       (for/list ([i (in-naturals)] #:break (= (- p pp) out-len))
                         (match-let-values ([(mp mr) (advance-in-list l p)])
                                           (set! p mp)
                                           mr)))
                     (let ([sub-count 0])
                       (for/list ([i (in-naturals)] #:break (= sub-count out-len))
                         (match-let-values ([(mp mr) (advance-in-list l p)])
                                           (set! p mp)
                                           (set! sub-count (add1 sub-count))
                                           mr)))
                     ))

               ; apply operators
               (match operator
                 [0 (apply + r)]
                 [1 (apply * r)]
                 [2 (apply min r)]
                 [3 (apply max r)]
                 [5 (if (apply > r) 1 0)]
                 [6 (if (apply < r) 1 0)]
                 [7 (if (apply = r) 1 0)])
               ]
              ))

  (values p v))

; returns result for part 2 and goes through input to update version sum
(advance-in-list sn 0)
; part 1
version-sum
