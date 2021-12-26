#lang racket/base

(define slist (for/list ([t (in-lines)]) t))
(define v (length slist))
(define h (string-length (car slist)))

(define (access l x y)
  (string-ref (list-ref l y) x))
(define (change l x y val)
  (string-set! (list-ref l y) x val))
(define (exchange l nl x y nx ny)
  (change nl nx ny (access l x y))
  (change nl x y #\.)
  1)

(define (do-round l)
  (let ([nl (map string-copy l)])
    (values nl
            (+
             (for/sum ([y v])
               (for/sum ([x h])
                 (if (char=? (access l x y) #\>)
                     (let ([nx (if (= x (sub1 h)) 0 (add1 x))])
                       (if (char=? (access l nx y) #\.)
                           (exchange l nl x y nx y)
                           0))
                     0)))
             (let ([l (map string-copy nl)])
               (for/sum ([x h])
                 (for/sum ([y v])
                   (if (char=? (access l x y) #\v)
                       (let ([ny (if (= y (sub1 v)) 0 (add1 y))])
                         (if (char=? (access l x ny) #\.)
                             (exchange l nl x y x ny)
                             0))
                       0))))))))

(define (recurse l count)
  ;(printf "~a: ~a~n" count l)
  (let-values ([(nl moves) (do-round l)])
    (if (= moves 0)
        count
        (recurse nl (add1 count)))))

(recurse slist 1)
