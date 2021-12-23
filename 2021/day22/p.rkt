#lang racket/base
(require racket/string)
(require racket/list)
(require racket/match)

(define (make-line t)
  (let ([turn-on (string-prefix? t "on")]
        [coords (string-trim (string-trim t "off ") "on ")])
    (cons turn-on
          (for/list ([s (string-split coords ",")])
            (let* ([ns (string-trim (string-trim (string-trim s "x=") "y=") "z=")]
                   [pos (string-split ns "..")])
              (cons (string->number (first pos)) (string->number (second pos))))))))

(define slist (for/list ([t (in-lines)]) (make-line t)))

(define (my-and a b) (and a b))
(define (my-true? a) a)

(define (unpack-coord c)
  (values (first c) (second c) (third c)))

; part 1
(define (check-coord c)
  (let ([max-coord 50])
    (or (<= (- max-coord) (car c) max-coord) (<= (- max-coord) (cdr c) max-coord))))

(let ([positions (make-hash)])
  (for ([s slist])
    (let* ([turn-on (car s)]
           [coords (cdr s)]
           [xs (first coords)]
           [ys (second coords)]
           [zs (third coords)])
      (when (foldl my-and #t (map check-coord coords))
        (for* ([x (in-inclusive-range (car xs) (cdr xs))]
               [y (in-inclusive-range (car ys) (cdr ys))]
               [z (in-inclusive-range (car zs) (cdr zs))])
          (hash-set! positions (list x y z) turn-on)))))
  (length (filter my-true? (hash-values positions))))

; part 2
(define (find-overlap coord1 coord2)
  (define (overlap-1d v1 v2)
    (if (or (<= (car v1) (car v2) (cdr v1)) (<= (car v2) (car v1) (cdr v2)))
        (cons (max (car v1) (car v2)) (min (cdr v1) (cdr v2)))
        #f))
  (define overlap (for/list ([v1 coord1] [v2 coord2]) (overlap-1d v1 v2)))
  (if (foldl my-and #t overlap)
      overlap
      #f))

(define (volume c)
  (for/product ([v c]) (add1 (- (cdr v) (car v)))))

(define (find-difference-list ori sub)
  (let ([dstart (cons (car ori) (sub1 (car sub)))]
        [dend (cons (add1 (cdr sub)) (cdr ori))])
    (if (and (= (car ori) (car sub)) (= (cdr ori) (cdr sub)))
        (list null sub null)
        (if (= (car ori) (car sub))
            (list null sub dend)
            (if (= (cdr ori) (cdr sub))
                (list dstart sub null)
                (list dstart sub dend))))))

(define (split-cuboid c intersection)
  (define diff-list (for/list ([ci c] [ii intersection]) (find-difference-list ci ii)))
  ;(printf "~a ~a = ~a~n" c intersection diff-list)
  (filter my-true? (for*/list
                       ([x (in-range 3)]
                        [y (in-range 3)]
                        [z (in-range 3)])
                     (let ([xe (list-ref (list-ref diff-list 0) x)]
                           [ye (list-ref (list-ref diff-list 1) y)]
                           [ze (list-ref (list-ref diff-list 2) z)])
                       (if (or (null? xe) (null? ye) (null? ze) (= x y z 1))
                           #f
                           (list xe ye ze))))))

(define (add-block-recursive v l)
  (let ([t (car v)] [c (cdr v)])
    (if (null? l)
        null
        (let* ([old-block (car l)]
               [intersection (find-overlap c old-block)])
          (if intersection
              (append
               (split-cuboid old-block intersection)
               (add-block-recursive v (cdr l)))
              (append
               (list old-block)
               (add-block-recursive v (cdr l))))))))

(define (go-through input-list result-list)
  (if (null? input-list)
      result-list
      (let ([v (car input-list)]
            [next-input (cdr input-list)])
        (go-through next-input (append (if (car v) (list (cdr v)) null) (add-block-recursive v result-list))))))

(let ([results (go-through slist null)])
  (for/sum ([r results])
    (volume r)))
