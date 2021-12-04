#lang racket

; bingo table size
(define size 5)

(define nlist (map (lambda(x)(string->number x)) (string-split (read-line) ",")))

(define (make-line t)
  (define l (string-split t))
  (for/vector ([v l])
				  (mcons (string->number v) 0)))

(define slist null)
(for ([t (in-lines)])
	  (define entry (list->vector (map (lambda(x)(make-line x)) (for/list ([c (in-range size)]) (read-line)))))
	  (set! slist (append slist (list entry))))

(define (accessb b l c)
  (vector-ref (vector-ref b l) c))

(define (check-board b)
  (define flag #f)
  (for ([v (in-range size)] #:break flag)
		 (define count-l (for/sum ([index (in-range size)]) (mcdr (accessb b v index))))
		 (define count-c (for/sum ([index (in-range size)]) (mcdr (accessb b index v))))
		 (set! flag (or (equal? count-c size) (equal? count-l size))))
  flag)

(define (find-value b v)
  (define answer null)
  (for ([l (in-range size)] #:break (not (null? answer)))
		 (for ([c (in-range size)] #:break (not (null? answer)))
				(when (equal? (mcar (accessb b l c)) v)
				  (set! answer (cons l c)))))
  answer)

(define (get-points b n)
  (* n (for*/sum ([l (in-range size)] [c (in-range size)])
					  (define v (accessb b l c))
					  (if (equal? (mcdr v) 1) 0 (mcar v)))))

; we'll pick the first and last results from this
(for ([n nlist] #:break (null? slist))
	  (for ([b slist])
			 (define p (find-value b n))
			 (unless (null? p)
				(set-mcdr! (vector-ref (vector-ref b (car p)) (cdr p)) 1))
			 (when (check-board b)
				(set! slist (remq b slist))
				(display "\nwon: ")
				(display (get-points b n)))))
