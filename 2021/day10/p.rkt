#lang racket/base

; part 1 info
(define points-corrupt #hash((#\) . 3) (#\] . 57) (#\} . 1197) (#\> . 25137)))
(define pair-match #hash((#\) . #\() (#\] . #\[) (#\} . #\{) (#\> . #\<)))
; part 2 info
(define points-complete #hash((#\( . 1) (#\[ . 2) (#\{ . 3) (#\< . 4)))

; part 1 vars
(define corrupt-points null)
; part 2 vars
(define complete-points null)

(for ([t (in-lines)])
  (define lt (string->list t))
  (define open-chars null)

  (define p 0)
  (for ([c lt] #:break (not (= p 0)))
    (if (hash-has-key? points-corrupt c)
        ; close char
        (begin
          (unless (char=? (hash-ref pair-match c) (car open-chars))
            (set! p (hash-ref points-corrupt c)))
          ; pop element, since it was correctly matched
          (set! open-chars (cdr open-chars)))
        ; open char
        (set! open-chars (append (list c) open-chars))))

  (set! corrupt-points (append corrupt-points (list p)))

  ; incomplete and not corrupted
  (when (and (> (length open-chars) 0) (= p 0))
    (define cp 0)
    (for ([c open-chars])
      (set! cp (+ (* cp 5) (hash-ref points-complete c))))
    (set! complete-points (append complete-points (list cp)))))

; part 1 result
(apply + corrupt-points)
; part 2 result
(define complete-points-sorted (sort complete-points <))
(define middle (floor (/ (length complete-points-sorted) 2)))
(list-ref complete-points-sorted middle)
