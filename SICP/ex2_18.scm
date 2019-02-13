#lang racket

(define (reverse l)
  (let loop ((n-l (list))
             (i-l l))
    (if (null? i-l)
        n-l
        (loop (cons (car i-l) n-l) (cdr i-l)))))

(reverse (list 1 4 9 16 25))