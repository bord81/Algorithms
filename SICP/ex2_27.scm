#lang racket

(define (deep-reverse l)
  (if (not (pair? l))
      l
      (let loop ((n-l (list))
                 (i-l l))
        (if (null? i-l)
            n-l
            (loop (cons (deep-reverse (car i-l)) n-l) (cdr i-l))))))


(define (reverse l)
  (let loop ((n-l (list))
             (i-l l))
    (if (null? i-l)
        n-l
        (loop (cons (car i-l) n-l) (cdr i-l)))))

(define x (list (list 1 2) (list 3 4)))

(reverse x)
(reverse (list 1 4 9 16 25))
(deep-reverse (list 1 4 9 16 25))
(deep-reverse x)