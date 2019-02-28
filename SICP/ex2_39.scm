#lang racket

(define (reverse-f-r sequence)
  (fold-right (lambda (x y)
                (cond ((null? y) (list x))
                      (else (append y (list x))))) '() sequence))

(define (reverse-f-l sequence)
  (fold-left (lambda (x y)
               (cons y x) ) '() sequence))

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(define (fold-right op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (fold-right op initial (cdr sequence)))))

(define (reverse-2-18 l)
  (let loop ((n-l (list))
             (i-l l))
    (if (null? i-l)
        n-l
        (loop (cons (car i-l) n-l) (cdr i-l)))))

(reverse-2-18 (list 1 4 9 16 25))
(reverse-f-r (list 1 4 9 16 25))
(reverse-f-l (list 1 4 9 16 25))