#lang racket

(define (for-each-my f list)
  (cond ((not (null? list))
         (begin
           (f (car list))
           (for-each-my f (cdr list))))))
        
(for-each (lambda (x)
            (newline)
            (display x))
          (list 57 321 88))

(newline)

(for-each-my (lambda (x)
            (newline)
            (display x))
          (list 57 321 88))