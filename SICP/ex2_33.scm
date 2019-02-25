#lang racket

(define (map-my p sequence)
  (accumulate (lambda (x y)
                (cons (p x) y)) '() sequence))

(define (append-my seq1 seq2)
  (accumulate cons
            seq2 seq1 ))

(define (length-my sequence)
  (accumulate
   (lambda (x y) (+ 1 y)) 0 sequence))


(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (filter predicate sequence)
  (cond ((null? sequence) '())
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (square x)
  (* x x))

(append-my '(1 2) '(5 4))

(length-my '(5 4 3 2))

(map-my square '(1 3 5 7))