#lang racket

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (x) (map * v x)) m))

(define (transpose mat)
  (accumulate-n
   (lambda (x y) (cons x y)) '() mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map
    (lambda (x) (map (lambda (y) (dot-product y x)) n))      m)))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init
                        (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define x '((1 2 3 4) (4 5 6 6) (6 7 8 9)))
(define y '(1 2 3 4))

(define w '((1 2 3) (4 5 6)))
(define q '((7 8 9) (10 11 12)))

(matrix-*-vector x y)

(transpose x)

(matrix-*-matrix w q)