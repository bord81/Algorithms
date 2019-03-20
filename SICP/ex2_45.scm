#lang planet neil/sicp

(define (split func-1 func-2)
  (lambda (painter n)
    (if (= n 0)
        painter
        (let ((smaller ((split func-1 func-2) painter (- n 1))))
          (func-1 painter (func-2 smaller smaller))))))

(define my-right-split (split beside below))
(define my-up-split (split below beside))

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

(paint (right-split einstein 2))
(paint (up-split einstein 2))

(paint (my-right-split einstein 2))
(paint (my-up-split einstein 2))

