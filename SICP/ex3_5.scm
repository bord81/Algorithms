#lang racket

(define (estimate-integral pred trials x1 x2 y1 y2)
  (monte-carlo trials (lambda ()
                        (if (pred (random-in-range x1 x2) (random-in-range y1 y2))
                            #t
                            #f))))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1)
                 (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1)
                 trials-passed))))
  (iter trials 0))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (in-unit-circle? x1 x2 y1 y2)
  (lambda (x y)
    (let ((test (+ (* x x) (* y y))))
      (if (or (< test 100) (= test 100))
          #t
          #f))))

(exact->inexact (* 400 (estimate-integral (in-unit-circle? -100 100 -100 100) 1000000 -100 100 -100 100)))