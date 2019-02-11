#lang racket

(define (make-interval a b) (cons a b))

(define (upper-bound x) (cdr x))

(define (lower-bound x) (car x))

(define (width x)
  (/ (- (upper-bound x) (lower-bound x)) 2))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
    (+ (upper-bound x) (upper-bound y))))	

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (if (not (= (width y) 0))
      (mul-interval
       x
       (make-interval (/ 1.0 (upper-bound y))
                      (/ 1.0 (lower-bound y))))
      #f))

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (lower-bound y))
                 (- (upper-bound x) (upper-bound y))))

(define x (make-interval 5 15))
(define y (make-interval 1 1))
(define z (make-interval 1 3))

(width y)
(width z)
(width x)
(width (div-interval x z))
(width (div-interval y x))
(width (div-interval x y))