(define (nth-root x n)
    (cond
        ((> n 1)
            (fixed-point ((repeated average-damp (find-exp n)) (lambda (y) (/ x  (repeated-mult y (- n 1)))))
                            1.0))))

(define (repeated f n)
    (cond
        ((= n 1) (lambda (x) (f x)))
        ((> n 1) (compose f (repeated f (- n 1))))))

(define (compose f g) 
    (lambda (x) (f (g x))))

(define (repeated-mult x n)
    (cond
        ((= n 1) x)
        ((> n 1) (* (repeated-mult x (- n 1)) x))))

(define (find-exp x)
    (cond
        ((< x 2) 0)
        ((> x 0) (+ 1 (find-exp (/ x 2))))))

(define (average-damp f)
    (lambda (x) (average x (f x))))

(define (average x y)
    (/ (+ x y) 2))

(define tolerance 0.00001)
(define (fixed-point f first-guess)
    (define (close-enough? v1 v2) (< (abs (- v1 v2))
        tolerance)) 
    (define (try guess)
        (let ((next (f guess)))
        (if (close-enough? guess next)
            next
            (try next))))
    (try first-guess))
