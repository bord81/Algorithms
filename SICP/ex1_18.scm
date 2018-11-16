(define (* a b)
    (if (= a (min a b))
      (peasant-mult a b 0 0)
      (peasant-mult b a 0 0)))

(define (peasant-mult a b p n)
    (cond ((= a 0) (- p n))
        ((even? a) (peasant-mult (halve a) (double b) (+ p b) (+ n b)))
        (else (peasant-mult (halve a) (double b) (+ p b) n))))

(define (double x)
    (+ x x))

(define (halve x)
    (truncate (/ x 2)))

(define (min x y)
    (cond ((< x y) x)
        ((> x y) y)
        (else x)))

(define (even? x)
    (= (remainder x 2) 0))
