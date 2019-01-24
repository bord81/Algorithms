(define (cons-e x y) 
    (lambda (m) (m x y)))

(define (car-e z)
    (z (lambda (p q) p)))

(define (cdr-e z)
    (z (lambda (p q) q)))
