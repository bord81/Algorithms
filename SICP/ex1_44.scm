(define (smooth f)
    (lambda (x) (/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3)))

(define (repeated f n)
    (cond
        ((= n 1) (lambda (x) (f x)))
        ((> n 1) (compose f (repeated f (- n 1))))))

(define (compose f g) 
    (lambda (x) (f (g x))))

(define dx 0.00001)
