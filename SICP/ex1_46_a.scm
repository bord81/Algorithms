(define (iterative-improve g-enough? impr x)
    (lambda (guess)
    (let loop ((g guess))
        (if (g-enough? g x)
            g
            (loop (impr g x))))))

(define (improve guess x) 
    (average guess (/ x guess)))

(define (good-enough? guess x)
    (< (abs (- (square guess) x)) 0.001))
    
(define (average x y) 
    (/ (+ x y) 2))

(define (sqrt x) ((iterative-improve good-enough? improve x) 1.0))
