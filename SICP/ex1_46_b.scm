(define (iterative-improve g-enough? impr)
    (lambda (guess)
    (let loop ((g guess) (imp (impr guess)))
        (if (g-enough? g imp)
            imp
            (loop imp (impr imp))))))
            
(define tolerance 0.00001)            
(define (fixed-point f first-guess)
(define (close-enough? v1 v2) (< (abs (- v1 v2))
        tolerance)) 
    ((iterative-improve close-enough? f) first-guess))
