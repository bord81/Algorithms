(define (cons-prod x y)
    (* (fast-expt 2 x) (fast-expt 3 y)))

(define (car-prod z)
(let loop ((n 0)
            (x z))
    (if (= (remainder x 2) 0)
        (loop (+ n 1) (/ x 2))
        n)))

(define (cdr-prod z)
    (let loop ((n 0)
                (x z))
    (if (= (remainder x 3) 0)
        (loop (+ n 1) (/ x 3))
        n)))


(define (fast-expt b n) 
    (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2)))) 
        (else (* b (fast-expt b (- n 1))))))
