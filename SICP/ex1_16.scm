(define (my-fast-expt b n)
    (fast-expt-invar 1 b n))

(define (fast-expt-invar a b n) 
    (cond ((= n 0) a)
    ((even? n) (fast-expt-invar (* a (square b)) b (- n 2)))
    ((> a 1) (* (square b) (fast-expt-invar a b (- n 1))))
    (else (* b (fast-expt-invar a b (- n 1))))))

(define (even? n)
    (= (remainder n 2) 0)) 
