(define (miller-rabin-test n k)
    (cond ((< n 2) #f)
        ((= n 2) #t)
        ((even? n) #f)
        ((> k 0) 
            (if (not (find-d n (- n 1) 0 (random (- n 1))))
                #f
                (miller-rabin-test n (- k 1))))
        (else #t)))

(define (find-d n d s a)
    (if (even? d)
        (find-d n (/ d 2) (+ s 1) a)
        (let ((x (expmod a d n)))
            (if (or (= x -1) (= x 1) (= x (- n 1)))
                #t
                (second-check a d n s)))))

(define (second-check base exp m s)
    (cond ((> s 0)
        (let ((x (expmod base (* exp (fast-expt 2 (- s 1))) m)))
        (cond ((= x (- m 1)) #t)
            ((= x -1) #t)
            (else (second-check base exp m (- s 1))))))
        (else #f)))

(define (fast-expt b n) 
    (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2)))) 
        (else (* b (fast-expt b (- n 1))))))

(define (even? x)
    (= (remainder x 2) 0))

(define (expmod base exp m) 
    (cond ((= exp 0) 1)
        ((even? exp)
        (remainder
        (square (expmod base (/ exp 2) m))
        m)) 
    (else
        (remainder
        (* base (expmod base (- exp 1) m))
        m))))
