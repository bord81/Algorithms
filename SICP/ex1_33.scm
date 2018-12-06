(define (filtered-accumulate combiner null-value filter? term a next b)
    (if (> a b)
        null-value
        (if (filter? a)
        (combiner (term a) (filtered-accumulate combiner null-value filter? term (next a) next b))
        (filtered-accumulate combiner null-value filter? term (next a) next b))))

(define (filtered-accumulate combiner null-value filter? term a next b)
    (define (iter a result)
            (if (> a b) 
                result
                (iter (next a) (if (filter? a)
                                (combiner (term a) result)
                                result))))        
    (iter a null-value))

(define (inc x) (+ x 1))

;'a' part
(define (sum-square-primes a b)
    (filtered-accumulate + 0 prime? square a inc b))
(define (prime? n)
    (if (< n 2)
        #f
        (= n (smallest-divisor n))))
(define (smallest-divisor n) (find-divisor n 2))
(define (find-divisor n test-divisor) 
    (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1))))) 
(define (divides? a b) (= (remainder b a) 0))

;'b' part
(define (product-primes-to-n n)
    (define (prime-to-n? i)
        (if (= (gcd n i) 1)
        #t
        #f))
    (filtered-accumulate * 1 prime-to-n? product-term 1 inc n))
(define (product-term n) n)
(define (gcd a b) 
    (if (= b 0)
    a
    (gcd b (remainder a b))))
