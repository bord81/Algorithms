(define (timed-prime-test n)
    (start-prime-test n (process-time-clock)))
(define (start-prime-test n start-time) 
    (search-for-primes (+ n 1) 3)
    (report-prime (- (process-time-clock) start-time)))
(define (report-prime elapsed-time)
    (newline)
  (display "Total search time is about: ")
  (display elapsed-time))
(define (search-for-primes more-than times)
    (if (> times 0)
        (if (= more-than (find-divisor more-than 2))
        (begin
            (newline)
            (display more-than)
            (search-for-primes (+ more-than 1) (- times 1)))       
            (search-for-primes (+ more-than 1) times))))
(define (find-divisor n test-divisor) 
    (cond ((> (square test-divisor) n) n)
    ((divides? test-divisor n) test-divisor)
    (else (find-divisor n (next test-divisor)))))       
(define (divides? a b) (= (remainder b a) 0))
(define (next n)
    (cond ((= n 2) 3)
        (else (+ n 2))))
