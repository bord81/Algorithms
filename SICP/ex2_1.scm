(define (make-rat n d)
    (let ((g (gcd n d)))
        (cond 
             ((and (< n 0) (> d 0)) (set! g (* g -1)))
             ((and (not (< n 0)) (< d 0)) (set! g (* g -1))))
        (cons (/ n g) (/ d g))))
