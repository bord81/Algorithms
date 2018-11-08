(define (some-func n)
    (if (< n 3)
        n
        (+ (some-func (- n 1)) (* 2 (some-func (- n 2))) (* 3 (some-func (- n 3))))))
