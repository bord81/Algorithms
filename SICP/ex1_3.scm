(define (two-max-squared a b c)
    (if (<= a b)
        (+ (* b b) (if (<= a c)
            (* c c)
            (* a a)))
        (+ (* a a) (if (<= b c)
            (* c c)
            (* b b)))))