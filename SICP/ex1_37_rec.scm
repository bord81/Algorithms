(define cont-frac
  (lambda (n d k)
    (cont-frac-int n d k 1)))

(define cont-frac-int
    (lambda (n d k i)
      (cond
     ((= i (+ k 1)) 0)
     (else (/ (n i) (+ (d i) (cont-frac-int n d k (+ i 1))))))))
