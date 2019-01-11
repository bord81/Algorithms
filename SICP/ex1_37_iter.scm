(define cont-frac
  (lambda (n d k)
    (cont-frac-int n d k 0)))

(define cont-frac-int
    (lambda (n d i a)
      (cond
     ((= i 0) a)
     (else (cont-frac-int n d (- i 1) (/ (n i) (+ (d i) a)))))))

(cont-frac (lambda (i) 1.0)
	   (lambda (i) 1.0)
	   11)
