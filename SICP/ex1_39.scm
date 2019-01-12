(define tan-cf
  (lambda (x k)
    (let ((d-vec (make-vector k)))
      (set! d-vec (calculate-d d-vec (vector-length d-vec) 0 1))
      (cont-frac (lambda (i) (cond
				   ((= i 1) x)
				   (else (* x x))))
		 (lambda (i) (vector-ref d-vec (- i 1)))
		      (- k 1)))))

(define calculate-d
  (lambda (dv size i val)
    (cond
     ((< i size) (begin
		   (cond
		    ((= i 0) (vector-set! dv i '1))
		    (else (vector-set! dv i val)))
		   (calculate-d dv size (+ i 1) (+ val 2))))
     (else dv))))

(define cont-frac
  (lambda (n d k)
    (cont-frac-int n d k 1)))

(define cont-frac-int
    (lambda (n d k i)
      (cond
     ((= i (+ k 1)) 0)
     (else (/ (n i) (- (d i) (cont-frac-int n d k (+ i 1))))))))
