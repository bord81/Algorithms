(define euler-e
  (lambda (k)
    (let ((d-vec (make-vector k)))
      (set! d-vec (calculate-d d-vec (vector-length d-vec) 0 1 1))
      (+ 2 (cont-frac (lambda (i) 1.0)
		      (lambda (i) (vector-ref d-vec (- i 1)))
		      (- k 1))))))

(define calculate-d
  (lambda (dv size i off val)
    (cond
     ((< i size) (begin
		   (cond
		    ((or (= off 0) (= off 1)) (vector-set! dv i '1))
		    ((= off 2) (vector-set! dv i (* val 2))))
		   (cond
		    ((or (= off 0) (= off 1)) (calculate-d dv size (+ i 1) (+ off 1) val))
		    ((= off 2) (calculate-d dv size (+ i 1) 0 (* val 2))))))
     (else dv))))
     
(define cont-frac
  (lambda (n d k)
    (cont-frac-int n d k 1)))

(define cont-frac-int
    (lambda (n d k i)
      (cond
     ((= i (+ k 1)) 0)
     (else (/ (n i) (+ (d i) (cont-frac-int n d k (+ i 1))))))))


(euler-e 10)
