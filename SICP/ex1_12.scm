(define (pascal-tri n k)
    (cond ((= n k) 1)
          ((= k 0) 1)
          (else (+ (pascal-tri (- n 1) (- k 1)) (pascal-tri (- n 1) k)))))
