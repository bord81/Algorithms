;classical Wallis product
;needs n = 300 000 to get 3.14159
(define (wallis-pi n) 
   (/ (* 4 (* n n)) (- (* 4 (* n n)) 1)))

;improved Wallis product ('empirical 3/4')
;max computable n on my interpreter is 48 (float argument), which gives 3.141551347008624
;'the classical' version needs n = 20 000 to achieve comparable precision
;3.14159 is already achived by n = 192, i.e. (* (wallis-pi-impr 192) 1.0)
(define (wallis-pi-impr n) 
   (/ (* (fast-expt 2 (+ (* 4 n) 2)) (+ n (/ 3 4)) (fast-expt (prod factorial 1 inc n) 4))
   (square (prod factorial 1 inc (+ (* 2 n) 1)))))
   
(define (prod term a next b) 
    (define (iter a result)
        (if (> a b) 
        result
        (iter (next a) (* (term a) result))))        
            (iter a 1))  
(define (factorial x) 
    (if (= x 0)
        1
        x))
(define (fast-expt b n) 
    (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2)))) 
        (else (* b (fast-expt b (- n 1))))))
(define (inc x) (+ x 1))
