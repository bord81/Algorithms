
(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((= x (car set)) #t)
        ((< x (car set)) #f)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (define (ins-sorted e s)
    (cond ((null? s) (cons e '()))
          ((> e (car s)) (cons (car s) (ins-sorted e (cdr s))))
          ((< e (car s)) (cons e s))))
  (if (element-of-set? x set)
      set
      (ins-sorted x set)))


(define (union-set set1 set2)
	(let loop ((res set1)
				(s2 set2))
	(cond ((null? s2) res)
		(else (loop (adjoin-set (car s2) res) (cdr s2))))))
		
		
(display(union-set '(1 3 5 7 9) '(2 4 6 8 10)))
(newline)
(display(union-set '(-1 0 1 3 5 6 7 9) '(2 4 6 8 10 12 14)))
(newline)
(display(union-set '(1 2 3) '(4 5 6 7 8)))