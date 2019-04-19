(define (element-of-set? x set)
	(cond ((null? set) #f)
		((equal? x (car set)) #t)
		(else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
	(if (element-of-set? x set)
		set
		(cons x set)))

(define (union-set set1 set2)
	(let loop ((s1 set1)
		(s2 set2))
		(cond ((null? s2) 
			s1)
			(else 
				(loop (adjoin-set (car s2) s1)
					 (cdr s2))))))

(display (union-set '(1 2 3 4 5) '(4 5 6 7 8)))
	