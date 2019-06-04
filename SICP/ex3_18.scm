(define (last-pair x)
  (if (null? (cdr x)) x (last-pair (cdr x))))

(define (make-cycle x) (set-cdr! (last-pair x) x) x)

(define z (make-cycle (list 'a 'b 'c)))

(define (find-loop lst)
  (let loop ((first lst)
             (second (cdr lst)))
    (cond ((or (null? first) (null? second)) #f)
          ((eq? (car first) (car second)) #t)
          ((or (null? (cdr first)) (null? (cddr second))) #f)
          (else (loop (cdr first) (cddr second))))))
         	
(find-loop z)
