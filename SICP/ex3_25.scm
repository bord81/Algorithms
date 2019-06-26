(define (assoc-my key records)
  (cond ((null? records) #f)
        ((equal? key (caar records)) (car records))
        (else (assoc-my key (cdr records)))))

(define (make-table)
  (let ((local-table (list '*table*)))
    
    (define (lookup key-lst) 
      (let ((record
             (assoc key-lst (cdr local-table))))
        (if record (cdr record) #f)))
    
(define (insert! key-lst value)
  (let ((record (assoc key-lst (cdr local-table))))
    (if record
        (set-cdr! record value)
        (set-cdr! local-table
                  (cons (cons key-lst value)
                        (cdr local-table)))))
  'ok)

    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation: TABLE" m))))
    dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

(put (list 'x 'y 'z) 5)
(put (list 'x 'f) 3)
(put (list 'n 'e 'v 'm) 4)

(get (list 'x 'y 'z))
(get (list 'x 'f))
(get (list 'n 'e 'v 'm))

(get (list 'x 'r))
(get (list 'r 'h))