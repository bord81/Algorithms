#lang racket

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp))
               (operands exp) var))))

(define (operator exp) (car exp))

(define (operands exp) (cdr exp))

(define (install-deriv-package)
  ;;internal procedures
  (define (=number? exp num) (and (number? exp) (= exp num)))
  (define (addend s) (cadr s))
  (define (augend s) (caddr s))
  (define (multiplier p) (cadr p))
  (define (multiplicand p) (caddr p))
  (define (base exp) (cadr exp))
  (define (exponent exp) (caddr exp))
  (define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
          ((and (number? a1) (number? a2))
           (+ a1 a2))
          (else (list '+ a1 a2))))
  (define (make-product m1 m2)
    (cond ((or (=number? m1 0) (=number? m2 0)) 0)
          ((=number? m1 1) m2)
          ((=number? m2 1) m1)
          ((and (number? m1) (number? m2)) (* m1 m2))
          (else (list '* m1 m2))))
  (define (make-exponentiation b e)
    (cond ((= e 0) 1)
          ((= e 1) b)
          (else (list '** b e))))

  ;;external interface
  (put 'deriv '+
       (lambda (exp var) (attach-tag '+ (make-sum (deriv (addend exp) var)
                                                  (deriv (augend exp) var)))))
  (put 'deriv '*
       (lambda (exp var) (attach-tag '* (make-sum
                                         (make-product (multiplier exp)
                                                       (deriv (multiplicand exp) var))
                                         (make-product (deriv (multiplier exp) var)
                                                       (multiplicand exp))))))
  (put 'deriv '**
       (lambda (exp var) (attach-tag '** (make-product (make-product
                                                        (exponent exp)
                                                        (make-exponentiation (base exp) (- (exponent exp) 1)))
                                                       (deriv (base exp) var)))))
  'done)

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))