#lang racket

(define (make-monitored f)
  (define (make-monitored-int f count)
    (lambda (param)
      (cond ((eq? param 'how-many-calls?)
             count)
            (else (f param)
                  (set! count (+ count 1))))))
  (make-monitored-int f 0))

(define s (make-monitored sqrt))
(s 100)
(s 'how-many-calls?)