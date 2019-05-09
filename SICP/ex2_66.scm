#lang racket

(define (lookup-db key set)
  (cond ((null? set) false)
        ((= key (entry set)) (data set))
        ((< key (entry set))
         (lookup-db key (left-branch set)))
        ((> key (entry set))
         (lookup-db key (right-branch set)))))