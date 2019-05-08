#lang racket

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1
                     (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list
                             (right-branch tree)
                             result-list)))))
  (copy-to-list tree '()))

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result
               (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result
                   (partial-tree
                    (cdr non-left-elts)
                    right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts
                     (cdr right-result)))
                (cons (make-tree this-entry
                                 left-tree
                                 right-tree)
                      remaining-elts))))))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set)
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set) (left-branch set)
                    (adjoin-set x (right-branch set))))))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-set? x (right-branch set)))))

(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set-list? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set-list? x (cdr set)))))

(define (adjoin-set-list x set)
  (define (ins-sorted e s)
    (cond ((null? s) (cons e '()))
          ((> e (car s)) (cons (car s) (ins-sorted e (cdr s))))
          ((< e (car s)) (cons e s))))
  (if (element-of-set-list? x set)
      set
      (ins-sorted x set)))

(define (intersection-set-list set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1 (intersection-set-list (cdr set1)
                                          (cdr set2))))
              ((< x1 x2)
               (intersection-set-list (cdr set1) set2))
              ((< x2 x1)
               (intersection-set-list set1 (cdr set2)))))))


(define (union-set-list set1 set2)
  (let loop ((res set1)
             (s2 set2))
    (cond ((null? s2) res)
          (else (loop (adjoin-set-list (car s2) res) (cdr s2))))))

(define (union-set set1 set2)
  (list->tree (union-set-list (tree->list-2 set1)
                              (tree->list-2 set2))))

(define (intersection-set set1 set2)
  (list->tree (intersection-set-list (tree->list-2 set1)
                              (tree->list-2 set2))))
 
(union-set (list->tree '(1 2 3 5 7 10 55))  
           (list->tree '(5 6 10 11 20 23 55)))
(intersection-set (list->tree '(1 2 3 5 7 10 55))  
                  (list->tree '(5 6 10 11 20 23 55))) 