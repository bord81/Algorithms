#lang racket

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)
                               (cadr pair))
                    (make-leaf-set (cdr pairs))))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (encode-symbol sym tree)

  (define (get-symbol-code sym tree)
    (if (leaf? tree)
        '()
        (let ((lb (left-branch tree))
              (rb (right-branch tree)))
          (cond ((contains-sym? sym (symbols lb)) (cons '0 (get-symbol-code sym lb)))
                ((contains-sym? sym (symbols rb)) (cons '1 (get-symbol-code sym rb)))))))
  (define (contains-sym? sym tree)
    (cond ((null? tree) #f)
          ((eq? (car tree) sym) #t)
          (else (contains-sym? sym (cdr tree)))))
  (if (contains-sym? sym (caddr tree))
      (get-symbol-code sym tree)
      (error "Symbol not found in tree: " sym)))

(define (find-symbol symbol branch)
  (define (helper symbol syms)
    (if (null? syms)
        #f
        (if (eq? symbol (car syms))
            #t
            (helper symbol (cdr syms)))))
  (helper symbol (symbols branch)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit: CHOOSE-BRANCH" bit))))

(define (left-branch tree)
  (car tree))

(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (make-leaf symbol weight) (list 'leaf symbol weight))

(define (leaf? object) (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))

(define (weight-leaf x) (caddr x))

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge freqs)
  (let loop ((leaves freqs))
    (cond ((> (length leaves) 3)
           (loop (insert-sorted (make-code-tree (car leaves) (cadr leaves)) (cddr leaves)))
           )
          (else (make-code-tree (caddr leaves) (make-code-tree (car leaves) (cadr leaves)))))))

(define (insert-sorted elem tree)
  (let loop ((result '())
             (temp tree)
             (inserted? #f))
    (cond ((null? temp) (reverse result))
          ((< (weight (car temp)) (weight elem))
           (loop (append (list (car temp)) result) (cdr temp) inserted?))
          ((not inserted?) (loop (append (list elem) result) temp #t))
          (else (loop (append (list (car temp)) result) (cdr temp) inserted?)))))
    

(define sample-msg  '(A D A B B C A))
(define sample-freqs '((A 4) (B 2) (C 1) (D 1)))
(define sample-tree (generate-huffman-tree sample-freqs))
sample-msg
(decode (encode sample-msg sample-tree) sample-tree)