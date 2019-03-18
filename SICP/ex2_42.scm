#lang racket

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position
                    new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(define empty-board empty)

(define (adjoin-position new-row col positions)
  (append positions (list (cons new-row col))))

(define (safe? col positions)
  (define (check-safe? pos1 pos2)
    (cond ((= (car pos1) (car pos2)) #f)
          ((= (abs (- (car pos1) (car pos2))) (abs (- (cdr pos1) (cdr pos2)))) #f)
          (else #t)))
  
  (let loop ((last-queen (list-ref positions (- col 1)))
        (rest-queens (filter (lambda (x) (not (= (cdr x) col))) positions)))
    (cond ((null? rest-queens) #t)
          ((not (check-safe? last-queen (car rest-queens))) #f)
          (else (loop last-queen (cdr rest-queens))))))

(define (flatmap proc seq)
  (accumulate append empty (map proc seq)))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      empty
      (cons low (enumerate-interval (+ low 1) high))))

(length (queens 2))
(length (queens 3))
(length (queens 4))
(length (queens 5))
(length (queens 6))
(length (queens 7))
(length (queens 8))
(length (queens 9))
(length (queens 10))
(length (queens 11))
