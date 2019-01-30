(define (rect-perimeter rect)
  (* (+ (dist-btw-points (car (car rect)) (cdr (car rect))) (dist-btw-points (car (car (cdr rect))) (cdr (car (cdr rect))))) 2))

(define (rect-area rect)
  (* (dist-btw-points (car (car rect)) (cdr (car rect))) (dist-btw-points (car (car (cdr rect))) (cdr (car (cdr rect))))))

(define (make-rect point1 point2 point3 point4)
  (define points (cons point4 (cons point3 (cons point2 (cons point1 ())))))
  (define sides (list))
  (define rect (list))
  (define (make-sides lst point out)
    (let loop ((points lst)
               (out-lst out))
      (if (pair? points)
          (begin
            (set! out-lst (cons
                           (cons (make-segment point (car points))
                                 (midpoint-segment (make-segment point (car points))))
                           out-lst))
            (loop (cdr points) out-lst))
          out-lst)))
  (define (find-bisects lst side)
    (let loop ((sides lst)
               (n 0))
      (if (pair? sides)
          (if (equal-points? (cdr (car sides)) (cdr side))
              (cond ((= (dist-btw-points (car (car (car sides))) (cdr (car (car sides)))) (dist-btw-points (car (car side)) (cdr (car side))))
                     (loop (cdr sides) (+ n 1)))
                    (else (loop (cdr sides) (+ n 2))))
              (loop (cdr sides) n))
          n)))
  (if (let points-ok? ((input-pts points))
        (if (and (pair? input-pts) (> (length input-pts) 1))
            (if (contains-point? (cdr input-pts) (car input-pts))
                #f
                (begin
                  (set! sides (make-sides (cdr input-pts) (car input-pts) sides))
                  (points-ok? (cdr input-pts))))
            #t))
      (let rect-ok? ((input-sides sides)
                     (n-bisects 0)
                     (n-incr 0))
        (if (> n-bisects 1)
            #f
            (if (and (pair? input-sides) (> (length input-sides) 1))
                (begin
                  (set! n-incr (find-bisects (cdr input-sides) (car input-sides)))
                  (cond ((= n-incr 0)
                         (cond ((= (length rect) 0) (begin (set! rect (cons (car (car input-sides)) rect))
                                                           (rect-ok? (cdr input-sides) n-bisects 0)))
                               ((= (length rect) 1) (if
                                                     (= (dist-btw-points (car (car rect)) (cdr (car rect))) (dist-btw-points (car (car (car input-sides))) (cdr (car (car input-sides)))))
                                                     (rect-ok? (cdr input-sides) n-bisects 0)
                                                     (begin (set! rect (cons (car (car input-sides)) rect)) (rect-ok? (cdr input-sides) n-bisects 0))))))
                        (else (rect-ok? (cdr input-sides) (+ n-bisects n-incr) 0))))
                rect))
        rect)
      #f))

(define (dist-btw-points point1 point2)
  (sqrt (+ (* (- (x-point point1) (x-point point2)) (- (x-point point1) (x-point point2))) (* (- (y-point point1) (y-point point2)) (- (y-point point1) (y-point point2))))))

(define (contains-point? lst point)
    (let loop ((points lst))
        (if (pair? points)
            (if (equal-points? point (car points))
                #t
                (loop (cdr points)))
            #f)))

(define (equal-points? point1 point2)
        (if (and (= (x-point point1) (x-point point2)) (= (y-point point1) (y-point point2)))
            #t
            #f))

(define (midpoint-segment segment)
    (make-point 
        (/ (+ (x-point (start-segment segment)) (x-point (end-segment segment))) 2)
        (/ (+ (y-point (start-segment segment)) (y-point (end-segment segment))) 2)))

(define (make-segment point1 point2)
    (cons point1 point2))

(define (start-segment segment)
    (car segment))

(define (end-segment segment)
    (cdr segment))

(define (make-point x y)
    (cons x y))

(define (x-point point)
    (car point))

(define (y-point point)
    (cdr point))

(make-rect  (make-point 1 1) (make-point 5 1) (make-point 1 8) (make-point 5 8))
(rect-perimeter (make-rect  (make-point 1 1) (make-point 5 1) (make-point 1 8) (make-point 5 8)))
(rect-area (make-rect  (make-point 1 1) (make-point 5 1) (make-point 1 8) (make-point 5 8)))
