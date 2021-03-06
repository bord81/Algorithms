#lang racket

(require racket/draw)

(define target (make-bitmap 100 100))
(define dc (new bitmap-dc% [bitmap target]))
(define (draw-line v1 v2)
  (send dc draw-line
        (xcor-vect v1) (ycor-vect v1)
        (xcor-vect v2) (ycor-vect v2)))

(define (make-segment v1 v2)
 (cons v1 v2))

(define (start-segment s)
  (car s))

(define (end-segment s)
  (cdr s))

(define (make-vect x y)
  (cons x y))

(define (xcor-vect v)
  (car v))

(define (ycor-vect v)
  (cdr v))

(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1) (xcor-vect v2))
             (+ (ycor-vect v1) (ycor-vect v2))))

(define (sub-vect v1 v2)
  (make-vect (- (xcor-vect v1) (xcor-vect v2))
             (- (ycor-vect v1) (ycor-vect v2))))

(define (scale-vect s v)
  (make-vect (* s (xcor-vect v))
             (* s (ycor-vect v))))

(define (origin-frame frame)
  (car frame))

(define (edge1-frame frame)
  (cadr frame))

(define (edge2-frame frame)
  (caddr frame))

(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v) (edge1-frame frame))
               (scale-vect (ycor-vect v) (edge2-frame frame))))))

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
        ((frame-coord-map frame)
         (start-segment segment))
        ((frame-coord-map frame)
         (end-segment segment))))
     segment-list)))

(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter (make-frame
                  new-origin
                  (sub-vect (m corner1) new-origin)
                  (sub-vect (m corner2) new-origin)))))))

(define (flip-vert painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

(define (rotate90 painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

(define (flip-horiz painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

(define (rotate180 painter)
  (transform-painter painter
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 0.0)))

(define (rotate270 painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))


(define d-painter (segments->painter 
                   (list (make-segment (make-vect 0.25 0) (make-vect 0.35 0.25))
                         (make-segment (make-vect 0.4 0) (make-vect 0.45 0.25))
                         (make-segment (make-vect 0.35 0.25) (make-vect 0.25 0.35)) 
                         (make-segment (make-vect 0.45 0.25) (make-vect 0.35 0.35))
                         (make-segment (make-vect 0.25 0.35) (make-vect 0.15 0.25))
                         (make-segment (make-vect 0.15 0.25) (make-vect 0 0.4)) 
                         (make-segment (make-vect 0 0.6) (make-vect 0.15 0.75)) 
                         (make-segment (make-vect 0.15 0.75) (make-vect 0.25 0.65))
                         (make-segment (make-vect 0.25 0.65) (make-vect 0.35 0.75))
                         (make-segment (make-vect 0.35 0.75) (make-vect 0.5 1))
                         (make-segment (make-vect 0.55 0.75) (make-vect 0.6 1))
                         (make-segment (make-vect 0.55 0.75) (make-vect 0.35 0.6))
                         (make-segment (make-vect 0.35 0.6) (make-vect 1 0.75))
                         (make-segment (make-vect 1 0.25) (make-vect 0.35 0.35))
                         (make-segment (make-vect 0.65 0.5) (make-vect 1 0.65))
                         (make-segment (make-vect 1 0.4) (make-vect 0.65 0.5)))))

(define d-painter-flip-v
  (flip-vert d-painter))
(send dc clear)
(d-painter-flip-v (make-frame
                   (make-vect 0 0)
                   (make-vect 0 99)
                   (make-vect 99 0)
                   ))
(send target save-file "/home/bobr/Downloads/ex2_50_v.jpeg" 'jpeg)

(define d-painter-rot-90
  (rotate90 d-painter))
(send dc clear)
(d-painter-rot-90 (make-frame
                   (make-vect 0 0)
                   (make-vect 0 99)
                   (make-vect 99 0)
                   ))
(send target save-file "/home/bobr/Downloads/ex2_50_90.jpeg" 'jpeg)

(define d-painter-flip-h
  (flip-horiz d-painter))
(send dc clear)
(d-painter-flip-h (make-frame
                   (make-vect 0 0)
                   (make-vect 0 99)
                   (make-vect 99 0)
                   ))
(send target save-file "/home/bobr/Downloads/ex2_50_h.jpeg" 'jpeg)


(define d-painter-rot-180
  (rotate180 d-painter))
(send dc clear)
(d-painter-rot-180 (make-frame
                   (make-vect 0 0)
                   (make-vect 0 99)
                   (make-vect 99 0)
                   ))
(send target save-file "/home/bobr/Downloads/ex2_50_180.jpeg" 'jpeg)

(define d-painter-rot-270
  (rotate270 d-painter))
(send dc clear)
(d-painter-rot-270 (make-frame
                   (make-vect 0 0)
                   (make-vect 0 99)
                   (make-vect 99 0)
                   ))
(send target save-file "/home/bobr/Downloads/ex2_50_270.jpeg" 'jpeg)