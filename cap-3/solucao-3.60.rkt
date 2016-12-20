#lang racket

(define-syntax cons-stream
  (syntax-rules ()
    ((_ a b) (cons a (delay b)))))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (display-line x)
  (displayln x))

(define (stream-car stream) 
  (car stream))

(define (stream-cdr stream) 
  (force (cdr stream)))

(define (stream-null? stream)
  (eq? stream '()))

(define the-empty-stream '())

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define (mul-streams s1 s2) (stream-map * s1 s2))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1)
                  (stream-car s2))
               (add-streams (scale-stream (stream-cdr s2)
                                          (stream-car s1)) 
                            (mul-series (stream-cdr s1) s2))))