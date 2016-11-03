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

(define (partial-sum stream)
  (define (aux stream last)
    (if (= last 0)
        (cons-stream (car stream) (aux (stream-cdr stream) (stream-car stream)))
        (if (stream-null? stream)
            0
            (let ((next (+ (stream-car stream) last)))
            (cons-stream next
                         (aux (stream-cdr stream) next))))))
  (aux stream 0))
