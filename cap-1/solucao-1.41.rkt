#lang racket

(define (inc x)
  (+ 1 x))

(define (double f)
  (if (not (procedure? f))
      (error "not a procedure")
      (lambda (x) (f (f x)))))

(((double (double double)) inc) 5)


