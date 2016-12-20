#lang racket

(require rackunit)

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (cond ((< n 0) (error "'n' can't be a negative number"))
        ((= n 0) null)
        ((= n 1) f)
        (else (compose f (repeated f (- n 1))))))

(check-equal? ((repeated sqr 2) 5) 625)
