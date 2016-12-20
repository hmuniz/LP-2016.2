#lang racket

(require rackunit)

(define (inc x) (+ x 1))

(define (compose f g)
  (lambda (x) (f (g x))))

(check-equal? ((compose sqr inc) 6)  49)
