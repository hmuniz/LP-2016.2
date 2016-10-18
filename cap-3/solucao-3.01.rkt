#lang racket

(define (make-accumulator balance)
  (lambda (amount)
    (begin (set! balance (+ balance amount))
           balance)))