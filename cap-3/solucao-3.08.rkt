#lang racket

(define f
  (let ((a 0))
    (lambda (x)
      (let ((keep a))
        (set! a x)
        keep))))
