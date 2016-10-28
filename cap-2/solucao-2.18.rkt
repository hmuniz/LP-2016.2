#lang racket

(define (reverse list)
  (define (rev-iter list out)
    (if (null? list)
        out
        (rev-iter (cdr list) (cons (car list) out))))
  (rev-iter list empty))