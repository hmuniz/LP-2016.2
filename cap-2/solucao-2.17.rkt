#lang racket

(define (last-pair-1 list)
  (define (last-pair-iter list last)
    (if (null? list)
        last
        (last-pair-iter (cdr list) list)))
  (last-pair-iter list empty))


(define (last-pair-2 list)
  (cond ((null? list) null)
        ((null? (cdr list))
         (cons (car list) null))
        (else (last-pair (cdr list)))))
