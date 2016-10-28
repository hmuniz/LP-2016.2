#lang racket

(define (square-list-1 items)
  (if (null? items)
      empty
      (cons (sqr (car items))
            (square-list1 (cdr items)))))

(define (square-list-2 items)
  (map sqr items))