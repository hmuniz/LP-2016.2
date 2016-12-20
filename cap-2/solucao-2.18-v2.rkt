#lang racket

(require rackunit)

(define (reverse items)
  (if (null? items)
      null
      (append (take-right items 1)
              (reverse (drop-right items 1)))))

(check-equal? (reverse (list 1 2 3 4 5)) '(5 4 3 2 1))