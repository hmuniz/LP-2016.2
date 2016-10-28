#lang racket

(define (fringe tree)
  (cond ((null? tree) empty)
        ((not (pair? tree)) (list tree))
        (else (append (fringe (car tree))
                      (fringe (cdr tree))))))

