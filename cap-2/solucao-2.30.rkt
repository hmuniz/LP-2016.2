#lang racket

; direct implementation
(define (square-tree-1 tree)
   (cond ((null? tree) null)
         ((not (pair? tree)) (sqr tree))
         (else (cons (square-tree-1 (car tree))
                     (square-tree-1 (cdr tree))))))

; using map and recursion
(define (square-tree-2 tree)
  (map (lambda (x)
         (if (pair? x)
             (square-tree-2 x)
             (sqr x)))
       tree))
