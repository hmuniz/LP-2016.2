#lang racket

(define (tree-map-1 proc tree)
  (map (lambda (x)
         (if (pair? x)
             (tree-map-1 proc x)
             (proc x)))
       tree))

(define (tree-map-2 proc tree)
  (cond ((null? tree) null)
        ((not (pair? tree)) (proc tree))
        (else (cons (tree-map-2 proc (car tree))
                    (tree-map-2 proc (cdr tree))))))


(define (square-tree tree) (tree-map-2 sqr tree))


