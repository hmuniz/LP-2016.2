#lang racket
(define (unique-sets-summing-to-k set-size max-number k) 
   (filter (lambda (tuple) (= (accumulate + 0 tuple) k)) 
           (make-unique-tuples max-number set-size))) 