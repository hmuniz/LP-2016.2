#lang racket
(define (split f g) 
   (define (rec painter n) 
     (if (= n 1) 
         painter 
         (let ((smaller (rec painter (- n 1)))) 
           (f painter (g smaller smaller))))) 
   rec) 
  