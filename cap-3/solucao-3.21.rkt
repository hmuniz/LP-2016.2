#lang racket
(define (front-ptr queue) (car queue))

(define (print-queue q) 
  (define (iter x) 
    (if (null? x) 
        (newline) 
        (begin (display (car x)) 
               (iter (cdr x))))) 
  (iter (front-ptr q)))