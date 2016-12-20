#lang racket
(define (square x)
  (* x x))

(define (tan-cf x k) 
  (define (iter i result) 
    (if (= i 0) 
        result 
        (iter (- i 1) 
              (/ (if (= i 1) x (square x)) 
                 (- (- (* 2 i) 1) 
                    result))))) 
  (iter k 0)) 