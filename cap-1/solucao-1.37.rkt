#lang racket
;Versão iterativa
(define (cont-frac n d k) 
  (define (cont-frac-iter i result) 
    (if (= i 0) 
        (/ (n i) (+ (d i) result)) 
        (cont-frac-iter (- i 1) 
                        (/ (n i) (+ (d i) result))))) 
  (cont-frac-iter k 0.0))

;Versão recursiva
(define (cont-fact n d k) 
  (define (recur i) 
    (if (> i k) 
        0 
        (/ (n i) (+ (d i) (recur (add1 i)))))) 
  (recur 1)) 