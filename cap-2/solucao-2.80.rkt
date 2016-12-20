#lang racket
(define (=zero? x) (apply-generic '=zero? x)) 

; adicionando para scheme-number-package 
(put '=zero? 'scheme-number (lambda (x) (= x 0))) 

; adicionando para rational-number-package 
(put '=zero? 'rational-number  
     (lambda (x) (= (number x) 0))) 

; adicionando para complex-number-package 
(put '=zero? 'complex-number 
     (lambda (x) (= (real-part x) (imag-part x) 0))) 