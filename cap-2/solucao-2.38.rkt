#lang racket

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(define (fold-right op initial sequence)
  (let ((rev-list (reverse sequence)))
  (define (iter result rev-list)
    (if (null? rev-list)
        result
        (iter (op (car rev-list) result)
              (cdr rev-list))))
  (iter initial rev-list)))


;QUALQUER OPERAÇAO QUE SEJA COMUTATIVA, COMO SOMA E MULTIPLICAÇAO POR EXEMPLO, 
