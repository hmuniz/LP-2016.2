#lang racket
;(define (square-list items)
;  (define (iter things answer)
;    (if (null? things)
;        answer
;        (iter (cdr things)
;              (cons answer
;                    (square (car things))))))
;  (iter items null))
;
(define square (lambda (x)(* x x)))

;Meu código

(define (square-list items)
  (define (aux dummy answer)
    (if (empty? dummy)
        answer
        (aux (cdr dummy)
             (append answer (list (square (car dummy)))))))
  (aux items null))
    