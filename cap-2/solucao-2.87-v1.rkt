#lang racket

(put '=zero? '(polynomial)
     (lambda (p) (zero-aux p)))


(define (zero-aux p)
  (or (null? (term-list p))
      (and (=zero? (coeff (first-term (term-list p))))
           (zero-aux2 (rest-term (term-list p))))))

(define (zero-aux2 terms-list)
  (and (=zero? (coeff (first-term terms-list)))
       (zero-aux2 (rest-term terms-list))))
  