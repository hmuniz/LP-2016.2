#lang racket

;; excepted solution
(define (same-parity a . z)
  (define (iter items answer)
    (if (null? items)
        answer
        (iter (cdr items)
              (if (= (remainder (car items) 2)
                     (remainder a 2))
                  (append answer (list (car items)))
                  answer))))
  (iter z (list a)))

;; using filter
(define (same-parity a . b)
  (if (odd? a)
      (cons a (filter (lambda (x) (odd? x)) b))
      (cons a (filter (lambda (x) (even? x)) b))))
