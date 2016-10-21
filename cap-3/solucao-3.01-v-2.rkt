#lang Racket
(define (make-accumulator balance)
  (lambda (amount)
    (if (eq? amount 'how-much-balance)
        balance
        (begin (set! balance (+ balance amount))
        balance))))
