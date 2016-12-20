#lang racket
(define (same-parity . a)
  (if (odd? (car a))
      (filter odd? a)
      (filter even? a)))