#lang racket

(define (pascal line position)
  (cond ((or (< line 0)
             (< position 1)
             (> position (+ line 1)))
         (error "invalid position or line" line position))
        ((or (= position 1)
             (= position (+ line 1)))
         1)
        (else (+ (pascal (- line 1) position)
                 (pascal (- line 1) (- position 1))))))