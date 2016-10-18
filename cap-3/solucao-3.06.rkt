#lang racket

(define (rand-update y)
  (* y (+ y 1)))

(define (rand seed)
  (define (generate)
    (begin
      (set! seed (rand-update seed))
      seed))
    (define (reset value)
      (set! seed value))
    (define (dispatch m)
      (cond ((eq? m 'generate) generate)
            ((eq? m 'reset) reset)
            (else (error "not valid"))))
    dispatch)
