#lang racket
(define (make-monitored f)
  (let ((count 0))
    (define (mf input)
      (cond
        ((eq? input 'how-many-calls?)count)
        ((eq? input 'reset-count) (set! count 0))
        (else (begin (set! count (+ count 1))
                     (f input)))))
    mf))