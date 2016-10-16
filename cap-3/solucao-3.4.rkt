#lang racket

(define (make-account balance password)
  (let ((count-erro 0))
    (define (withdraw amount)
      (if (>= balance amount)
          (begin (set! balance 
                       (- balance amount))
                 balance)
          "Insufficient funds"))
    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)
    (define (call-the-cops input)
      (display "ARRESTED"))
    (define (dispatch some-password m)
      (if (eq? some-password password)
          (begin
            (set! count-erro 0)
            (cond ((eq? m 'withdraw) withdraw)
                  ((eq? m 'deposit) deposit)
                  (else (error "Unknown request: 
                 MAKE-ACCOUNT" m))))
          (if (>= count-erro 6)
              call-the-cops
              (begin
                (set! count-erro (+ count-erro 1))
                (error "Incorrect password")))))
    dispatch))
