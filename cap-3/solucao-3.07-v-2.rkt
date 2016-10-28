#lang racket
(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance 
                     (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch some-password m)
    (if (eq? some-password password)
        (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              (else (error "Unknown request: 
                 MAKE-ACCOUNT" m)))
        (error "Incorrect password")))
  dispatch)

(define (make-joint account password new-password)
  (define (dispatch some-password m)
    (if (eq? some-password new-password)
        (cond ((eq? m 'withdraw)
               (account password 'withdraw))
              ((eq? m 'deposit)
               (account password 'deposit))
              (else (error "Unknown request: 
                 MAKE-ACCOUNT" m)))
        (error "Incorrect password")))
  (if (account password 'withdraw)
      dispatch
      (error "Main account incorrect password")))

; PERCEBAM QUE 'withdraw FOI ARBITRARIAMENTE ESCOLHIDO.
        