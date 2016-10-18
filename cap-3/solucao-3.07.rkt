#lang racket
(define (make-account balance password)
  (define (correct-password? some-password password)
  (if (pair? password)
      (or (eq? some-password (car password))
          (eq? some-password (cdr password)))
      (eq? some-password password)))
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance 
                     (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (acc-joint-password new-p)
    (set! password (cons password new-p)))
  (define (dispatch some-password m)
    (if (correct-password? some-password password)
        (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              ((eq? m 'acc-joint-password) acc-joint-password)
              (else (error "Unknown request: 
                 MAKE-ACCOUNT" m)))
        (error "Incorrect password")))
  dispatch)

(define (make-joint account old-p new-p)
 (begin
   ((account old-p 'acc-joint-password) new-p)
  (lambda (some-password m)
    (account some-password m))))

