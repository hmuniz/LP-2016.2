#lang racket
(define (square x) 
  (* x x))

(define (divides? a b) 
  (= (remainder b a) 0))

(define (even? n) 
  (= (remainder n 2) 0))

(define (smallest-divisor n) 
  (define (find-divisor n test) 
    (cond ((> (square test) n) n) 
          ((divides? test n) test) 
          (else (find-divisor n (+ test 1))))) 
  (find-divisor n 2)) 

(define (prime? n) 
  (= n (smallest-divisor n))) 

(define (runtime) 
  (current-milliseconds))

(define (timed-prime-test n) 
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time) 
  (if (prime? n) 
      (report-prime n (- (runtime) start-time)) 
      #f))

(define (report-prime n elapsed-time) 
  (display n) 
  (display "***") 
  (display elapsed-time) 
  (newline)) 

(define (search-for-primes n counter) 
  (if (even? n) 
      (s-f-p (+ n 1) counter) 
      (s-f-p n counter))) 

(define (s-f-p n counter) 
  (if (> counter 0) 
      (if (timed-prime-test n) 
          (s-f-p (+ n 2) (- counter 1)) 
          (s-f-p (+ n 2) counter)) 
      "COMPUTATION COMPLETE"))