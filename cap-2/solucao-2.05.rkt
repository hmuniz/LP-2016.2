#lang racket
(define (cons a b)
  (* (expt 2 a)
     (expt 3 b)))

(define (car z)
  (define (car-iter z counter)
    (if (= (remainder z 2) 0)
        (car-iter (/ z 2) (+ counter 1))
        counter))
  (car-iter z 0))

(define (cdr z)
  (define (cdr-iter z counter)
    (if (= (remainder z 3) 0)
        (cdr-iter (/ z 3) (+ counter 1))
        counter))
  (cdr-iter z 0))