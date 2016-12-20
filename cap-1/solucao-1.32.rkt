#lang racket

(define (inc x)
  (+ 1 x))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

;; a. recursive process

(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) next b))))

;; b. iterative process

(define (accumulate-i combiner null-value term a next b)
  (define (aux a b out)
    (if (> a b)
        out
        (aux (next a) b (combiner (term a) out))))
  (aux a b null-value))


;; testando

(define (sum-integers acc a b)
  (acc + 0 identity a inc b))

(equal? (sum-integers accumulate 1 10) (sum-integers accumulate-i 1 10))
