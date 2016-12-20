#lang racket

(require math/number-theory)

(define (filtered-accumulate combiner null-value term a next b predicate)
  (if (> a b)
      null-value
      (if (predicate a)
          (combiner (term a)
                    (filtered-accumulate combiner null-value term (next a) next b predicate))
          (filtered-accumulate combiner null-value term (next a) next b predicate))))

;; item a

(define (sum-of-sqrt-primes a b)
  (filtered-accumulate + 0 sqr a (lambda (x) (+ 1 x)) b prime?))

(sum-of-sqrt-primes 2 6)
(sum-of-sqrt-primes 2 10)

;; item b

(define (gcd a b)
   (if (= b 0)
       a
       (gcd b (remainder a b))))

(define (product-of-coprimes n)
  (filtered-accumulate * 1 identity 1 (lambda (x) (+ 1 x)) n
                       (lambda (x) (equal? 1 (gcd x n)))))

(product-of-coprimes 10)



