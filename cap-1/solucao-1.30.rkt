#lang racket

;; definicao recursiva do livro

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

;; exercicio

(define (sum-i term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ (term a) result))))
  (iter a 0))

;; teste

(define (sum-cubes a-sum a b)
  (a-sum (lambda (x) (* x x x)) a (lambda (x) (+ 1 x)) b))

(equal? (sum-cubes sum 1 10) (sum-cubes sum-i 1 10))



