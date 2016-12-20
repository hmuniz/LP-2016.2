#lang racket

;; a)

(define (product term a next b)
  (if (> a b)
      1
      (* (term a) (product term (next a) next b))))


;; fatorial

(define (id x) x)

(define (next x) (+ 1 x))

(define (fact x)
  (product id 1 next x))

; testes

(require rackunit)

(check-equal? (fact 6) 720)

(check-equal? (fact 9) 362880)


;; b) Pi de John Wallis

(define (pi-wallis x)
  (if (even? x)
      (/ (+ 2 x) (+ 1 x))
      (/ (+ 1 x) (+ 2 x))))

(define (pi-sum x)
  (product  pi-wallis 1 next x))


;; c) Produto interativo

(define (product-it term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a)(* (term a) result))))
  (iter a 1))

;; fatorial interativo

(define (fact-it x)
  (product-it id 1 next x))

        
;; testes

(require rackunit)

(check-equal? (fact-it 6) 720)
(check-equal? (fact-it 9) 362880)
  
  
