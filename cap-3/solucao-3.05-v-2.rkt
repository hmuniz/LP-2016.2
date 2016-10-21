#lang racket

(require rackunit)

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))
; questão propriamente dita
(define (estimate-integral predicate x1 x2 y1 y2 trials)
  (*(* (- x1 x2) (- y1 y2))
    (monte-carlo trials predicate)))

(define (circulo?)
  (>= 1 (+ (square (random-in-range -1 1))
           (square (random-in-range -2 1)))))

(define (estimate-pi)
  (estimate-integral circulo? -1.0 1 -2 1 100000))
; Fiquei brincando com os os parametros do retangulo, desde que o circulo continue dentro dele
(define square
  (lambda (x) (* x x)))

(check-= (estimate-pi) 3.1416 0.2 "Aproximação ruim para o pi")