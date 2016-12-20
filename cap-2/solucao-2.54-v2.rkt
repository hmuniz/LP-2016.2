#lang racket

;; segunda versão do exercício 2.54 que passa em todos os test cases

(require racket/trace rackunit)

(define (equal? a b)
  (cond ((and (empty? a) (empty?  b)) #t)
        ((or (empty?  a) (empty?  b)) #f)
        ((and (cons? a) (cons? b))
         (and (equal? (first a) (first b))
              (equal? (rest a) (rest b))))
        ((or (cons? a) (cons? b)) #f)
        (else (eq? a b))))


;;;;;;;;;;;;;;;;;;;
;;;;;; TESTS ;;;;;;
;;;;;;;;;;;;;;;;;;;

(check-true (equal? '(1 2 3 (4 5) 6) '(1 2 3 (4 5) 6)))
(check-false (equal? '(1 2 3 (4 5) 6) '(1 2 3 (4 5 7) 6)))
(check-true (equal? '(isso é uma lista) '(isso é uma lista)))
(check-false (equal? '(isso é uma lista)
		     '(isso é uma lista (com outra lista) no meio)))
