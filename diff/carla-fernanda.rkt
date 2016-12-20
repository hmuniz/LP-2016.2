#lang racket

(require levenshtein)

; Resultados gerados na forma de matriz

(define (matrix-of-results list-a list-b teste)
  (define (aux list-1 list-2 matrix)
    (cond ((empty? list-1) matrix)
          (else (aux (cdr list-1)
                     list-2
                     (append matrix (list (map (lambda (x) (teste (car list-1) x))
                                               list-2)))))))
  (aux list-a list-b '()))

; Resultados gerados na forma de lista + Frequência

(define (list-of-results list-a list-b teste)
  (define (aux list-1 list-2 list-out)
    (cond ((empty? list-1) list-out)
          (else (aux (cdr list-1)
                     list-2
                     (append list-out (map (lambda (x) (teste (car list-1) x))
                                           list-2))))))
  (aux list-a list-b '()))

;; sobre o codigo acima, não precisava com

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

;; (accumulate append empty (matrix-of-results '("alexandre" "rademaker") '("pedro" "paulo") string-levenshtein))

(define (results-to-list lol)
  (define (lol-aux lol res)
    (if (empty? lol)
        res
        (lol-aux (cdr lol) (append res (car lol)))))
  (lol-aux lol empty))

(define (frequency-of-results results)
  (define (aux list-results list-frequencies)
    (if (empty? list-results)
        (unique-values list-frequencies)
        (aux (cdr list-results) (append list-frequencies
                                        (list (value-key (car list-results)))))))
  (define (value-key item)
    (cons item (count (lambda (x) (eq? x item)) results)))
  (aux results '()))

(define (unique-values values)
  (define (aux list-values uniques)
    (cond ((empty? list-values) uniques)
          ((member (car list-values) uniques) (aux (cdr list-values) uniques))
          (else (aux (cdr list-values)
                     (append uniques (list (car list-values)))))))
  (aux values '()))