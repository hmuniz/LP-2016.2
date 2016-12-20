#lang racket

(require levenshtein)

(define (make-matriz-of-diference list-a list-b test)
  (if (empty? list-a)
      empty
      (cons (map (lambda (line) (test (car list-a) line)) list-b)
              (make-matriz-of-diference (cdr list-a) list-b test))))