#lang racket
(require levenshtein)

(define (distance-matrix phrases1 phrases2 teste)
  (define (aux seq1 seq2 out)
    (cond ((null? seq1) (reverse (cdr out)))
          ((null? seq2) (aux (cdr seq1) (reverse phrases2) (cons '() out)))
          (else (aux seq1
                     (cdr seq2)
                     (cons (cons (teste (car seq1) (car seq2))
                                 (car out))
                           (cdr out))))))
  (aux phrases1 (reverse phrases2) (list null)))
