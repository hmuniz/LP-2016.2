#lang racket

(require levenshtein)

(define (matriz lista-1 lista-2 teste)
  (matriz-aux lista-1 lista-2 '() teste))

(define (matriz-aux lista-1 lista-2 lista-final teste)
  (if (eq? lista-1 empty)
      (reverse lista-final)
      (matriz-aux (cdr lista-1)
                  lista-2
                  (cons (map (lambda (x) (teste (car lista-1) x)) lista-2)
                        lista-final)
                  teste)))
  
      
