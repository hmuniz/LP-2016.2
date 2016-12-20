#lang racket

;;; EXERCÍCIO 1 DE 3 (DISTANCE) ;;;

(require levenshtein) ;biblioteca necessária para calcular distância
(require htdp/matrix) ;biblioteca necessária para formar a matriz
(define (distance a b) ;função auxiliar para "difference"
  (string-levenshtein a b))
(define (difference lst-1 lst-2) ;função principal
  (let ((p (length lst-1))
        (b (length lst-2)))
    (define (auxdiff lst-1 lst-2 out)
      (if (null? lst-1)
          (make-matrix p b (reverse out))
          (begin (for ((i lst-2))
                   (set! out (append (list (distance (car lst-1) i)) out)))
                 (auxdiff (cdr lst-1) lst-2 out))))
    (auxdiff lst-1 lst-2 '())))

