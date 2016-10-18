#lang racket

; Primeiro código que tentei
;(define (even2 n)
;  (if (even? n) n 0 ))
;
;(define (odd2 n)
;  (if (odd? n) n 0))
;
;(define (same-parity first . rest)
;  (cond ((empty?((even? first)(cons first((for-each even? rest)))
;        ((odd? first)(cons first (for-each odd? rest)))))

(define lista (list 1 2 3 4 5 6 7))

; Esse código não é meu, mas peguei a ideia dele
;(define (same-parity a . z)
;   (define (iter items answer)
;     (if (null? items)
;         answer
;         (iter (cdr items)
;               (if (= (remainder (car items) 2)
;                      (remainder a 2))
;                   (append answer (list (car items)))
;                   answer))))
;   (iter z (list a)))


(define (same-parity first . last)
  (define (aux items n)
    (if (empty? items)
        n
        (aux (cdr items)
             (if (= (remainder (car items) 2)
                    (remainder first 2))
                 (append n (list (car items)))
                 n))))
  (aux last (list first)))
        