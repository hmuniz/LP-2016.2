#lang racket

(require racket/trace rackunit)

(define (accumulate op initial sequence) 
   (if (null? sequence) 
       initial 
       (op (car sequence) 
           (accumulate op initial (cdr sequence))))) 
  
(define (accumulate-n op init sequence)  
   (if (null? (car sequence)) 
       null 
       (cons (accumulate op init (map car sequence)) 
             (accumulate-n op init (map cdr sequence))))) 
  
(define matriz '((1 2 3 4)
		 (5 6 7 8)
		 (9 10 11 12))) 

(define matriz-2 '((1 2 3 4)
		   (5 6 7 8)
		   (9 10 11 12)
		   (13 14 15 16))) 

(define matriz-i '((1 0 0 0)
		   (0 1 0 0)
		   (0 0 1 0)
		   (0 0 0 1)))


(define (dot-product v1 v2) 
   (accumulate + 0 (map * v1 v2))) 

(define (matrix-*-vector m v) 
   (map (lambda (m-linha) (dot-product m-linha v)) 
        m)) 

(define (transpose m) 
   (accumulate-n cons null m)) 
  
(define (matrix-*-matrix m n) 
   (let ((n-coluna (transpose n))) 
     (map (lambda (m-linha) (matrix-*-vector n-coluna m-linha)) 
          m)))


;;;;;;;;;;;;;;;;;;;
;;;;;; TESTS ;;;;;;
;;;;;;;;;;;;;;;;;;;

(check-equal? (dot-product (list 1 2 3) (list 4 5 6)) 32)

(check-equal? (matrix-*-vector matriz (list 1 2 3 4)) '(30 70 110))

(check-equal? (transpose matriz)
	      '((1 5 9) (2 6 10) (3 7 11) (4 8 12)))

(check-equal? (matrix-*-matrix matriz '((1 0) (0 1) (0 1) (1 0)))
	      '((5 5) (13 13) (21 21)))

(check-equal? (matrix-*-matrix matriz-2 matriz-i)
	      matriz-2)
