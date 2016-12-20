#lang racket

(define (root? x)
  (eq? (list-ref x 4) "root"))

;; porque o make-tree precisa ser definido tao internamente?

(define (trab-3 lst)
  (let ((a (car (filter root? lst))))
    (define (iter lst a)
      (cons (list-ref a 4)
	    (cons (cons (list-ref a 2) (list-ref a 1))
		  (let ((b (filter (lambda (x) (eq? (list-ref x 3) (car a)))
				   lst)))
		    (define (make-tree lst b) (if (eq? b '())
						  empty
						  (cons (iter lst (car b))
							(make-tree lst (cdr b)))))
		    (make-tree lst b)))))
    (iter lst a)))
