#lang racket

(define lista '(("1" "Disney" "nnp" "2" "nsubj") 
                ("2" "acquired" "verb" "0" "root")
                ("3" "the" "art" "4" "det")
                ("4" "Pixar" "nnp" "2" "dobj")))

(define (transf-aux-1 origem)
  (let ((id (list-ref (car origem) 0))
        (word (list-ref (car origem) 1))
        (class (list-ref (car origem) 2))
        (head-to (list-ref (car origem) 3))
        (by (list-ref (car origem) 4)))
    (if (eq? by "root")
        (list id class word)
        (transf-aux-1 (cdr origem)))))


(define (transf-aux-2 origem original)
  (if (null? origem)
      empty
      (let ((id (list-ref (car origem) 0))
            (word (list-ref (car origem) 1))
            (class (list-ref (car origem) 2))
            (head-to (list-ref (car origem) 3))
            (by (list-ref (car origem) 4))
            (aux (transf-aux-1 lista)))
        (if (eq? head-to (car aux))
            (append (list (append (list by (list class word))
				  (transf-aux-3 original id)))
		    (transf-aux-2 (cdr origem) original))
            (transf-aux-2 (cdr origem) original)))))


(define (transf-aux-3 origem ident)
  (if (null? origem)
      null
      (let ((id (list-ref (car origem) 0))
            (word (list-ref (car origem) 1))
            (class (list-ref (car origem) 2))
            (head-to (list-ref (car origem) 3))
            (by (list-ref (car origem) 4)))
        (if (eq? head-to ident)
            (append (list (list by (list class word)))
		    (transf-aux-3 (cdr origem) ident))
            (transf-aux-3 (cdr origem) ident)))))


(define (transf origem)
  (append (list "root" (cdr (transf-aux-1 origem)))
	  (transf-aux-2 origem origem)))
