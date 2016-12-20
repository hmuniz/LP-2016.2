#lang racket
(define b '(w / want-01
    :arg0 (b / boy)
    :arg1 (g / go-01
	   :arg0 b)))


(define c '(S / SAY-01 :ARG0
              (G / ORGANIZATION :NAME (N / NAME :OP1 "UN"))
              :ARG1 (F / FLEE-01 :ARG0
                       (P / PERSON :QUANT (A / ABOUT :OP1 14000))
                       :ARG1 (H / HOME :POSS P)
                       :TIME (W / WEEKEND)
                       :TIME(A2 / AFTER :OP1
                                (W2 / WARN-01 :ARG1
                                    (T / TSUNAMI)
                                    :LOCATION (L / LOCAL))))))


(define (amr-to-graph amr)
  (if (pair? amr)
      (if (equal? (string-ref (symbol->string (car amr)) 0) #\:)
	  (cons (list (car amr) (amr-to-graph (cadr amr)))  (amr-to-graph (cddr amr)))
	  (if (and (not (empty? (cdr amr))) (equal? (symbol->string (cadr amr)) "/"))
	      (if (pair? (cdddr amr))
		  (cons (car amr) (list (amr-to-graph (cddr amr))))
		  (cons (car amr) (amr-to-graph (cddr amr))))
	      (cons (car amr) (amr-to-graph (cdr amr)))))
      amr))
