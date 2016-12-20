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


(define (amr-to-graph-all amr)
  (append (amr-to-graph amr) (amr-to-graph-2 amr (car amr))))


(define (amr-to-graph amr)
  (define (append-all list)
    (if (empty? list)
      empty
      (append (car list) (append-all (cdr list)))))
  (if (pair? amr)
      (when (equal? (symbol->string (cadr amr)) "/")
        (append (list (list 'instance-of (car amr) (caddr amr)))
                (append-all (map (lambda (amr) (amr-to-graph amr)) (cdr amr)))))
      empty))


(define (amr-to-graph-2 amr first-item)
  (if (pair? amr)
      (if (equal? (string-ref (symbol->string (car amr)) 0) #\:)
          (if (pair? (cadr amr))
              (append (list (list (car amr) first-item (caadr amr)))
                      (amr-to-graph-2 (cadr amr) (caadr amr))
                      (amr-to-graph-2 (cddr amr) first-item))
              (cons (list (car amr) first-item (cadr amr))
                    (amr-to-graph-2 (cddr amr) first-item)))
          (amr-to-graph-2 (cdr amr) first-item))
      empty))