#lang racket

(define (getf list a)
  (if (equal? (car list) a)
      (cdr list)
      (getf (cddr list) a)))

(define english-1
  '((Initial (1))
    (Final (9))
    (From 1 to 3 by NP)
    (From 1 to 2 by DET)
    (From 2 to 3 by N)
    (From 3 to 4 by BV)
    (From 4 to 5 by ADV)
    (From 4 to 5 by |#|)
    (From 5 to 6 by DET)
    (From 5 to 7 by DET)
    (From 5 to 8 by |#|)
    (From 6 to 7 by ADJ)    
    (From 6 to 6 by MOD)
    (From 7 to 9 by N)
    (From 8 to 8 by MOD)
    (From 8 to 9 by ADJ)
    (From 9 to 4 by CNJ)
    (From 9 to 1 by CNJ)))

(define (initial-nodes network)
  (list-ref (assoc 'Initial network) 1))

(define (final-nodes network)
  (list-ref (assoc 'Final network) 1))

(define (transitions network)
  (cddr network))

(define (trans-node transition)
  (getf transition 'From))

(define (trans-newnode transition)
  (getf transition 'to))

(define (trans-label transition)
  (getf transition 'by))

(define abbreviations
  '((NP kim sandy lee emerson rafael)
    (DET a the her his)
    (N consumer man woman boy girl)
    (BV is was were)
    (CNJ and or )
    (ADJ happy stupid smart kind)
    (MOD very )
    (ADV often always sometimes never)))

(define (recognize network tape)
  (with-handlers(((lambda (x) (eq? exit-handler x))
                  (λ(s) #t)))
    (for-each (λ (node)
                (recognize-next node tape network))
              (initial-nodes network)))
  #f)

(define (recognize-next node tape network)
  (if (and (null? tape) (member node (final-nodes network)))
      (raise exit-handler #t)
      (map (transition (transitions network))
        (if (equal? node (trans-node transition))
            (map (newtape (recognize-move (trans-label transition) tape))
              (recognize-next (trans-newnode transition) newtape network))))))

(define (recognize-move label tape)
  (if (or (equal? label (car tape))
          (member (car tape) (assoc label abbreviations)))
      (list (cdr tape))
      (if (equal? label '|#|)
          (list tape)
          nil)))

(define (generate-move label tape)
  (if (list? label)
      (let ((results
        (map (newinput) (recognize-move (car label) (car tape))
             (map (newoutput) (generate-move (cadr label) (cadr tape)))
                  (set results (cons (list newinput newoutput) results)))))
        results)
      (if (equal? label '|#|)
          (list tape)
          (if (assoc label  abbreviations)
              (generate-move-list (cdr (assoc label abbreviations)) tape)))))
(define (generate-move-list labels tape)
  (if (null? labels)
      nil
      (append (generate-move (car labels) tape)
              (generate-move-list (cdr labels) tape))))


(define (generate-next node tape network)
  (if (and (null? (car tape))
           (member node (final-nodes network)))
      (raise exit-handler (cadr tape))
      (map (transition (transitions network))
           (if (equal? node (trans-node transition))
               (map (newtape (generate-move (trans-label transition) tape))
                    (generate-next (trans-newnode transition) newtape network))))))

(define (generate network)
  (map (initialnode (initial-nodes network))
       (generate-next initialnode nil network)))
