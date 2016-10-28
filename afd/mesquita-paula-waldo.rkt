#lang racket
"Deixamos o english-1 dado em questao, pois usamos oara testar o generate." 
(require racket/trace)
(define english-1-initial
  '((Initial (1))
    (Final (9))
    (From 1 to 3 by NP)
    (From 1 to 2 by DET)
    (From 2 to 3 by N)
    (From 3 to 4 by VS)
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

"A primeira sublista gera e recebe frases afirmativas, a segunda frases interrogativas."
(define english-1
  '(((Initial (1))
     (Final (9))
     (From 1 to 3 by NP)
     (From 1 to 2 by DET)
     (From 2 to 3 by N)
     (From 3 to 4 by VS)
     (From 4 to 5 by ADV)
     (From 4 to 5 by |#|)
     (From 5 to 6 by DET)
     (From 5 to 7 by DET)
     (From 5 to 8 by |#|)
     (From 6 to 7 by ADJ)    
     (From 6 to 6 by MOD)
     (From 7 to 6 by CNJ)
     (From 7 to 9 by N)
     (From 8 to 8 by MOD)
     (From 8 to 9 by ADJ)
     (From 9 to 4 by CNJ)
     (From 9 to 1 by CNJ))
    ((Initial (1))
     (Final (14))
     (From 1 to 2 by VS)
     (From 1 to 3 by VP)
     (From 1 to 4 by QW)
     (From 2 to 6 by NP)
     (From 3 to 5 by NP)
     (From 4 to 7 by VP)
     (From 4 to 8 by VS)
     (From 5 to 9 by CNJ)
     (From 6 to 13 by ADJ)
     (From 7 to 12 by NP)
     (From 8 to 13 by NP)
     (From 9 to 10 by NP)
     (From 10 to 9 by CNJ)
     (From 10 to 13 by ADJ)
     (From 11 to 7 by |#|)
     (From 11 to 13 by NP)
     (From 12 to 11 by CNJ)
     (From 13 to 14 by QM)
     (From 13 to 14 by |#|))))




; como Racket não possui uma função getf nativa, optamos por escrevê-la em Racket, 
; ao invés de utilizar alternativas que não têm a mesma clareza e abstração do
; do código original
(define (getf lst key (default null))
  (cond ((null? lst) default)
        ((null? (cdr lst)) default)
        ((eq? (car lst) key) (cadr lst))
        (else (getf (cddr lst) key default))))

(define (initial-nodes network)
  (list-ref (assoc 'Initial network) 1))

(define (final-nodes network)
  (list-ref (assoc 'Final network) 1))

(define (transitions network)
  (filter (lambda (x) (eq? (car x) 'From)) network))

(define (trans-node transition)
  (getf transition 'From))

(define (trans-newnode transition)
  (getf transition 'to))

(define (trans-label transition)
  (getf transition 'by))

; adicionamos um termo vazio à nossa lista de abreviações. Para evitar um erro na 
; recognize-next.
(define abbreviations
  '((NP kim sandy lee)
    (DET a the her)
    (N consumer man woman)
    (VS is was)
    (CNJ and or)
    (ADJ happy stupid)
    (MOD very)
    (ADV often always sometimes)
    (VP are were)
    (QW how where who)
    (QM ?)
    (|#|)))
" Dependendo da estrutura da frase se escolhe ou a sublista de afirmar=tiva ou de pergunta"
(define (recognize network tape)
  (if (member (car tape) (or (assoc 'NP abbreviations) (assoc 'DET abbreviations)))
      (set! network (car network))
      (set! network (cadr network)))
  (with-handlers ([(lambda (x) (equal? x 'stop)) (lambda (v) #t)])
    (for ((initialnode (initial-nodes network)))
      (recognize-next initialnode tape network))
    #f))

(define (recognize-next node tape network)
  (if (and (null? tape) (member node (final-nodes network)))
      (raise 'stop #t)
      (for ((transition (transitions network)))
        (if (equal? node (trans-node transition))
            (for ((newtape (recognize-move (trans-label transition) tape)))
              (recognize-next (trans-newnode transition) newtape network))
            null))))

(define (recognize-move label tape)
  (if (or (equal? label (car tape))
          (member (car tape) (assoc label abbreviations)))
      (list (cdr tape))
      (if (equal? label '|#|)
          (list tape)
          null)))
" Criamos primeiramente a funcao generate-initial que funcionou corretamente com a lista
inicial, porem ao implementarmos emenglish-1 houve um erro de memoria. o generate
com a tag de question gerou algumas perguntas corretamente, porem o erro aconteceu depois.
Como a estrutura dos dados sao iguais em english-1-initial e ela funcionou com o codigo de generate
initial, nao entendemos o porque do erro de memoria"
(define (generate-move label tape)
  (cdr (assoc label abbreviations)))

(define (generate-next node tape network)
  (if (member node (final-nodes network))
      (print tape)
      (for ((transition (transitions network)))
        (if (equal? node (trans-node transition))
            (for ((newtape (generate-move (trans-label transition) tape)))
              (generate-next (trans-newnode transition) (append tape (list newtape)) network))
            'pass))))


(define (generate network tag)
  (begin ((if (equal? tag 'question)
              (set! network (cadr network))
              (set! network (car network)))
          (for ((initialnode (initial-nodes network)))
            (generate-next initialnode null network)))))


(define (generate-initial network)
  (for ((initialnode (initial-nodes network)))
    (generate-next initialnode null network)))


(trace recognize)
(trace recognize-next)
(trace recognize-move)