#lang racket

(require rackunit racket/trace)

(define disney
  '(("1" "Disney"  "nnp" "2"  "nsubj") 
    ("2" "acquired" "verb" "0" "root") 
    ("3" "the" "art" "4" "det") 
    ("4" "Pixar" "nnp" "2" "dobj")))

;função que retorna a sublista com a root
;no caso do token Disney, a segunda sublista é a root do token
(define (list-root phrase)
  (car (filter (lambda (x) (equal? "0" (list-ref x 3))) phrase)))

;função que retorna a palavra root, necessariamente o segundo elemento da lista root
;no caso do token Disney, o verbo "acquired" é o root
(define (word-root xs)
  (list-ref (list-root xs) 1))

;função que retorna o nó da root
;no caso do token Disney o nó é a string "2", isso será importante para encontrar os arrows
(define (node-root xs)
  (list-ref (list-root xs) 0))

;função que retorna todos os arrows direcionados a frase root
;no caso do token Disney, as palavras "Disney" e "Pixar" se referem a "acquired"
(define (list-arrow xs node)
  (filter (lambda (xs) (equal? node (list-ref xs 3))) xs))

;função que retorna o primeiro arrow da lista de arrows
(define (node-arrow arrow)
  (list-ref arrow 0))

;função que retorna o segundo arrow da lista de arrows
(define (word-arrow arrow)
  (list-ref arrow 1))

;função que retorna o último arrow da lista de arrows
(define (misc-arrow arrow)
  (last arrow))

;função que recebe uma palavra e um token, retornando a sua classe
;como "verb" de verbo ou "art" de artigo
(define (upostag word xs)
  (list-ref  (car (filter (lambda (x) (equal? (list-ref x 1) word)) xs)) 2))

;função que recebe uma lista de arrows, uma palavra e um token
;ela irá construir uma lista de acordo com a
;formatação pedida em https://github.com/arademaker/LP-2016.2/issues/59
;para isso, ela usa um processo recursivo
(define (build arrows word xs)
  (if (false? (empty? arrows)) ;se a lista de arrows não for vazia
      (list (last (car arrows)); criar uma lista com o último elemento do primeiro arrow
            (build (cdr arrows) word xs) ; chamada recursiva com o restante da lista de arrows
            (build (list-arrow xs (node-arrow (car arrows)))
                   (word-arrow (car arrows))
                   xs))
      (list (upostag word xs) word)))

;função do tipo higher order, que recebe uma lista token, como disney acima
;e retorna outra função, no caso build, que tem como input outras funções
(define (build-first-format xs)
  (build (list-arrow xs (node-root xs)) (word-root xs) xs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; TESTS ;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;:

(check-equal? disney '(("1" "Disney" "nnp" "2" "nsubj")
                       ("2" "acquired" "verb" "0" "root")
                       ("3" "the" "art" "4" "det")
                       ("4" "Pixar" "nnp" "2" "dobj")))

(check-equal?  (list-root disney)
               '("2" "acquired" "verb" "0" "root"))

(check-equal? (word-root disney) "acquired")

(check-equal? (node-root disney) "2")

(check-equal? (list-arrow disney "2")
              '(("1" "Disney" "nnp" "2" "nsubj") ("4" "Pixar" "nnp" "2" "dobj")))

(check-equal? (node-arrow (list-arrow disney "2"))
              '("1" "Disney" "nnp" "2" "nsubj"))

(check-equal? (word-arrow (list-arrow disney "2"))
              '("4" "Pixar" "nnp" "2" "dobj"))

(check-equal? (misc-arrow (list-arrow disney "2"))
              '("4" "Pixar" "nnp" "2" "dobj"))

(check-equal? (build-first-format disney)
              '("nsubj" ("dobj" ("verb" "acquired") ("det" ("nnp" "Pixar") ("art" "the"))) ("nnp" "Disney")))

(check-equal? (build '(("1" "Disney" "nnp" "2" "nsubj") ("4" "Pixar" "nnp" "2" "dobj"))
                     "acquired"
                     disney)
              '("nsubj" ("dobj" ("verb" "acquired") ("det" ("nnp" "Pixar") ("art" "the"))) ("nnp" "Disney")))