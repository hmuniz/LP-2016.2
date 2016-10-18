#lang racket
;Esse código a gente não vai rodar, mas eu vou colocar o código  aqui pra vocês visualizarem melhor sem precisar recorrer ao livro.
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))


;Normal-order evaluation
;Lembrando que a normal-order evaluation não avalia o valor dos 'operands' até les serem necessários: vai ficar uma expressão bem longa.
(gcd 206 40)
(gcd 40 (remainder 206 40))
(gcd (remainder 206 40)(remainder 40 (remainder 206 40)))
(gcd (remainder 40 (remainder 206 40)) (remainder ((remainder 206 40) (remainder 40 (remainder 206 40)))))

;Não vou desenvolver mais a expressão, mas já dá pra perceber que o processo é longo.
; Me lembra até a figura 1.3 do livro, que ele usa pra ilustrar um processo recursivo.                                                  

                                                  
;Applicative order
;Lembrando que a applicative order avalia os operadores no decorrer do processo
; Dá pra ver que a order of growth dele em relação ao espaço é bem menor, assim como a order of growth do número de passos - proporcional ao log de n,como colocado no livro.
(gcd 206 40)
(gcd 40 (remainder 206 40))
(gcd 40 6)
(gcd 6 (remainder 40 6))
(gcd 6 4)
(gcd 4 (remainder 6 4))
(gcd 4 2)
(gcd 2 (remainder 4 2))
(gcd 2 0)
> 2