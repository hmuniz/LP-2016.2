#lang racket
;Definido os procedimentos mais simples para fazer o teste de Fermat funcionar, que são o 'even', o  'square' e os demais apresentados na seção 1.26.
; Escolhi continuar com os procedimentos como explicitado no livro para que possamos manter uma uniformidade.

(define (square x)(* x x))

(define (even? n)(=(remainder n 2)0))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond((> ( square test-divisor) n) n)
       ((divides? test-divisor n) test-divisor)
  (else (find-divisor n (+ test-divisor 1)))))


(define (divides? a b)
  ( = (remainder b a) 0))

(define (prime? n)
  ( = n (smallest-divisor n)))

; Agora vou definir os procedimentos base do Teste de Fermat.
;O procedimento abaixo é necessário para podermos realizar o teste de Fermat, como explicado no livro. Usei o modelo lá explicitado.

(define(expmod base exp m)
  (cond ((= exp 0) 1)
        (( even? exp)
         (remainder (square (expmod base ( / exp 2) m))
                    m))
        (else
         (remainder ( * base ( expmod base ( - exp 1) m))
                    m))))
; Vou continuar com o modelo do livro para o exercício 1.22
; Obs: não consegui usar o runtime. Dei uma procurada na internet e uns caras usaram o 'current-inexact-milliseconds', mas tem que importar um módulo.
; http://docs.racket-lang.org/reference/time.html#%28def._%28%28quote._%7E23%7E25kernel%29._current-inexact-milliseconds%29%29
; Como eu ainda sou inciante no Racket, segui a dica deles, mas dvee ter outras maneiras de medir o tempo.

(require (lib "27.ss" "srfi")) 

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (current-inexact-milliseconds)))

; Tive alguns problemas nessa parte,sobretudo porque o Racket não estava aceitando o 'if'. Tentei usar o 'cond', mas fui descobrir depois de desbugar que a sintaxe estava errada.


(define (start-prime-test n start-time)
   (cond ((fast-prime? n 200)
          (report-prime (- (current-inexact-milliseconds) start-time)))))

; Percebam que o código acima foi modficado pra colocar o 'fast-prime?' do Método de Fermat. COloquei arbitrariamente o número de testes para 200.

(define (report-prime elapsed-time)
  (display "***")
  (display elapsed-time))

; Agora vem a parte do exercício 1.22 em si. Reparem que o meu código trabalha com palpites sobre o número final no procedimento.
; Dei uma pesquisada depois de fazer e parece que todo mundo que costuma resolver acab enveredando por esse caminho.
; Me pergunto se a gente poderia passar os primos que a gente encontra pra uma lista e quando o lenght dela chegar a três a gente 'matava' o procedimento.
; Se vocês quiserem tentar, fica a sugestão. Confessso que dei uma olhada na documentação sobre o assunto e fiquei um pouco confuso. Devo tentar futuramente.

(define (search-for-primes inicio fim)
   (if (even? inicio)
       (search-for-primes (+ inicio 1) fim)
       (cond ((< inicio fim) (timed-prime-test inicio) (search-for-primes (+ inicio 2) fim)))))

;Cara, dêem uma olhada na nota de de rodapé 19 do Capítulo 1. Demorei muito no meu código até descobri o que eu estava fazendo de errado na utilização do 'if' e do 'cond'.
; Meu código tava diferente, mas vi que a indentação que os caras usam segue um padrão, então adaptei as minhas expressões.



; Vamos usar agora o método de Fermat (finalmente)

(define (fermat-test n)
  (define (try-it a)
    ( = (expmod a n n ) a ))
  (try-it ( + 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond (( = times 0) true)
        (( fermat-test n) (fast-prime? n ( - times 1)))
        (else false)))



(search-for-primes 1000 1050)
(search-for-primes 10000 10500)
(search-for-primes 100000 105000)

;Me pareceu que o teste de Fermat leva mais tempo do que os algoritmos anteriores. Vocês acharam o mesmo resultado?
; Talvez seja pelo fato de eu ter colocado o número de testes para 200. Pode ser interessante 'brincar' com esse número, já que ele é arbitrário.

                      





