#lang racket

(require levenshtein rackunit racket/trace)

(define (matrix-of-difference list-1 list-2 proc)
  (define accu '())
  (for* ([i list-1]
         [j list-2])
    (set! accu (cons (proc i j) accu)))
  (reverse accu))


;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; TESTS ;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;:

(check-equal? (matrix-of-difference '("a" "b" "c") '("d" "a" "e") equal?)
              '(#f #t #f #f #f #f #f #f #f))

(check-equal? (matrix-of-difference '("pedro" "rademaker" "yuri")
                                    '("pedro" "b" "cdc") equal?)
              '(#t #f #f #f #f #f #f #f #f))

(check-equal? (matrix-of-difference '("pedro" "rademaker" "yuri")
                                    '("pedro" "daniel" "galo doido")
                                    levenshtein)
              '(0 6 8 8 7 9 4 5 9))

(check-equal? (matrix-of-difference '("a" "b" "c")
                                    '("a" "b" "c")
                                    levenshtein) '(0 1 1 1 0 1 1 1 0))

(check-equal? (matrix-of-difference '( '("outra lista" "com " "3 elementos")
                            '("pedro" "beleza"))
                            '( '("ai" "ai" "ai")
                          '("pedro" "arademaker"))
                          levenshtein) '(1 1 1 1))