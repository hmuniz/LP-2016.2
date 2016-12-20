#lang racket

(require racket/pretty)

(define x '(("1" "Disney" "nnp" "2" "nsubj") 
            ("2" "acquired" "verb" "0" "root") 
            ("3" "the" "art" "4" "det") 
            ("4" "Pixar" "nnp" "2" "dobj")))


(define y '(("1" "É" "cc" "9")
            ("2" "por" "case" "3")
            ("3" "isso" "nmod" "9")
            ("4" "que" "cc" "9")
            ("5" "," "punct" "6")
            ("6" "explica" "root" "0")
            ("7" "," "punct" "6")
            ("8" "não" "neg" "9")
            ("9" "tem" "ccomp" "6")
            ("10" "pena" "dobj" "9")
            ("11" "de" "case" "12")
            ("12" "Hillary" "nmod" "10")
            ("13" "Clinton" "name" "12")
            ("14" "." "punct" "6")))


(define (print-lista lista)
  (define (string-from-lista lista)
    (define (list-to-string lista str)
      (if (null? (cdr lista))
          (string-append str (car lista))
          (list-to-string (cdr lista)
                          (string-append str (car lista) " "))))
    (list-to-string lista ""))
  (pretty-display (map string-from-lista lista)))

(print-lista x)
