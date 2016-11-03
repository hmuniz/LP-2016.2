#lang racket
              ; Stream package
;; Constructor
(define-syntax cons-stream
    (syntax-rules ()
      ((cons-stream head tail)
       (cons head (delay tail)))))

;; Selectors
(define (stream-car s)
  (car s))

(define (stream-cdr s)
  (force (cdr s)))

;; Auxiliar functions
(define (stream-null? s)
  (eq? s the-empty-stream))

(define the-empty-stream '())

(define (display-stream s)
  (stream-for-each displayln s))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

              ; Footnote 12 in section 2.2.3
;; Scheme standardly provides a map procedure that is more
;; general than the one described here. This more general
;; map takes a procedure of n arguments, together with n
;; lists, and applies the procedure to all the first elements
;; of the lists, all the second elements of the lists, and so
;; on, returning a list of the results.

              ; Exercise 3.50
(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))

               ; Tests
(display-stream (stream-map + '(6 4 2) '(3 4 3) '(1 4 4)))
(display-stream (stream-map * '(6 4 2) '(3 4 3) '(1 4 4)))
(display-stream (stream-map - '(6 4 2) '(3 4 3) '(1 4 4)))
(display-stream (stream-map = '(6 4 2) '(3 4 3) '(1 4 4)))
(display-stream (stream-map > '(6 4 2) '(3 4 3) '(1 4 4)))
(display-stream (stream-map < '(6 4 2) '(3 4 3) '(1 4 4)))
                