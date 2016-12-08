#lang racket
(define (invert-unit-series s) 
   (define inverted-unit-series 
     (cons-stream 1 (scale-stream (mul-streams (stream-cdr s) 
                                               inverted-unit-series) 
                                  -1))) 
   inverted-unit-series)