#lang racket

;PRIMEIRA CONSTRUCAO

(define (make-frame1 origin edge1 edge2)
  (list origin edge1 edge2))
 
(define (frame-origin1 frame)
  (car frame))
 
(define (frame-edge-1.1 frame)
  (car (cdr frame)))
 
(define (frame-edge-1.2 frame)
  (car (cdr (cdr frame))))

;SEGUNDA CONSTRUCAO

(define (make-frame2 origin edge1 edge2)
  (cons origin (cons edge1 edge2)))
 
(define (frame-origin2 frame)
  (car frame))
 
(define (frame-edge-2.1 frame)
  (car (cdr frame)))
 
(define (frame-edge-2.2 frame)
  (cdr (cdr frame)))