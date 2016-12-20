#lang racket

(define (make-vet x y) (cons x y)) 

(define (xcor-vet vet) (car vet)) 

(define (ycor-vet vet) (cdr vet))

(define (eq-vet? v1 v2) 
  (and (= (xcor-vet v1) (xcor-vet v2)) 
       (= (ycor-vet v1) (ycor-vet v2)))) 
  
(define (add-vet v1 v2) 
  (make-vet (+ (xcor-vet v1) (xcor-vet v2)) 
             (+ (ycor-vet v1) (ycor-vet v2)))) 

(define (sub-vet v1 v2) 
  (make-vet (- (xcor-vet v1) (xcor-vet v2)) 
             (- (ycor-vet v1) (ycor-vet v2)))) 

(define (scale-vet s vet) 
  (make-vet (* s (xcor-vet vet)) 
             (* s (ycor-vet vet))))
