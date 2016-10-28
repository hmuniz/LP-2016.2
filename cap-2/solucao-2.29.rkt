#lang racket

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

; (a)

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
   (car (cdr mobile)))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (car (cdr branch)))

; (b)

(define (branch-weight branch)
   (if (pair? (branch-structure branch))
       (total-weight (branch-structure branch))
       (branch-structure branch)))

(define (total-weight mobile)
   (+ (branch-weight (left-branch mobile))
      (branch-weight (right-branch mobile))))

; (c)

(define (branch-torque branch)
   (* (branch-length branch)
      (branch-weight branch)))

(define (branch-balanced? branch)
   (if (pair? (branch-structure branch))
       (balanced? (branch-structure branch))
       true))

(define (balanced? mobile)
   (and (= (branch-torque (left-branch mobile))
           (branch-torque (right-branch mobile)))
        (branch-balanced? (left-branch mobile))
        (branch-balanced? (right-branch mobile))))


; (d)

(define (make-mobile left right)
  (cons left right))

(define (make-branch length structure)
  (cons length structure))

(define (right-branch mobile)
  (cdr mobile))

(define (branch-structure branch)
  (cdr branch))
        
