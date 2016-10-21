#lang racket
(define (estimate-integral
         P x1 x2 y1 y2 tests)
  
  (define (random-in-range low high)
    (let ((range (- high low)))
      (+ low (random range))))

  (define (monte-carlo trials experiment)
    (define (iter trials-remaining trials-passed)
      (cond ((= trials-remaining 0)
             (/ trials-passed trials))
            ((experiment)
             (iter (- trials-remaining 1) (+ trials-passed 1)))
            (else
             (iter (- trials-remaining 1) trials-passed))))
    (iter trials 0))

  (define (does-it-lie?)
    (P (random-in-range (min x1 x2) (max x1 x2))
       (random-in-range (min y1 y2) (max y1 y2))))

  (define area-rect
    (* (- (max x1 x2) (min x1 x2))
       (- (max x1 x2) (min x1 x2))))

   (* area-rect (monte-carlo tests does-it-lie?)))