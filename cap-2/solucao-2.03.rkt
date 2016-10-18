#lang racket
(define (make-point x y)
  (cons x y))

(define (x-point point)
  (car point))

(define (y-point point)
  (cdr point))

(define (make-segment start end)
  (if (equal? start end)
      (error "Foi dado um ponto, não um segmento")
      (cons start end)))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))

(define (mod-segment segment)
  (sqrt (+ (sqr (x-point (vetor segment)))
           (sqr (y-point (vetor segment))))))

(define (vetor segment)
  (cons (- (x-point (end-segment segment))
           (x-point (start-segment segment)))
        (- (y-point (end-segment segment))
           (y-point (start-segment segment)))))

(define (make-retangle base hight)
  (if (and (or (equal? (start-segment base) (start-segment hight)) ;Verificação de um ponto em comum
               (equal? (start-segment base) (end-segment hight))
               (equal? (end-segment base) (start-segment hight))
               (equal? (end-segment base) (end-segment hight)))
           (= (+ (* (x-point (vetor base)) ;Verificação de ortogonalidade
                    (x-point (vetor hight)))
                 (* (y-point (vetor base))
                    (y-point (vetor hight))))
              0))
      (cons base hight)
      (error "Os segmentos não são ortogonais e/ou não têm extremidade em comum.")))

(define (base retangle)
  (car retangle))

(define (hight retangle)
  (cdr retangle))

(define (perimetro retangle)
  (+ (* 2 (mod-segment (base retangle)))
     (* 2 (mod-segment (hight retangle)))))

(define (area retangle)
  (* (mod-segment (base retangle))
     (mod-segment (hight retangle))))

