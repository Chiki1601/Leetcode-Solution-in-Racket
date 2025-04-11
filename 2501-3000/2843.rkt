(define number->list (compose1 string->list number->string))
(define (char->number c) (- (char->integer c) 48))

(define (count-symmetric-integers low high) 
  (stream-count (lambda (num)
                  (let* ([ls (number->list num)]
                         [l (length ls)])
                  (and (even? l) (symmetric? (map char->number ls) (/ l 2)))))
                (in-inclusive-range low high)))

(define (symmetric? cs n)
  (eq? (apply + (take cs n))
       (apply + (drop cs n))))
