(define (find-judge- n trust trust-counts)
  (if (empty? trust)
      (if (index-of (vector->list trust-counts) (- n 1))
          (index-of (vector->list trust-counts) (- n 1))
          -1)
      (begin 
       (vector-set! trust-counts (caar trust)
                   (- (vector-ref trust-counts (caar trust)) 1))
       (vector-set! trust-counts (cadar trust)
              (+ (vector-ref trust-counts (cadar trust)) 1))
       (find-judge- n (cdr trust) trust-counts))
  ))

(define (find-judge n trust)
  (if (= n 1) 1
     (find-judge- n trust (make-vector (+ 1 n) 0))))
