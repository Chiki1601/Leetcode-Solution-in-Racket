(define/contract (make-the-integer-zero num1 num2)
  (-> exact-integer? exact-integer? exact-integer?)
  (let loop ([t 0])
    (cond
      [(> t 60) -1]
      [else
       (define s (- num1 (* t num2)))
       (cond
         [(< s 0) (loop (add1 t))]
         [(< s t) (loop (add1 t))]
         [else
          (define ones (bit-count s))
          (if (<= ones t)
              t
              (loop (add1 t)))])])))

;; helper: count number of 1 bits in binary representation
(define (bit-count n)
  (let loop ([x n] [count 0])
    (if (zero? x)
        count
        (loop (bitwise-and x (sub1 x)) (add1 count)))))
