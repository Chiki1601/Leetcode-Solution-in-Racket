(define/contract (distance n from to)
    (-> exact-integer? exact-integer? exact-integer? exact-integer?)
    (if (<= from n) (+ (- (min (add1 n) to) from) (distance n (* 10 from) (* 10 to))) 0))

(define/contract (find-kth-number n k)
  (-> exact-integer? exact-integer? exact-integer?)
  (let recur ([curr 1][k (sub1 k)])
    (cond 
        [(zero? k) curr]
        [else (let [(steps (distance n curr (add1 curr)))]
            (if (<= steps k) (recur (add1 curr) (- k steps))
                (recur (* 10 curr) (sub1 k))))])))
