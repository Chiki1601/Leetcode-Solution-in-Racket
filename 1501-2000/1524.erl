(define/contract (num-of-subarrays arr)
  (-> (listof exact-integer?) exact-integer?)
  (let recur ([arr arr][odd-count 0][even-count 0])
    (cond
        [(empty? arr) 0]
        [(odd? (first arr)) (remainder (+ (add1 even-count) (recur (rest arr) (add1 even-count) odd-count)) 1000000007)]
        [(even? (first arr)) (remainder (+ odd-count (recur (rest arr) odd-count (add1 even-count))) 1000000007)])))
