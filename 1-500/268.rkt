(define/contract (missing-number nums)
  (-> (listof exact-integer?) exact-integer?)
  (apply bitwise-xor (append nums (range 0 (add1 (length nums))))))
