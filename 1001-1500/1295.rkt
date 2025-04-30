(define/contract (find-numbers nums)
  (-> (listof exact-integer?) exact-integer?)

  (foldl (lambda (:integer
                  result)
           (if (or (< 9999 :integer 100000)
                   (< 99 :integer 1000)
                   (< 0 :integer 10))
               
               result
               
               (add1 result)))
         
         0
         nums))
