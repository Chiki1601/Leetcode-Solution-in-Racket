(define (find-left left maxVal count vec k)
  (if (>= count k)
      (if (eq? (vector-ref vec left) maxVal)
          (find-left (add1 left) maxVal (sub1 count) vec k)
          (find-left (add1 left) maxVal count vec k))
      left))
      

(define (count-subarrays nums k)
  (let ([maxVal (foldl max 0 nums)]
        [vec (list->vector nums)])
    (let while ([left 0]
                [nums nums]
                [count 0])
      (if (null? nums)
          left
          (if (eq? (car nums) maxVal)
              (+ left (while (find-left left maxVal (add1 count) vec k) (cdr nums) (if (>= (add1 count) k) (sub1 k) (add1 count))))
              (+ left (while (find-left left maxVal count vec k) (cdr nums) count)))))))
