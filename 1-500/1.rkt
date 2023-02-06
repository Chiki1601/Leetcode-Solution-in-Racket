(define(two-sum nums target)
  (define ht (make-hasheqv)) ; target-number -> index

  (for ([x nums] [i (in-naturals)])
    (hash-update! ht (- target x) (λ (i) i) i))

  (for/or ([y nums] [j (in-naturals)])
    (define i (hash-ref ht y #f))
    (and i
         (< i j)
         (list i j))))
