(define/contract (length-after-transformations s t nums)
  (-> string? exact-integer? (listof exact-integer?) exact-integer?)

  (define MOD 1000000007)
  (define ALPHA 26)
  (define codeA (char->integer #\a))

  ;; Initialize the base transformation matrix
  (define base
    (for/vector ([i (in-range ALPHA)])
      (for/vector ([j (in-range ALPHA)])
        0)))

  ;; Fill the matrix with transformations
  (for ([i (in-range ALPHA)]
        [count nums])
    (for ([k (in-range 1 (+ count 1))])
      (define to (modulo (+ i k) ALPHA))
      (vector-set! (vector-ref base i) to
                   (add1 (vector-ref (vector-ref base i) to)))))

  ;; Initialize counts vector from string s
  (define counts
    (let ([v (make-vector ALPHA 0)])
      (for ([ch (in-string s)])
        (define idx (- (char->integer ch) codeA))
        (vector-set! v idx (add1 (vector-ref v idx))))
      v))

  ;; Matrix multiplication
  (define (mul-matrix A B)
    (for/vector ([i (in-range ALPHA)])
      (for/vector ([j (in-range ALPHA)])
        (define sum
          (for/sum ([k (in-range ALPHA)])
            (modulo (* (vector-ref (vector-ref A i) k)
                       (vector-ref (vector-ref B k) j))
                    MOD)))
        sum)))

  ;; Vector and matrix multiplication
  (define (mul-vector vec M)
    (for/vector ([j (in-range ALPHA)])
      (define sum
        (for/sum ([i (in-range ALPHA)])
          (modulo (* (vector-ref vec i)
                     (vector-ref (vector-ref M i) j))
                  MOD)))
      sum))

  ;; Matrix exponentiation
  (define (matrix-exponentiate mat exp)
    (define result
      (for/vector ([i (in-range ALPHA)])
        (for/vector ([j (in-range ALPHA)])
          (if (= i j) 1 0)))) ; Identity matrix

    (let loop ([mat mat] [exp exp] [res result])
      (if (= exp 0)
          res
          (loop (mul-matrix mat mat)
                (quotient exp 2)
                (if (odd? exp)
                    (mul-matrix res mat)
                    res)))))

  ;; Apply exponentiation and transformation
  (define power-mat (matrix-exponentiate base t))
  (define final-counts (mul-vector counts power-mat))

  ;; Sum all counts
  (foldl (λ (x acc) (modulo (+ x acc) MOD)) 0 (vector->list final-counts)))
