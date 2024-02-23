(define (find-cheapest-price n flights src dst k)
  (define inf-cost (* #e1e4 (+ k 2)))

  (define edges
    (let ([es (make-hash)])
      (for ([f flights])
        (hash-update! es (first f) (λ (old) (cons f old)) '()))
      (λ (from) (hash-ref es from '()))))

  (define (go from stops)
    (cond [(= from dst) 0]
          [(= stops 1) inf-cost]
          [else (for/fold ([min-cost inf-cost])
                          ([edge (edges from)])
                  (min min-cost
                       (+ (third edge)
                          (go-mem (second edge) (sub1 stops)))))]))

  (define go-mem
    (let ([mem (make-hash)])
      (λ (from stops)
        (define key (list from stops))
        (when (not (hash-has-key? mem key))
          (hash-set! mem key (go from stops)))
        (hash-ref mem key))))

  (define ans (go src (+ k 2)))
  (if (>= ans inf-cost)
      -1
      ans))
