(define/contract (get-words-in-longest-subsequence words groups)
  (-> (listof string?) (listof exact-integer?) (listof string?))

  ;; Helper function to check if two strings differ by exactly one character
  (define (is-one-char-diff s1 s2)
    (let loop ((i 0) (diff? #f))
      (cond
        [(= i (string-length s1)) diff?]
        [(char=? (string-ref s1 i) (string-ref s2 i))
         (loop (add1 i) diff?)]
        [diff? #f]  ;; Already found a difference before
        [else (loop (add1 i) #t)])))

  ;; Convert lists to vectors for efficient indexing
  (define word-vec (list->vector words))
  (define group-vec (list->vector groups))
  (define len (vector-length word-vec))

  ;; Initialize dp and prev
  (define dp (make-vector len 1))
  (define prev (make-vector len -1))

  (define max-len 1)
  (define i-max-len 0)

  (for ([i (in-range 1 len)])
    (define cur-word (vector-ref word-vec i))
    (define cur-group (vector-ref group-vec i))
    (define cur-len 1)
    (define cur-prev -1)

    (for ([j (in-range 0 i)])
      (define prev-word (vector-ref word-vec j))
      (define prev-group (vector-ref group-vec j))
      (define prev-len (vector-ref dp j))

      (when (and (= (string-length cur-word) (string-length prev-word))
                 (not (= cur-group prev-group))
                 (<= cur-len prev-len)
                 (is-one-char-diff cur-word prev-word))
        (set! cur-len (+ 1 prev-len))
        (set! cur-prev j)))

    (vector-set! dp i cur-len)
    (vector-set! prev i cur-prev)

    (when (> cur-len max-len)
      (set! max-len cur-len)
      (set! i-max-len i)))

  ;; Reconstruct result sequence
  (define (build-result idx acc)
    (if (= idx -1)
        acc
        (build-result (vector-ref prev idx) (cons (vector-ref word-vec idx) acc))))

  (build-result i-max-len '()))
