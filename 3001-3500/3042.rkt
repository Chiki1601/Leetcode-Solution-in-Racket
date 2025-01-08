(define/contract (count-prefix-suffix-pairs words)
  (-> (listof string?) exact-integer?)
  (let loop ((current-word (car words))
             (candidates (cdr words))
             (cnt 0))
    (if (empty? candidates)
        cnt
        (loop (car candidates)
              (cdr candidates)
              (+ cnt (count
                      (lambda (candidate)
                        (and (string-prefix? candidate current-word)
                             (string-suffix? candidate current-word)))
                      candidates))))))
