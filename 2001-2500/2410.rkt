(define/contract (match-players-and-trainers players trainers)
  (-> (listof exact-integer?) (listof exact-integer?) exact-integer?)
  (let ([pl (sort players <)]
        [tr (sort trainers <)])
    (let loop ([p pl] [t tr])
      (cond ((or (null? t) (null? p)) 0)
            ((<= (first p) (first t))
             (add1 (loop (cdr p) (cdr t))))
            (else
             (loop p (cdr t)))))))
