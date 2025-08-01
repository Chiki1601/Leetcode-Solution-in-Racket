(define/contract (calculate lst num)
 (-> (listof exact-integer?) exact-integer? (listof exact-integer?))
    (cond [(null? lst) (cons num null)]
    [else (cons (+ (first lst) num) (calculate (rest lst) (first lst)))]))

(define/contract (generate-helper numRows currRow)
(-> exact-integer? exact-integer? (listof (listof exact-integer?)))
(cond [(= 1 currRow) (list (list 1))]
          [else
           (let ([remaining (generate-helper numRows (sub1 currRow))])
             (cons (calculate (first remaining) 0) remaining))]))

(define/contract (generate numRows)
  (-> exact-integer? (listof (listof exact-integer?)))
  (reverse (generate-helper numRows numRows)))
