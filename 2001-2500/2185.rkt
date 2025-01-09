(define/contract (prefix-count words pref)
  (-> (listof string?) string? exact-integer?)
  (apply + (map (lambda (x) (if (string-prefix? x pref) 1 0)) words)))
