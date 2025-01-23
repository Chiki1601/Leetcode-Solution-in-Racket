(define/contract (count-servers grid)
  (-> (listof (listof exact-integer?)) exact-integer?)
  
  (define servers '())
  
  ; Collect all the server positions (1s in the grid)
  (for ([row (in-list grid)] [i (in-naturals)])
    (for ([cell (in-list row)] [j (in-naturals)])
      (when (= cell 1)
        (set! servers (cons (list i j) servers)))))
  
  ; Count how many servers are in each row and each column
  (define servers-by-row (make-hash))
  (define servers-by-col (make-hash))
  
  (for ([server (in-list servers)])
    (define i (first server))
    (define j (second server))
    (hash-update! servers-by-row i add1 0)
    (hash-update! servers-by-col j add1 0))
  
  ; Find rows and columns with exactly one server
  (define single-server-rows
    (filter (lambda (key) (= (hash-ref servers-by-row key) 1))
            (hash-keys servers-by-row)))
  
  (define single-server-cols
    (filter (lambda (key) (= (hash-ref servers-by-col key) 1))
            (hash-keys servers-by-col)))
  
  ; Calculate possible isolated servers
  (define possible-isolated-servers '())
  (for ([i single-server-rows])
    (for ([j single-server-cols])
      (set! possible-isolated-servers (cons (list i j) possible-isolated-servers))))
  
  ; Remove isolated servers from the list
  (define remaining-servers
    (filter (lambda (server) (not (member server possible-isolated-servers)))
            servers))
  
  (length remaining-servers)
)
