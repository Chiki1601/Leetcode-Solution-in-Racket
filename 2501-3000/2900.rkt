(define/contract (get-longest-subsequence words 
                                          groups 
                                          [preceding;UInt8
                                           2]
                                          [output;list:string?
                                  ;we never look at the words
                                           '()])
  (-> (listof string?) (listof exact-integer?) (listof string?))
  [if (null? words;list:string?
            )
      (reverse output;list:string?
               )
      (let
          ([car!groups;UInt8
            (car groups;list:UInt8
                 )])
        
        [if (= car!groups;UInt8
               preceding;UInt8
               )
            {get-longest-subsequence (cdr words;list:string?
                                          )
                                     (cdr groups;list:UInt8
                                          )
                                     preceding;UInt8
                                     output;list:string?
                                     }
            {get-longest-subsequence (cdr words;list:string?
                                          )
                                     (cdr groups;list:UInt8
                                          )
                                     car!groups;UInt8
                                     (cons (car words;list:string?
                                                );string
                                           output;list:string?
                                           )}])])
