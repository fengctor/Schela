(define (filterM pred lst)
  (match lst
    [() '(())]
    [(cons x xs)
     (flatMap (lambda (b)
                ((if b (curry map (curry cons x)) id)
                 (filterM pred xs)))
              (pred x))]))
