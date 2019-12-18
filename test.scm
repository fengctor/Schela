; hi
(define (filterM pred lst) ;; comment here
  (match lst ;; comment (there))))
    [() '(())]
    [(cons x xs)
     (flatMap (lambda (b)
                ((if b (curry map (curry cons x)) id)
                 (filterM pred xs)))
              (pred x))]))
