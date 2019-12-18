; hi
(define (filterM pred lst) ;; comment here
  (match lst ;; comment (there))))
    [() '(())]
    [(cons x xs)
     (flatMap (lambda (b)
                ((if b (curry map (curry cons x)) id)
                 (filterM pred xs)))
              (pred x))]))

;; strict Y combinator
(define Y (lambda (f) ((lambda (x) (f (lambda (v) ((x x) v))))
                       (lambda (x) (f (lambda (v) ((x x) v)))))))

(define Fact
  (Y (lambda (fact) (lambda (n) (if (zero? n) 1 (* n (fact (- n 1))))))))
(define Fib
  (Y (lambda (fib) (lambda (n) (if (<= n 1) n (+ (fib (- n 1)) (fib (- n 2))))))))
