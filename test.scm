(define (filterM pred lst) ;; comment here
  (match lst ;; comment (there))))
    [() '(())]
    [(cons x xs)
     (flat-map (lambda (b)
                ((if b (curry map (curry cons x)) id)
                 (filterM pred xs)))
              (pred x))]))

(define Fact
  (Y (lambda (fac) (lambda (n) (if (zero? n) 1 (* n (fac (- n 1))))))))
(define Fib
  (Y (lambda (fib) (lambda (n) (if (<= n 1) n (+ (fib (- n 1)) (fib (- n 2))))))))

(define (prime? n)
  (define (primeAcc i)
    (cond
      [(> (* i i) n) #t]
      [(zero? (mod n i)) #f]
      [else (primeAcc (+ i 1))]))
  (&& (> n 1) (primeAcc 2)))
