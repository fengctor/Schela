(define (not x) (if x #t #f))

(define (null? l) (eqv? l '()))

(define (list . xs) xs)


(define (id x) x)

(define (flip f) (lambda (x y) (f y x)))

(define (curry f x) (lambda (y) (f x y)))

(define (compose f g) (lambda (x) (f (g x))))

(define zero? (curry = 0))

(define positive? (curry < 0))

(define negative? (curry > 0))

(define (odd? num) (= (mod num 2) 1))

(define (even? num) (= (mod num 2) 0))

(define (abs n) (if (negative? n) (* n (- 0 1)) n))

(define (gcd a b) (if (zero? a) b (gcd (mod b a) a)))

(define (lcm a b) (/ (* a b) (gcd a b)))

(define (foldr f base xs)
  (if (null? xs)
    base
    (f (car xs) (foldr f base (cdr xs)))))

(define reduce foldr)

(define (foldl f base xs)
  (if (null? xs)
    base
    (foldl f (f base (car xs)) (cdr xs))))

(define fold foldl)

(define (unfold f init pred)
  (if (pred init)
    (list init)
    (cons init (unfold f (f init) pred))))

(define (map f xs)
  (foldr (lambda (c r) (cons (f c) r)) '() xs))

(define (filter pred xs)
  (foldr (lambda (c r) (if (pred c) (cons c r) r)) '() xs))

(define (reverse xs)
  (foldl (flip cons) '() xs))

(define (append xs ys)
  (foldr cons ys xs))

(define (flatten xxs)
  (foldr append '() xxs))

(define (flatMap f xs)
  (foldr (lambda (c r) (append (f c) r)) '() xs))

(define (sum . ns) (foldl + 0 ns))

(define (product . ns) (foldl * 1 ns))

(define (and . bs) (foldl && #t bs))

(define (or . bs) (foldl || #f bs))

(define (max x . xs) (foldl (lambda (a c) (if (> c a) c a)) x xs))

(define (min x . xs) (foldl (lambda (a c) (if (< c a) c a)) x xs))

(define (all pred . xs) (foldl (lambda (a c) (&& (pred c) a)) #t xs))

(define (any pred . xs) (foldl (lambda (a c) (|| (pred c) a)) #f xs))

(define (range lo hi)
  (unfold (curry + 1) lo (curry <= hi)))

(define (zip xs ys)
  (if (|| (null? xs) (null? ys))
    '()
    (cons (cons (car xs) (car ys)) (zip (cdr xs) (cdr ys)))))

(define (splitAt i xs)
  (define (splitHelp j l r)
    (cond [(null? l) (cons (reverse r) l)]
          [(<= j 0) (cons (reverse r) l)]
          [else (splitHelp (- j 1) (cdr l) (cons (car l) r))]))
  (splitHelp i xs '()))

(define (splitBy pred xs)
  (foldr 
    (lambda (c r) (if (pred c) 
                    (cons (cons c (car r)) (cdr r)) 
                    (cons (car r) (cons c (cdr r)))))
    (cons '() '())
    xs))
