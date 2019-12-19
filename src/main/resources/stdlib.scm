;; foundational functions

(define (not x) (if x #f #t))

(define (id x) x)

(define (const x y) x)

(define (flip f) (lambda (x y) (f y x)))

(define (curry f x) (lambda (y) (f x y)))

(define (compose f g) (lambda (x) (f (g x))))

(define (list . xs) xs)

;; predicates

(define (null? l) (eqv? l '()))

(define zero? (curry = 0))

(define positive? (curry < 0))

(define negative? (curry > 0))

(define (odd? num) (= (mod num 2) 1))

(define (even? num) (= (mod num 2) 0))

;; math functions

(define (abs n) (if (negative? n) (* n (- 0 1)) n))

(define (gcd a b) (if (zero? a) b (gcd (mod b a) a)))

(define (lcm a b) (/ (* a b) (gcd a b)))

;; higher order list functions

(define (foldr f base lst)
  (match lst
         (() base)
         ((cons x xs) (f x (foldr f base xs)))))

(define reduce foldr)

(define (foldl f base lst)
  (match lst
         (() base)
         ((cons x xs) (foldl f (f base x) xs))))

(define fold foldl)

(define (unfold f init pred)
  (if (pred init)
    (list init)
    (cons init (unfold f (f init) pred))))

(define (map f xs)
  (foldr (lambda (c r) (cons (f c) r)) '() xs))

(define (filter pred xs)
  (foldr (lambda (c r) (if (pred c) (cons c r) r)) '() xs))

;; list functions

(define (length lst)
  (foldl (lambda (a c) (+ a 1)) 0 lst))

(define (reverse xs)
  (foldl (flip cons) '() xs))

(define (append xs ys)
  (foldr cons ys xs))

(define (flatten xxs)
  (foldr append '() xxs))

(define (flat-map f xs)
  (foldr (lambda (c r) (append (f c) r)) '() xs))

(define (range lo hi)
  (unfold (curry + 1) lo (curry <= hi)))

(define (zip xs ys)
  (if (|| (null? xs) (null? ys))
    '()
    (cons (cons (car xs) (car ys)) (zip (cdr xs) (cdr ys)))))

(define (zip-with f lst1 lst2)
  (match (cons lst1 lst2)
         [(cons '() _) '()]
         [(cons _ '()) '()]
         [(cons (cons x xs) (cons y ys))
          (cons (f x y) (zipWith f xs ys))]))

(define (split-at i xs)
  (define (splitHelp j l r)
    (cond [(null? l) (cons (reverse r) l)]
          [(<= j 0) (cons (reverse r) l)]
          [else (splitHelp (- j 1) (cdr l) (cons (car l) r))]))
  (splitHelp i xs '()))

(define (split-by pred xs)
  (foldr 
    (lambda (c r) (if (pred c) 
                    (cons (cons c (car r)) (cdr r)) 
                    (cons (car r) (cons c (cdr r)))))
    (cons '() '())
    xs))

(define (take n lst)
  (match lst
         [() '()]
         [(cons x xs) 
          (if (<= n 0) 
            '() 
            (cons x (take (- n 1) xs)))]))

(define (drop n lst)
  (match lst
         [() '()]
         [(cons x xs) 
          (if (<= n 0) 
            (cons x xs) 
            (drop (- n 1) xs))]))

(define (takeWhile pred lst)
  (match lst
         [() '()]
         [(cons x xs) 
          (if (pred x) 
            (cons x (takeWhile pred xs)) 
            '())]))

(define (dropWhile pred lst)
  (match lst
         [() '()]
         [(cons x xs) 
          (if (pred x) 
            (dropWhile pred xs) 
            (cons x xs))]))

(define (sort-by lte? lst)
  (define (merge lst1 lst2)
    (match (cons lst1 lst2)
           [(cons '() ys) ys]
           [(cons xs '()) xs]
           [(cons (cons x xs) (cons y ys))
            (if (lte? x y)
              (cons x (merge xs (cons y ys)))
              (cons y (merge (cons x xs) ys)))]))
  (define (mergePairs llst)
    (match llst
           [(cons lst1 (cons lst2 llsts))
            (mergePairs (cons (merge lst1 lst2) llsts))]
           [_ llst]))
  (match lst
         [() '()]
         [_ (car (mergePairs (map (curry (flip cons) '()) lst)))]))


(define (sort lst) (sort-by <= lst))

;; convenient vararg functions

(define (sum . ns) (foldl + 0 ns))

(define (product . ns) (foldl * 1 ns))

(define (and . bs) (foldl && #t bs))

(define (or . bs) (foldl || #f bs))

(define (max x . xs) (foldl (lambda (a c) (if (> c a) c a)) x xs))

(define (min x . xs) (foldl (lambda (a c) (if (< c a) c a)) x xs))

(define (all pred . xs) (foldl (lambda (a c) (&& (pred c) a)) #t xs))

(define (any pred . xs) (foldl (lambda (a c) (|| (pred c) a)) #f xs))
