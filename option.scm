;; an example library for working with `option`s

;; data definitions
(struct none)
(struct some (x))

;; type predicates
(define (none? opt)
  (match opt
         ((none) #t)
         (_ #f)))
(define (some? opt)
  (match opt
         ((some _) #t)
         (_ #f)))

;; produces the value wrapped in the `option` if applicable,
;;  otherwise produces the given `default`
(define (opt-get-or-else opt default)
  (match opt
         ((none) default)
         ((some x) x)))

(define (opt-to-list opt)
  (match opt
         ((none) '())
         ((some x) (list x))))

;; filter for `option`
(define (opt-filter pred opt)
  (match opt
         ((none) none)
         ((some x) (if (pred x) (some x) none))))

;; maps a function f over the `option` structure
(define (opt-map f opt)
  (match opt
         ((none) none)
         ((some x) (some (f x)))))

;; monadic bind for `option`
(define (opt-bind opt f)
  (match opt
         ((none) none)
         ((some x) (f x))))

;; kleisli composition; for functions a -> `option` b with b -> `option c`
;;  giving a function a -> `option c`
(define (opt-kleisli-compose f1 f2)
  (lambda (a) (opt-bind (f1 a) f2)))
