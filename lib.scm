(define-syntax let
  (syntax-rules ()
    [(let ([var expr] ...) body ...)
     ((lambda (var ...) body ...) expr ...)]))

(define-syntax or
  (syntax-rules ()
    [(or) #f]
    [(or e) e]
    [(or e1 e2 ...)
     (let ([temp e1])
       (if temp temp (or e2 ...)))]))

(define-syntax and
  (syntax-rules ()
    [(and) #t]
    [(and e) e]
    [(and e1 e2 ...)
     (let ([temp e1])
       (if (not temp) #t (and e2 ...)))]))

(define-syntax cond
  (syntax-rules (else)
    [(cond (else e1 e2 ...))
     (begin e1 e2 ...)]
    [(cond (e0 e1 e2 ...))
     (if e0 (begin e1 e2 ...))]
    [(cond (e0 e1 e2 ...) c1 c2 ...)
     (if e0 (begin e1 e2 ...)
            (cond c1 c2 ...))]))

(define newline
  (lambda ()
    (display "\n")))

(define add1 (lambda (n) (+ n 1)))
(define sub1 (lambda (n) (- n 1)))
(define zero? (lambda (n) (equal? 0 n)))
(define even? (lambda (n) (zero? (remainder n 2))))
(define odd? (lambda (n) (not (even? n))))

(define list (lambda ls ls))

(define list? (lambda (ls)
  (if (null? ls)
    #t
    (if (pair? ls)
      (list? (cdr ls))
      #f))))

(define length (lambda (ls)
  (define loop (lambda (ls count)
    (if (null? ls)
      count
      (loop (cdr ls) (add1 count)))))
  (loop ls 0)))

(define map (lambda (f . lss)
  (define map1 (lambda (g ls)
    (if (not (pair? ls))
      '()
      (cons (g (car ls))
            (map1 g (cdr ls))))))
  (if (not (pair? (car lss)))
    '()
    (cons (apply f (map1 car lss))
          (apply map f (map1 cdr lss))))))

(define append (lambda (ls . rest)
  (if (null? ls)
    (if (null? rest)
      '()
      (apply append rest))
    (cons (car ls)
          (apply append (cdr ls) rest)))))
