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

(define newline
  (lambda ()
    (display "\n")))

(define add1 (lambda (n) (+ n 1)))
(define sub1 (lambda (n) (- n 1)))

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
