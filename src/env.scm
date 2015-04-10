(define (new-env parent-env)
  (mcons parent-env '()))

(define (set-binding env name value)
  (cond
    ((null? (mcdr env))
      (set-mcdr! env (mcons (cons name value) '())))
    ((equal? (car (mcar (mcdr env))) name)
      (set-mcar! (mcdr env) (cons name value)))
    (else (set-binding (mcdr env) name value)))
  env)

(define (lookup env name)
  (define (lookup-iter lst)
    (cond
      ((null? lst) (lookup (mcar env) name))
      ((equal? (car (mcar lst)) name) (cdr (mcar lst)))
      (else (lookup-iter (mcdr lst)))))
  (if (null? env)
    (raise (string-append "unbound name " name))
    (lookup-iter (mcdr env))))
