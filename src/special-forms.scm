(define global-env (new-env '()))

(define (add-built-in name func)
  (set-binding global-env name (cons "built-in" func)))

(define (add-special-form name func)
  (set-binding global-env name (cons "special-form" func)))

(define (validate-list list length-pred? param-pred?)
  (define (validate-iter lst index)
    (cond
      ((null? lst) #t)
      ((param-pred? index (car lst)) (validate-iter (cdr lst) (+ index 1)))
      (else #f)))
  (and
    (length-pred? (length list))
    (validate-iter list 0)))

(add-special-form "quote"
  (lambda (env actuals)
    (if (= (length actuals) 1)
      (eval-quote (cons "quote" (car actuals)))
      (raise "Bad syntax for special-form \"quote\""))))

;evaluate all but the last expression (TCO)
(add-special-form "begin"
  (lambda (env actuals)
    (if (> (length actuals) 0)
      (eval-iter env actuals)
      (raise "Bad syntax for special-form \"begin\""))))

(add-special-form "if"
  (lambda (env actuals)
    (if (= (length actuals) 3)
      (let (
        (cond-expr (car actuals))
        (then-expr (cadr actuals))
        (else-expr (caddr actuals)))
        (let ((condition (eval-expr env cond-expr)))
          (if (pfalse? condition)
            (new-tail-expr env else-expr)
            (new-tail-expr env then-expr))))
      (raise "Special-form \"if\" takes 3 expressions"))))

(add-special-form "and"
  (lambda (env actuals)
    (define (eval-iter actuals)
      (cond
        ((null? actuals) ptrue)
        ((pfalse? (eval-expr env (car actuals))) pfalse)
        (else (eval-iter (cdr actuals)))
      ))
    (eval-iter actuals)))

(add-special-form "or"
  (lambda (env actuals)
    (define (eval-iter actuals)
      (cond
        ((null? actuals) pfalse)
        ((not (pfalse? (eval-expr env (car actuals)))) ptrue)
        (else (eval-iter (cdr actuals)))
      ))
    (eval-iter actuals)))

(add-special-form "cond"
  (lambda (env actuals)
    (define (eval-clauses clauses)
      (if (null? clauses) pundef
        (let ((clause (cdar clauses)))
          (let (
            (cond-expr (car clause))
            (then-exprs (cdr clause)))
            (cond
              ((or
                (and (equal? (car cond-expr) "name") (equal? (cdr cond-expr) "else"))
                (not (pfalse? (eval-expr env cond-expr))))
                (eval-iter env then-exprs))
              (else (eval-clauses (cdr clauses)))
            )))))
    (if 
      (validate-list actuals
        (lambda (length) (> length 0))
        (lambda (index element)
          (and
            (equal? (car element) "list")
            (> (length (cdr element)) 1))))
      (eval-clauses actuals)
      (raise "Bad syntax for special-form \"cond\""))))

(define (new-lambda env formals exprs)
  (define (validate-formals formals)
    (define (find-dup name lst)
      (cond
        ((null? lst) #f)
        ((equal? (cdar lst) name) #t)
        (else (find-dup name (cdr lst)))))
    (if (null? formals) #t
      (if
        (or
          (not (equal? (caar formals) "name"))
          (find-dup (cdar formals) (cdr formals)))
        #f
        (validate-formals (cdr formals)))))
  (if
    (or
      (and (= (length formals) 2) (pdot? (car formals)) (equal? (caadr formals) "name"))
      (validate-formals formals))
    (cons "lambda"
      (list env formals exprs))
    (begin (displayln formals) (raise "Invalid formal parameter definition."))))

(add-special-form "lambda"
  (lambda (env actuals)
    (if (> (length actuals) 1)
      (cond
        ((equal? (caar actuals) "list")
          (let ((formals (cdar actuals)) (exprs (cdr actuals)))
            (new-lambda env formals exprs)))
        ((equal? (caar actuals) "name")
          (let ((formals (cons pdot (cons (car actuals) '()))) (exprs (cdr actuals)))
            (new-lambda env formals exprs)))
        (else (raise "Invalid formal parameter definition.")))
      (raise "lambda definition requires a formal definition and some expressions."))))

(add-special-form "define"
  (lambda (env actuals)
    (if (> (length actuals) 1)
      (let ((name (car actuals)) (value (cdr actuals)))
        (cond
          ((and (equal? (car name) "name") (= (length value) 1))
            (set-binding env (cdr name) (eval-expr env (car value))))
          ((and (equal? (car name) "list") (> (length (cdr name)) 0))
            (let (
              (lambda-name (cadr name))
              (lambda-formals (cddr name))
              (lambda-exprs value))
            (set-binding env (cdr lambda-name)
              (new-lambda env lambda-formals lambda-exprs))))
          (else (raise "Bad syntax for \"define\""))))
      (raise "Special-form \"define\" binds a name to a value"))
    pundef))

(add-special-form "let"
  (lambda (env actuals)
    (define (bind-iter env eval-env bindings)
      (if (null? bindings) env
        (let ((binding (car bindings)))
          (let ((name (cadr binding)) (expr (caddr binding)))
            (set-binding env (cdr name) (eval-expr eval-env expr))
            (bind-iter env eval-env (cdr bindings))))))
    (if (and (> (length actuals) 1) (equal? (caar actuals) "list"))
      (let (
        (bindings (cdar actuals))
        (exprs (cdr actuals)))
        (if
          (validate-list bindings
            (lambda (length) #t)
            (lambda (index element)
              (and
                (equal? (car element) "list")
                (= (length (cdr element)) 2)
                (equal? (caadr element) "name"))))
          (eval-iter (bind-iter (new-env env) env bindings) exprs)
          (raise "Bindings must be pairs of name and expression.")))
      (raise "Bad syntax for special-form \"let\""))))

(add-special-form "let*"
  (lambda (env actuals)
    (define (bind-iter env bindings)
      (if (null? bindings) env
        (let ((binding (car bindings)))
          (let ((name (cadr binding)) (expr (caddr binding)))
            (bind-iter
              (set-binding (new-env env) (cdr name) (eval-expr env expr))
              (cdr bindings))))))
    (if (and (> (length actuals) 1) (equal? (caar actuals) "list"))
      (let (
        (bindings (cdar actuals))
        (exprs (cdr actuals)))
        (if
          (validate-list bindings
            (lambda (length) #t)
            (lambda (index element)
              (and
                (equal? (car element) "list")
                (= (length (cdr element)) 2)
                (equal? (caadr element) "name"))))
          (eval-iter (bind-iter env bindings) exprs)
          (raise "Bindings must be pairs of name and expression.")))
      (raise "Bad syntax for special-form \"let*\""))))

(add-special-form "letrec"
  (lambda (env actuals)
    (define (bind-iter env bindings)
      (if (null? bindings) env
        (let ((binding (car bindings)))
          (let ((name (cadr binding)) (expr (caddr binding)))
            (bind-iter
              (set-binding env (cdr name) (eval-expr env expr))
              (cdr bindings))))))
    (if (and (> (length actuals) 1) (equal? (caar actuals) "list"))
      (let (
        (bindings (cdar actuals))
        (exprs (cdr actuals)))
        (if
          (validate-list bindings
            (lambda (length) #t)
            (lambda (index element)
              (and
                (equal? (car element) "list")
                (= (length (cdr element)) 2)
                (equal? (caadr element) "name"))))
          (eval-iter (bind-iter (new-env env) bindings) exprs)
          (raise "Bindings must be pairs of name and expression.")))
      (raise "Bad syntax for special-form \"let*\""))))

(add-special-form "with-handlers"
  (lambda (env actuals)
    (define (make-handlers clauses)
      (if (null? clauses) '()
        (let ((clause (car clauses)))
          (cons
            (cons (cadr clause) (caddr clause))
            (make-handlers (cdr clauses))))))
    (define (test-handlers handlers obj)
      (if (null? handlers) (raise obj)
        (let ((pred-lambda (caar handlers)) (handler-lambda (cdar handlers)))
          (if
            (not (pfalse?
              (eval-expr env (cons "list" (cons pred-lambda (cons obj '()))))))
            (eval-expr env (cons "list" (cons handler-lambda (cons obj '()))))
            (test-handlers (cdr handlers) obj)
        ))))
    (define (eval-iter exprs)
      (let ((result (eval-expr env (car exprs))))
        (if (null? (cdr exprs)) result (eval-iter (cdr exprs)))))
    
    (if
      (and
        (> (length actuals) 1)
        (equal? (caar actuals) "list")
        (validate-list (cdar actuals)
          (lambda (length) #t)
          (lambda (index element)
            (and
              (or (equal? (car element) "list") (equal? (car element) "bracket"))
              (= (length (cdr element)) 2)
            ))))
      (with-handlers
        (
          ((lambda (e) #t) (lambda (obj) (test-handlers (make-handlers (cdar actuals)) obj)))
        )
        (eval-iter (cdr actuals))
      )
      (raise "Bad syntax for special-form \"with-handler\""))))
