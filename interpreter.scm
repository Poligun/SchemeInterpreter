(define (repl . args)

;primitive.scm
(define ptrue (cons "boolean" "true"))
(define pfalse (cons "boolean" "false"))
(define pnil (cons "pair" (cons "nil" "nil")))

(define pexit (cons "exit" '()))
(define pdot (cons "dot" '()))
(define pundef (cons "undefined" '()))
(define peof (cons "eof" '()))

(define (ptrue? value) (eq? ptrue value))
(define (pfalse? value) (eq? pfalse value))
(define (pnil? value) (eq? pnil value))

(define (pexit? value) (eq? pexit value))
(define (pdot? value) (eq? pdot value))
(define (pundef? value) (eq? pundef value))
(define (peof? value) (eq? peof value))

(define (new-char char) (if (eof-object? char) peof (cons "char" char)))
(define (new-regexp str) (cons "regexp" (regexp str)))

(define (length list)
  (define (length-iter list len)
    (if (null? list) len
      (length-iter (cdr list) (+ len 1))))
  (length-iter list 0))

(define (accumulate initial list func)
  (define (accumulate-iter sofar togo)
    (if (null? togo)
      sofar
      (accumulate-iter
        (func sofar (car togo))
        (cdr togo))))
  (accumulate-iter initial list))

(define (list->pairs list)
  (if (null? list) pnil
    (cons "pair"
      (cons
        (car list)
        (list->pairs (cdr list))))))

(define (pairs->list pairs)
  (if (pnil? pairs) '()
    (cons
      (cadr pairs)
      (pairs->list (cddr pairs))
    )))

;env.scm
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

;eval-expr.scm
(define (eval-expr env expr)
  (let ((type (car expr)))
    (cond
      ((equal? type "boolean") expr)
      ((equal? type "number") expr)
      ((equal? type "char") expr)
      ((equal? type "string") expr)
      ((equal? type "regexp") expr)
      ((equal? type "pair") expr)
      ((equal? type "mpair") expr)
      ((equal? type "name") (lookup env (cdr expr)))
      ((equal? type "list")
        (if (null? (cdr expr))
          (raise "missing procedure expression")
          (let ((result (eval-call env (cadr expr) (cddr expr))))
            (if (equal? (car result) "tail-expr")
              (eval-expr (cadr result) (cddr result))
              result))
          ))
      (else (raise "Illegal expression"))
      )))

(define (new-tail-expr env expr)
  (cons "tail-expr"
    (cons env expr)))

(define (eval-quote quote)
  (define (list->pairs lst)
    (if (null? lst) pnil
      (cons "pair"
        (cons
          (eval-quote (cons "quote" (car lst)))
          (list->pairs (cdr lst))))))
  (let ((quoted (cdr quote)))
    (let ((type (car quoted)))
      (cond
        ((equal? (car quoted) "list")
          (if (and (= (length (cdr quoted)) 2) (equal? (caadr quoted) "name") (equal? (cdadr quoted) "quote"))
            (cons "quote" (eval-quote (cons "quote" (caddr quoted))))
            (list->pairs (cdr quoted))))
        ((equal? (car quoted) "quote") (eval-quote (car quoted)))
        ((or
          (equal? (car quoted) "number")
          (equal? (car quoted) "char")
          (equal? (car quoted) "string")
          (equal? (car quoted) "regexp")
          (equal? (car quoted) "pair")
          (equal? (car quoted) "name"))
          quoted)
        (else quote)
      ))))

(define (eval-actuals env actuals)
  (if (null? actuals) '()
    (cons
      (eval-expr env (car actuals))
      (eval-actuals env (cdr actuals)))))

(define (eval-call env name actuals)
  (let ((operator (eval-expr env name)))
    (let ((type (car operator)))
      (cond
        ((equal? type "special-form") ((cdr operator) env actuals))
        ((equal? type "built-in") ((cdr operator) (eval-actuals env actuals)))
        ((equal? type "lambda") (eval-lambda (cdr operator) (eval-actuals env actuals)))
        (else (raise "value given is not callable"))))))

;Return all but the last expression (to implement TCO)
(define (eval-iter env exprs)
  (cond
    ((null? (cdr exprs)) (new-tail-expr env (car exprs)))
    (else (eval-expr env (car exprs)) (eval-iter env (cdr exprs)))
  ))

(define (eval-lambda lambda-func actuals)
  (define (bind-formals parent-env formals actuals)
    (define (bind-iter env formals actuals)
      (if (null? formals) env
        (bind-iter
          (set-binding env (cdar formals) (car actuals))
          (cdr formals)
          (cdr actuals))))
    (cond
      ((and (> (length formals) 0) (pdot? (car formals)))
        (set-binding
          (new-env parent-env)
          (cdadr formals)
          (list->pairs actuals)))
      ((= (length formals) (length actuals))
        (bind-iter (new-env parent-env) formals actuals))
      (else (raise "Wrong number of arguments passed to the given lambda"))
    ))
  (let (
    (parent-env (car lambda-func))
    (formals (cadr lambda-func))
    (exprs (caddr lambda-func)))
    (eval-iter (bind-formals parent-env formals actuals) exprs)))

(define (expr->printable-string expr)
  (expr->string
    (if (or (equal? (car expr) "name") (equal? (car expr) "pair") (equal? (car expr) "quote"))
      (cons "quote" expr)
      expr
    )))

(define (expr->string expr)
  (let ((type (car expr)) (value (cdr expr)))
    (cond
      ((ptrue? expr) "#t")
      ((pfalse? expr) "#f")
      ((peof? expr) "#<eof>")
      ((equal? type "number") (number->string value))
      ((equal? type "char")
        (string-append "#\\"
          (cond
            ((equal? value #\space) "space")
            ((equal? value #\tab) "tab")
            ((equal? value #\newline) "newline")
            (else (string value)))))
      ((equal? type "string") (string-append "\"" value "\""))
      ((equal? type "pair") (string-append "(" (pair->string expr) ")"))
      ((equal? type "mpair") (string-append "(" (mpair->string expr) ")"))
      ((equal? type "quote") (string-append "'" (expr->string value)))
      ((equal? type "special-form") "<special-form>")
      ((equal? type "built-in") "<built-in>")
      ((equal? type "lambda") "<lambda>")
      (else (cdr expr)))))

(define (pair->string pair)
  (if (pnil? pair) ""
    (let ((first (cadr pair)) (second (cddr pair)))
      (string-append
        (expr->string first)
        (if (equal? (car second) "pair")
          (if (pnil? second) "" (string-append " " (pair->string second)))
          (string-append " . " (expr->string second)))))))

(define (mpair->string mpair)
  (let ((first (mcar (cdr mpair))) (second (mcdr (cdr mpair))))
    (string-append
      "mcons "
      (expr->string first)
      " "
      (expr->string second))))

(define (mpair->disp-string mpair)
  (let ((first (mcar (cdr mpair))) (second (mcdr (cdr mpair))))
    (string-append
      (expr->string first)
      (cond
        ((equal? (car second) "mpair") (string-append " " (mpair->disp-string second)))
        ((pnil? second) "")
        (else (string-append " . " (expr->disp-string second)))
      ))))

(define (expr->disp-string expr)
  (cond
    ((equal? (car expr) "pair")
      (string-append "(" (pair->string expr) ")"))
    ((equal? (car expr) "mpair")
      (string-append "{" (mpair->disp-string expr) "}"))
    ((or (equal? (car expr) "char") (equal? (car expr) "string")) (cdr expr))
    (else (expr->string expr))
  ))

;special-forms.scm
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

(add-special-form "longest"
  (lambda (env actuals)
    (define (lst-length lst len)
      (cond
        ((not (equal? (car lst) "pair")) (raise "Evaluated expression is not a list"))
        ((pnil? lst) len)
        (else (lst-length (cddr lst) (+ len 1)))))
    (define (test-iter rest-pairs max-len expr)
      (if (null? rest-pairs) (new-tail-expr env expr)
        (let (
          (first-len (lst-length (eval-expr env (cadar rest-pairs)) 0))
          (second (caddar rest-pairs)))
          (if (> first-len max-len)
            (test-iter (cdr rest-pairs) first-len second)
            (test-iter (cdr rest-pairs) max-len expr)))))
    (if
      (validate-list actuals
        (lambda (length) (> length 0))
        (lambda (index element)
          (and
            (equal? (car element) "list")
            (= (length (cdr element)) 2))))
      (test-iter actuals -1 pundef)
      (raise "Bad syntax for special-form \"longest\""))))

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

;built-in.scm
(add-built-in "exit"
  (lambda (params)
    (if (= (length params) 0)
      pexit
      (raise "Built-in function \"exit\" takes no argument"))))

(add-built-in "void" (lambda (params) pundef))

(add-built-in "cons"
  (lambda (params)
    (if
      (validate-list params
        (lambda (length) (= length 2))
        (lambda (index element) (not (equal? (car element) "special-form"))))
      (cons "pair" (cons (car params) (cadr params)))
      (raise "Built-in funcion \"cons\" takes 2 arguments (non special-form)."))))

(add-built-in "car"
  (lambda (params)
    (if
      (validate-list params
        (lambda (length) (= length 1))
        (lambda (index element) (and (equal? (car element) "pair") (not (pnil? element)))))
      (cadar params)
      (raise "Built-in function \"car\" takes 1 argument (pair)."))))

(add-built-in "cdr"
  (lambda (params)
    (if
      (validate-list params
        (lambda (length) (= length 1))
        (lambda (index element) (and (equal? (car element) "pair") (not (pnil? element)))))
      (cddar params)
      (raise "Built-in function \"cdr\" takes 1 argument (pair)."))))

(add-built-in "mcons"
  (lambda (params)
    (if
      (validate-list params
        (lambda (length) (= length 2))
        (lambda (index element) (not (equal? (car element) "special-form"))))
      (cons "mpair" (mcons (car params) (cadr params)))
      (raise "Built-in funcion \"mcons\" takes 2 arguments (non special-form)."))))

(add-built-in "mcar"
  (lambda (params)
    (if
      (validate-list params
        (lambda (length) (= length 1))
        (lambda (index element) (and (equal? (car element) "mpair") (not (pnil? element)))))
      (mcar (cdar params))
      (raise "Built-in function \"car\" takes 1 argument (pair)."))))

(add-built-in "mcdr"
  (lambda (params)
    (if
      (validate-list params
        (lambda (length) (= length 1))
        (lambda (index element) (and (equal? (car element) "mpair") (not (pnil? element)))))
      (mcdr (cdar params))
      (raise "Built-in function \"cdr\" takes 1 argument (pair)."))))

(add-built-in "set-mcar!"
  (lambda (params)
    (if
      (validate-list params
        (lambda (length) (= length 2))
        (lambda (index element)
          (if (= index 0) (equal? (car element) "mpair") #t)))
      (let ((mpair (car params)) (new-value (cadr params)))
        (set-mcar! (cdr mpair) new-value))
      (raise "Built-in function \"set-mcar!\" takes 2 arguments."))
    pundef))

(add-built-in "set-mcdr!"
  (lambda (params)
    (if
      (validate-list params
        (lambda (length) (= length 2))
        (lambda (index element)
          (if (= index 0) (equal? (car element) "mpair") #t)))
      (let ((mpair (car params)) (new-value (cadr params)))
        (set-mcdr! (cdr mpair) new-value))
      (raise "Built-in function \"set-mcar!\" takes 2 arguments."))
    pundef))

(add-built-in "apply"
  (lambda (params)
    (if
      (validate-list params
        (lambda (length) (= length 2))
        (lambda (index element)
          (cond
            ((= index 0) (or (equal? (car element) "lambda") (equal? (car element) "built-in")))
            ((= index 1) (or (equal? (car element) "pair") (pnil? element)))
          )))
      (if (equal? (caar params) "built-in")
        ((cdar params) (pairs->list (cadr params)))
        (eval-lambda (cdar params) (pairs->list (cadr params))))
      (raise "Built-in function \"apply\" takes 2 argument."))))

(add-built-in "raise"
  (lambda (params)
    (if
      (validate-list params
        (lambda (length) (= length 1))
        (lambda (index element) #t))
      (raise (car params))
      (raise "Built-in function \"raise\" takes no argument."))
    pundef))

;built-in-arithmetic.scm
(define (number-pred? index element) (equal? (car element) "number"))

(add-built-in "+"
  (lambda (params)
    (cons "number"
      (accumulate 0 params
        (lambda (sofar num)
          (if (equal? (car num) "number")
            (+ sofar (cdr num))
            (raise "unexpected argument type")))))))

(add-built-in "-"
  (lambda (params)
    (if (or (< (length params) 1) (not (equal? (caar params) "number")))
      (raise "expecting at least 1 number")
      (cons "number"
        (if (= (length params) 1)
          (- (cdar params))
          (accumulate (cdar params) (cdr params)
            (lambda (sofar num)
              (if (equal? (car num) "number")
                (- sofar (cdr num))
                (raise "unexpected argument type")))))))))

(add-built-in "*"
  (lambda (params)
    (cons "number"
      (accumulate 1 params
        (lambda (sofar num)
          (if (equal? (car num) "number")
            (* sofar (cdr num))
            (raise "unexpected argument type")))))))

(add-built-in "/"
  (lambda (params)
    (if (or (< (length params) 1) (not (equal? (caar params) "number")))
      (raise "expecting at least 1 number")
      (cons "number"
        (if (= (length params) 1)
          (/ (cdar params))
          (accumulate (cdar params) (cdr params)
            (lambda (sofar num)
              (if (equal? (car num) "number")
                (/ sofar (cdr num))
                (raise "unexpected argument type")))))))))

(add-built-in "modulo"
  (lambda (params)
    (if
      (validate-list params
        (lambda (length) (= length 2))
        number-pred?)
      (cons "number"
        (modulo (cdar params) (cdadr params)))
      (raise "Built-in function \"modulo\" takes 2 numbers"))))

(add-built-in "floor"
  (lambda (params)
    (if
      (validate-list params
        (lambda (length) (= length 1))
        number-pred?)
      (cons "number"
        (floor (cdar params)))
      (raise "Built-in function \"floor\" takes 1 number"))))

(define (pred-iter pred? element lst)
  (cond
    ((null? lst) ptrue)
    ((pred? element (cdar lst)) (pred-iter pred? element (cdr lst)))
    (else pfalse)))

(add-built-in "="
  (lambda (params)
    (if (> (length params) 1)
      (let ((first (cdar params)) (rest (cdr params)))
        (pred-iter = first rest))
      (raise "Built-in function \"=\" takes at least 2 numbers"))))

(add-built-in "<"
  (lambda (params)
    (if (> (length params) 1)
      (let ((first (cdar params)) (rest (cdr params)))
        (pred-iter < first rest))
      (raise "Built-in function \"<\" takes at least 2 numbers"))))

(add-built-in ">"
  (lambda (params)
    (if (> (length params) 1)
      (let ((first (cdar params)) (rest (cdr params)))
        (pred-iter > first rest))
      (raise "Built-in function \">\" takes at least 2 numbers"))))

(add-built-in "eq?"
  (lambda (params)
    (if
      (validate-list params
        (lambda (length) (= length 2))
        (lambda (index element) #t))
      (if (eq? (cdar params) (cdadr params)) ptrue pfalse)
      (raise "Built-in function \">\" takes 2 arguments"))))

(add-built-in "equal?"
  (lambda (params)
    (if
      (validate-list params
        (lambda (length) (= length 2))
        (lambda (index element) #t))
      (if (equal? (car params) (cadr params)) ptrue pfalse)
      (raise "Built-in function \">\" takes 2 arguments"))))

;built-in-pred.scm
(add-built-in "null?"
  (lambda (params)
    (if (= (length params) 1)
      (if (pnil? (car params)) ptrue pfalse)
      (raise "Built-in function \"null?\" takes 1 argument."))))

(add-built-in "pair?"
  (lambda (params)
    (if (= (length params) 1)
      (if 
        (and 
          (equal? (caar params) "pair")
          (not (pnil? (car params))))
        ptrue pfalse)
      (raise "Built-in function \"pair?\" takes 1 argument."))))

(add-built-in "false?"
  (lambda (params)
    (if (= (length params) 1)
      (if (pfalse? (car params)) ptrue pfalse)
      (raise "Built-in function \"false?\" takes 1 argument"))))

(add-built-in "number?"
  (lambda (params)
    (if (= (length params) 1)
      (if (equal? (caar params) "number") ptrue pfalse)
      (raise "Built-in function \"number?\" takes 1 argument"))))

(add-built-in "real?"
  (lambda (params)
    (if (= (length params) 1)
      (if
        (and
          (equal? (caar params) "number")
          (real? (cdar params)))
        ptrue pfalse)
      (raise "Built-in function \"real?\" takes 1 argument"))))

(add-built-in "char?"
  (lambda (params)
    (if (= (length params) 1)
      (if (equal? (caar params) "char") ptrue pfalse)
      (raise "Built-in function \"char?\" takes 1 argument"))))

(add-built-in "string?"
  (lambda (params)
    (if (= (length params) 1)
      (if (equal? (caar params) "string") ptrue pfalse)
      (raise "Built-in function \"string?\" takes 1 argument"))))

(add-built-in "regexp?"
  (lambda (params)
    (if (= (length params) 1)
      (if (equal? (caar params) "regexp") ptrue pfalse)
      (raise "Built-in function \"regexp?\" takes 1 argument"))))

(add-built-in "eof-object?"
  (lambda (params)
    (if (= (length params) 1)
      (if (peof? (car params)) ptrue pfalse)
      (raise "Built-in function \"eof-object?\" takes 1 argument"))))

;built-in-string.scm
(add-built-in "regexp"
  (lambda (params)
    (if
      (validate-list params
        (lambda (length) (= length 1))
        (lambda (index element) (equal? (car element) "string")))
      (new-regexp (cdar params))
      (raise "Built-in function \"regexp\" takes 1 string"))))

(add-built-in "regexp-match"
  (lambda (params)
    (if
      (validate-list params
        (lambda (length) (= length 2))
        (lambda (index element)
          (if (equal? index 0)
            (equal? (car element) "regexp")
            (equal? (car element) "string"))))
      (let ((regex (car params)) (str (cadr params)))
        (if (regexp-match (cdr regex) (cdr str)) ptrue pfalse))
      (raise "Built-in function \"regexp-match\" takes 1 string"))))

(add-built-in "list->string"
  (lambda (params)
    (define (convert-pair pair)
      (cond
        ((pnil? pair) '())
        ((equal? (car pair) "pair")
          (let ((first (cadr pair)) (second (cddr pair)))
            (if (equal? (car first) "char")
              (cons (cdr first) (convert-pair second))
              (raise "Parameter passed in is not a list of chars"))))
        (else (raise "Parameter passed in is not a list of chars"))))
    (if (= (length params) 1)
      (cons "string" (list->string (convert-pair (car params))))
      (raise "Built-in function \"list->string\" expects a list of chars"))))

(add-built-in "string-length"
  (lambda (params)
    (if
      (validate-list params
        (lambda (length) (= length 1))
        (lambda (index element) (equal? (car element) "string")))
      (cons "number" (string-length (cdar params)))
      (raise "Built-in function \"string-length\" takes 1 string"))))

(add-built-in "string->number"
  (lambda (params)
    (if
      (validate-list params
        (lambda (length) (= length 1))
        (lambda (index element) (equal? (car element) "string")))
      (cons "number" (string->number (cdar params)))
      (raise "Built-in function \"string->number\" takes 1 string"))))

(add-built-in "string->list"
  (lambda (params)
    (define (convert-iter list)
      (if (null? list) pnil
        (cons "pair" (cons (new-char (car list)) (convert-iter (cdr list))))))
    (if
      (validate-list params
        (lambda (length) (= length 1))
        (lambda (index element) (equal? (car element) "string")))
      (convert-iter (string->list (cdar params)))
      (raise "Built-in function \"string->list\" takes 1 string"))))

(add-built-in "number->string"
  (lambda (params)
    (if
      (validate-list params
        (lambda (length) (= length 1))
        (lambda (index element) (equal? (car element) "number")))
      (cons "string" (number->string (cdar params)))
      (raise "Built-in function \"number->string\" takes 1 string"))))

(add-built-in "string-append"
  (lambda (params)
    (define (append-iter list)
      (if (null? list) ""
        (string-append (cdar list) (append-iter (cdr list)))))
    (if
      (validate-list params
        (lambda (length) #t)
        (lambda (index element) (equal? (car element) "string")))
      (cons "string" (append-iter params))
      (raise "Built-in function \"string-append\" takes 0 or more string(s)"))))

;built-in-io.scm
(add-built-in "display"
  (lambda (params)
    (if
      (validate-list params
        (lambda (length) (= length 1))
        (lambda (index element) #t))
      (display (expr->disp-string (car params)))
      (raise "Built-in function \"display\" takes 1 argument."))
    pundef))

(add-built-in "displayln"
  (lambda (params)
    (if
      (validate-list params
        (lambda (length) (= length 1))
        (lambda (index element) #t))
      (displayln (expr->disp-string (car params)))
      (raise "Built-in function \"display\" takes 1 argument."))
    pundef))

(add-built-in "newline"
  (lambda (params)
    (if
      (validate-list params
        (lambda (length) (= length 0))
        (lambda (index element) #t))
      (newline)
      (raise "Built-in function \"display\" takes no argument."))
    pundef))

(add-built-in "load"
  (lambda (params)
    (define (load-iter port env)
      (let ((expr (read-expr port)))
        (if (peof? expr) 1
          (begin
            (eval-expr env expr)
            (load-iter port env)
          ))))
    (if
      (validate-list params
        (lambda (length) (= length 1))
        (lambda (index element) (equal? (car element) "string")))
      (let* (
        (file (car params))
        (fin (open-input-file (cdr file))))
        (load-iter fin global-env)
        (close-input-port fin))
      (raise "Built-in function \"load\" takes 1 argument."))
    pundef))

(add-built-in "read"
  (lambda (params)
    (eval-quote (cons "quote"
      (cond
        ((= (length params) 0) (read-expr (current-input-port)))
        ((= (length params) 1)
          (if (equal? (caar params) "input-port") (read-expr (cdar params))
            (raise "The only argument for \"read-char\" must be an input port.")))
        (else (raise "Built-in function \"read-char\" takes 0 or 1 argument.")))))))

(add-built-in "read-char"
  (lambda (params)
    (cond
      ((= (length params) 0) (new-char (read-char)))
      ((= (length params) 1)
        (if (equal? (caar params) "input-port") (new-char (read-char (cdar params)))
          (raise "The only argument for \"read-char\" must be an input port.")))
      (else (raise "Built-in function \"read-char\" takes 0 or 1 argument.")))))

(add-built-in "peek-char"
  (lambda (params)
    (cond
      ((= (length params) 0) (new-char (peek-char)))
      ((= (length params) 1)
        (if (equal? (caar params) "input-port") (new-char (peek-char (cdar params)))
          (raise "The only argument for \"peek-char\" must be an input port.")))
      (else (raise "Built-in function \"peek-char\" takes 0 or 1 argument.")))))

(add-built-in "read-line"
  (lambda (params)
    (cond
      ((= (length params) 0) (cons "string" (read-line)))
      ((= (length params) 1)
        (if (equal? (caar params) "input-port")
          (if (eof-object? (peek-char (cdar params)))
            peof
            (cons "string" (read-line (cdar params))))
          (raise "The only argument for \"read-line\" must be an input port.")))
      (else (raise "Built-in function \"read-line\" takes 0 or 1 argument.")))))

(add-built-in "flush-output"
  (lambda (params)
    (if
      (validate-list params
        (lambda (length) (= length 0))
        (lambda (index element) #t))
      (flush-output)
      (raise "Built-in function \"flush-output\" takes no argument."))
    pundef))

(add-built-in "current-input-port"
  (lambda (params)
    (if
      (validate-list params
        (lambda (length) (= length 0))
        (lambda (index element) #t))
      (cons "input-port" (current-input-port))
      (raise "Built-in function \"current-input-port\" takes no argument."))))

(add-built-in "open-input-file"
  (lambda (params)
    (if
      (validate-list params
        (lambda (length) (= length 1))
        (lambda (index element) (equal? (car element) "string")))
      (cons "input-port" (open-input-file (cdar params)))
      (raise "Built-in function \"open-input-file\" takes no argument."))))

(add-built-in "close-input-port"
  (lambda (params)
    (if
      (validate-list params
        (lambda (length) (= length 1))
        (lambda (index element) (equal? (car element) "input-port")))
      (cons "input-port" (close-input-port (cdar params)))
      (raise "Built-in function \"close-input-port\" takes no argument."))))

;read-expr.scm
(define (read-expr port)

(define (whitespace? char)
  (or
    (equal? char #\space)
    (equal? char #\tab)
    (equal? char #\newline)))

(define (parenthesis? char)
  (or
    (equal? char #\()
    (equal? char #\))))

(define (bracket? char)
  (or
    (equal? char #\[)
    (equal? char #\])))

(define (read-quote) (list "list" (cons "name" "quote") (read-expr port)))

(define (read-string-iter)
  (let ((char (read-char port)))
    (cond
      ((equal? char #\") '())
      ((equal? char #\\) (cons (read-char port) (read-string-iter)))
      (else (cons char (read-string-iter)))
  )))

(define (read-string) (cons "string" (list->string (read-string-iter))))

(define (read-hash)
  (let ((next1 (read-char port)))
    (cond
      ((equal? next1 #\t) ptrue)
      ((equal? next1 #\f) pfalse)
      ((equal? next1 #\\)
        (let ((peek (peek-char port)))
          (if (or (parenthesis? peek) (bracket? peek) (equal? peek #\;))
            (new-char (read-char port))
            (parse-char (list->string (read-term-iter))))))
      ((and (equal? next1 #\r) (equal? (peek-char port) #\x))
        (read-char port)
        (if (equal? (peek-char port) #\")
          (begin (read-char port) (new-regexp (list->string (read-string-iter))))
          (raise "Unrecognized input: #rx")))
      (else (raise (string-append "Unrecognized input: #" (string next1))))
    )))

(define (parse-char str)
  (new-char
    (cond
      ((equal? str "space") #\space)
      ((equal? str "tab") #\tab)
      ((equal? str "newline") #\newline)
      ((= (string-length str) 1) (car (string->list str)))
      (else (raise (string-append "Unrecognized input: #\\" str)))
    )))

(define (read-term-iter)
  (let ((peek (peek-char port)))
    (cond
      ((or (whitespace? peek) (parenthesis? peek) (bracket? peek)) '())
      ((equal? peek #\;) (read-line port) '())
      (else (cons (read-char port) (read-term-iter)))
    )))

(define (parse-term term)
  (cond
    ((regexp-match #rx"^[-+]?([0-9]*[.])?[0-9]+$" term) (cons "number" (string->number term)))
    ((regexp-match #rx"^\\.$" term) pdot)
    ((regexp-match #rx"^[^#,].*$" term) (cons "name" term))
    (else (raise (string-append "Unrecognized input: " term)))
  ))

(define (read-term) (parse-term (list->string (read-term-iter))))

(define (read-list)
  (let ((peek (peek-char port)))
    (cond
      ((whitespace? peek) (read-char port) (read-list))
      ((equal? peek #\)) (read-char port) '())
      (else (cons (read-expr port) (read-list)))
    )))

(define (read-bracket)
  (let ((peek (peek-char port)))
    (cond
      ((whitespace? peek) (read-char port) (read-bracket))
      ((equal? peek #\]) (read-char port) '())
      (else (cons (read-expr port) (read-bracket)))
    )))

(let ((peek (peek-char port)))
  (cond
    ((eof-object? peek) peof)
    ((whitespace? peek) (read-char port) (read-expr port))
    ((equal? peek #\;) (read-line port) (read-expr port))
    ((equal? peek #\') (read-char port) (read-quote))
    ((equal? peek #\") (read-char port) (read-string))
    ((equal? peek #\#) (read-char port) (read-hash))
    ((equal? peek #\() (read-char port) (cons "list" (read-list)))
    ((equal? peek #\[) (read-char port) (cons "bracket" (read-bracket)))
    ((equal? peek #\)) (read-char port) (raise "Unexpected right parenthesis"))
    ((equal? peek #\]) (read-char port) (raise "Unexpected right bracket"))
    (else (read-term))
  ))
)

;repl.scm
(define level (if (and (> (length args) 0) (number? (car args))) (car args) 1))

(define (print-cursor level)
  (if (= level 0)
    (begin (display "> ") (flush-output))
    (begin (display "-") (print-cursor (- level 1)))))

(define stdin-port (current-input-port))

(define (repl-loop)
  (print-cursor level)
  (let (
    (result
      (with-handlers
        (
          (string? (lambda (msg) (display "error: ") (displayln msg) pundef))
          ((lambda (e) (not (exn:fail? e))) (lambda (expr) (display "uncaught exception: ") (displayln (expr->string expr)) pundef))
        )
        (eval-expr global-env (read-expr stdin-port)))))
    (cond
      ((pundef? result) (repl-loop))
      ((pexit? result) (void))
      (else
        (displayln (expr->printable-string result))
        (repl-loop)
    ))))

(repl-loop)
)
