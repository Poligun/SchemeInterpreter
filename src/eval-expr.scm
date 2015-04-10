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
