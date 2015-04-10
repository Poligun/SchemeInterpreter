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

