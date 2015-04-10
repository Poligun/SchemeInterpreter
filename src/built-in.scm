; special function
(add-built-in "exit"
  (lambda (params)
    (if (= (length params) 0)
      pexit
      (raise "Built-in function \"exit\" takes no argument"))))

; do nothing
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

(add-built-in "raise"
  (lambda (params)
    (if
      (validate-list params
        (lambda (length) (= length 1))
        (lambda (index element) #t))
      (raise (car params))
      (raise "Built-in function \"flush-output\" takes no argument."))
    pundef))
