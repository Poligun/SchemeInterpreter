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
