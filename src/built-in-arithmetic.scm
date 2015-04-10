(define (number-pred? index element) (equal? (car element) "number"))

;refers +
(add-built-in "+"
  (lambda (params)
    (cons "number"
      (accumulate 0 params
        (lambda (sofar num)
          (if (equal? (car num) "number")
            (+ sofar (cdr num))
            (raise "unexpected argument type")))))))

;refers -
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

;refers *
(add-built-in "*"
  (lambda (params)
    (cons "number"
      (accumulate 1 params
        (lambda (sofar num)
          (if (equal? (car num) "number")
            (* sofar (cdr num))
            (raise "unexpected argument type")))))))

;refers /
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

;refers modulo
(add-built-in "modulo"
  (lambda (params)
    (if
      (validate-list params
        (lambda (length) (= length 2))
        number-pred?)
      (cons "number"
        (modulo (cdar params) (cdadr params)))
      (raise "Built-in function \"modulo\" takes 2 numbers"))))

;refers floor
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
