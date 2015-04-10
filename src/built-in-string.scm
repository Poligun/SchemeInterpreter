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


