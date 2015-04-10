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
