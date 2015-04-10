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
