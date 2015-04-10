(define (repl . args)
  (define level
    (if (and (> (length args) 0) (number? (car args)))
      (car args)
      1))
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
        )))
    )
  (displayln "new interpreter started")
  (repl-loop)
)
