(import test)

(define-syntax test-values
  (syntax-rules ()
    ((_ name expect (expr ...))
     (test name expect (call-with-values (lambda () (expr ...)) list)))))

(define-syntax test-parameterize
  (ir-macro-transformer
   (lambda (expr inject compare)
     (let ((name (cadr expr))
           (params (caddr expr))
           (expect (cadddr expr))
           (body (cddddr expr)))
       (cons
        'begin
        (let loop ((ps params))
          (if (eq? ps '())
            '()
            (cons
             (let* ((param (car ps))
                    (test-name (string-append name " " (symbol->string (strip-syntax param)))))
               `(test ,test-name (,expect (quote ,param)) (,@body (quote ,param))))
             (loop (cdr ps))))))))))

;; tests

#;
(test-group "radix"
  (include "tests/test-radix.scm"))

#;
(test-group "route"
  (include "tests/test-route.scm"))

(test-group "path"
  (include "tests/test-path.scm"))
