(include "path.scm")

(define +route+
  '((separator . "/")
    (segment . "a") (segment . "b") (separator . "/")
    (param . f) (separator . "/")
    (segment . "q") (segment . "r") (separator . "/")
    (param . g) (separator . "/")
    (segment . "m") (segment . "n") (separator . "/")))

(test-group "route-path"
  (test "route-path order"
    '("/ab/" f "/qr/" g "/mn/")
    (route->path +route+)))

(define +p-root+
  (compact-root->tree
   '(#f
     ("/" #f
      ("foo/" #f
       (bar #f
        ("/" 1)))
      ("spam/" #f
       (eggs 2
        ("/" #f
         ("bacon/" 4)
         ("ham/" 3
          (sausage 5)))))))))

(test-group "path-search"
  (test-values "one parameter"
    (list (make-node 1 #()) #t '((bar . "1234")))
    (path-search +p-root+ "/foo/1234/"))

  (test-values "two parameter"
    (list (make-node 5 #()) #t '((sausage . "5678")
                                 (eggs . "1234")))
    (path-search +p-root+ "/spam/1234/ham/5678"))

  (test-values "no match"
    (list +p-root+ #f '())
    (path-search +p-root+ "bogus"))

  (test-values "partial match"
    (list (make-node 1 #()) #f '((bar . "1234")))
    (path-search +p-root+ "/foo/1234/bogus")))

;; insert tests

#;
(define (test-path-insert))

#;
(test-group "path-insert!"
  ())
