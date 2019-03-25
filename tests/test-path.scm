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
