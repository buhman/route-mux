(include "route.scm")

;; tests

(test-group "grammar"
  (test "root"
    '((separator . "/"))
    (parse route-uri "/"))

  (test "segments"
    '((separator . "/")
      (segment . "a") (separator . "/")
      (segment . "b") (separator . "/")
      (segment . "c") (separator . "/"))
    (parse route-uri "/a/b/c/"))

  (test "params"
    '((separator . "/")
      (param . a) (separator . "/")
      (param . b) (separator . "/")
      (param . c) (separator . "/"))
    (parse route-uri "/:a/:b/:c/"))

  (test "mixed"
    '((separator . "/")
      (segment . "a") (separator . "/")
      (param . b) (separator . "/")
      (param . c) (separator . "/")
      (segment . "d") (separator . "/"))
    (parse route-uri "/a/:b/:c/d/"))

  (test "symbols"
    '((separator . "/")
      (segment . "sensi-ble") (separator . "/")
      (param . g~o-o_k.) (separator . "/"))
    (parse route-uri "/sensi-ble/:g~o-o_k./"))

  (test "trailing separator"
    '((separator . "/")
      (segment . "a")
      #f)
    (parse route-uri "/a"))

  (test "invalid whitespace"
    #f
    (parse route-uri "/a b c"))

  (test "invalid symbols"
    #f
    (parse route-uri "/gook!"))

  (test "invalid leading separator"
    #f
    (parse route-uri "garbage")))
