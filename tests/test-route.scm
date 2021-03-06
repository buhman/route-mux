(import (route-mux route)
        comparse)

;; tests

(test-group "route grammar"
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
      (segment . "a"))
    (parse route-uri "/a"))

  (test "invalid whitespace"
    #f
    (parse route-uri "/a b c"))

  (test "invalid symbols"
    #f
    (parse route-uri "/gook!"))

  (test "invalid leading separator"
    #f
    (parse route-uri "garbage"))

  (test "invalid parameter"
    #f
    (parse route-uri "/a:b/"))

  (test "invalid trailing separator"
    #f
    (parse route-uri "/a//"))

  (test "invalid multiple separator"
    #f
    (parse route-uri "/a///b/"))

  (test "invalid double root"
    #f
    (parse route-uri "//")))
