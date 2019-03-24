(include "radix.scm")

;; test data

(define +names+
  '((romane . 1)
    (romanus . 2)
    (romulus . 3)
    (rubens . 4)
    (ruber . 5)
    (rubicon . 6)
    (rubicundus . 7)))

(define +names+
  '((romane . 1)
    (romanus . 2)
    (romulus . 3)
    (rubens . 4)
    (ruber . 5)
    (rubicon . 6)
    (rubicundus . 7)))

(define +root+
  (make-node #f
    (list
     (make-edge "r"
       (make-node #f
         (list
          (make-edge "om"
            (make-node #f
              (list
               (make-edge "an"
                 (make-node #f
                   (list
                    (make-edge "e"
                      (make-node 1 '()))
                    (make-edge "us"
                      (make-node 2 '())))))
               (make-edge "ulus"
                 (make-node 3 '())))))
          (make-edge "ub"
            (make-node #f
              (list
               (make-edge "e"
                 (make-node #f
                   (list
                    (make-edge "ns"
                      (make-node 4 '()))
                    (make-edge "r"
                      (make-node 5 '())))))
               (make-edge "ic"
                 (make-node #f
                   (list
                    (make-edge "on"
                      (make-node 6 '()))
                    (make-edge "undus"
                      (make-node 7 '()))))))))))))))

(define +one-edge+
  (make-node 1
    (list
     (make-edge "test"
       (make-node 2 '())))))

;; tests

(test-group "node-search"
  (test-values "edge-match complete"
    (list (make-node 2 '()) "1234" 0 #f)
    (node-search +one-edge+ "test1234"))

  (test-values "edge-match incomplete"
    (list +one-edge+ "oast" 2 (make-edge "test" (make-node 2 '())))
    (node-search +one-edge+ "teoast"))

  (test-values "edge-match none"
    (list +one-edge+ "foo" 0 #f)
    (node-search +one-edge+ "foo"))

  (test-parameterize "node-search edge recursion"
    (romane romanus romulus rubens ruber rubicon rubicundus)
    (lambda (s) (alist-ref s +names+))
    (lambda (s) (node-value (node-search +root+ (symbol->string s))))))
