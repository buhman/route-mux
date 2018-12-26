;;(import radix)
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

(define +root+
  (make-node "r" 'non-leaf
             (list
              (make-node "om" 'non-leaf
                         (list
                          (make-node "ulus" 3 '())
                          (make-node "an" 'non-leaf
                                     (list
                                      (make-node "e" 1 '())
                                      (make-node "us" 2 '())))))
              (make-node "ub" 'non-leaf
                         (list
                          (make-node "e" 'non-leaf
                                     (list
                                      (make-node "ns" 4 '())
                                      (make-node "r" 5 '())))
                          (make-node "ic" 'non-leaf
                                     (list
                                      (make-node "on" 6 '())
                                      (make-node "undus" 7 '()))))))))

(define +blog-root+
  (make-node "/" #t
             (list
              (make-node "s" #f
                         (list
                          (make-node "earch/" #t '())
                          (make-node "upport/" #t '())))
              (make-node "blog/" #t
                         (list
                          (make-node 'article #f
                                     (list
                                      (make-node "/" #t '())))))
              (make-node "about-us/" #t
                         (list
                          (make-node "team/" #f '())))
              (make-node "contact/" #t '()))))

;; tests

(test-group "node-search"
  (test-parameterize "sanity"
    (romane romanus romulus rubens ruber rubicon rubicundus)
    (lambda (s)
      (alist-ref s +names+))
    (lambda (s)
      (node-value (node-search +root+ (symbol->string s)))))

  (test-parameterize "non-leaf"
    (r rom roman rub rube rubic)
    (lambda (_) 'non-leaf)
    (lambda (s)
      (node-value (node-search +root+ (symbol->string s)))))

  (test-values "params"
    (list (make-node "/" #t '()) '((article . "foo")))
    (node-search +blog-root+ "/blog/foo/")))
