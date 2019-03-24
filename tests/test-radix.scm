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

(define vec-map (compose list->vector map))

(define (compact-root->tree r)
  (match r
    ((value . edges)
     (make-node value
       (vec-map compact-edge->tree edges)))))

(define (compact-edge->tree e)
  (match e
    ((label value . edges)
     (make-edge label
       (make-node value
         (vec-map compact-edge->tree edges))))))

(define +root+
  (compact-root->tree
   '(#f
     ("r" #f
      ("om" #f
       ("an" #f
        ("aa" 99)
        ("e" 1)
        ("gg" 99)
        ("us" 2)
        ("zz" 99))
       ("ulus" 3))
      ("ub" #f
       ("e" #f
        ("aa" 99)
        ("ns" 4)
        ("oo" 99)
        ("pp" 99)
        ("qq" 99)
        ("r" 5)
        ("zz" 99))
       ("ic" #f
        ("on" 6)
        ("undus" 7)))))))

(define +root-basic+
  (compact-root->tree
   '(#f
     ("r" #f
      ("om" #f
       ("an" #f
        ("e" 1)
        ("us" 2))
       ("ulus" 3))
      ("ub" #f
       ("e" #f
        ("ns" 4)
        ("r" 5))
       ("ic" #f
        ("on" 6)
        ("undus" 7)))))))

(define +one-edge+
  (compact-root->tree
   '(1
     ("test" 2))))

;; tests

(test-group "node-search"
  (test-values "edge-match complete"
    (list (make-node 2 #()) "1234" 0 #f)
    (node-search +one-edge+ "test1234"))

  (test-values "edge-match incomplete"
    (list +one-edge+ "oast" 2 (make-edge "test" (make-node 2 #())))
    (node-search +one-edge+ "teoast"))

  (test-values "edge-match none"
    (list +one-edge+ "foo" 0 #f)
    (node-search +one-edge+ "foo"))

  (test-parameterize "node-search edge recursion"
    (romane romanus romulus rubens ruber rubicon rubicundus)
    (lambda (s) (alist-ref s +names+))
    (lambda (s) (node-value (node-search +root+ (symbol->string s))))))

;; insert tests

(define (test-node-insert alist)
  (let ((root (make-node #f #())))
    (for-each
     (lambda (name)
       (match name
         ((key . value)
          (node-insert! root (symbol->string key) value))))
     alist)
    root))

(test-group "node-insert"
  (test "sorted-insert start"
    #(1 2 4 5)
    (sorted-insert #(2 4 5) 1 <))

  (test "sorted-insert middle"
    #(2 3 4 5)
    (sorted-insert #(2 4 5) 3 <))

  (test "sorted-insert end"
    #(2 4 5 6)
    (sorted-insert #(2 4 5) 6 <))

  (test "sorted-edge-insert"
    (vector (make-edge "a" #f) (make-edge "b" #f) (make-edge "c" #f))
    (sorted-edge-insert
     (vector (make-edge "a" #f) (make-edge "c" #f))
     (make-edge "b" #f)))

  (test "node-insert"
    +root-basic+
    (test-node-insert +names+))

  (test "node-insert reverse"
    +root-basic+
    (test-node-insert (reverse +names+))))
