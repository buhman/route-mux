(module (route-mux radix)
  (
   ;; constructors
   make-node
   make-edge
   ;; selectors
   node-value
   node-edges
   edge-label
   edge-node
   ;; predicates
   node?
   edge?
   ;; searching
   node-search
   ;; mutators
   node-insert!
   ;; internals
   sorted-insert
   sorted-edge-insert)

  (import (chicken base)
          (scheme)
          (only (chicken format) fprintf)
          (only (srfi 13) string< string-prefix-length substring/shared)
          (only matchable match)
          (only vector-lib vector-unfold))

  ;; model

  (define-record-type node
    (make-node value edges)
    node?
    ;; any
    (value node-value (setter node-value))
    ;; #(edge ...)
    (edges node-edges (setter node-edges)))

  (define-record-printer (node n out)
    (fprintf out "#,(node ~s ~s)" (node-value n) (node-edges n)))

  (define-record-type edge
    (make-edge label node)
    edge?
    ;; string
    (label edge-label (setter edge-label))
    ;; node
    (node edge-node (setter edge-node)))

  (define-record-printer (edge e out)
    (fprintf out "#,(edge ~s ~s)" (edge-label e) (edge-node e)))

  (define (edge-match return edge key parent-node)
    (let* ((label (edge-label edge)))
      (if (not (string? label))
        ;; this is a parameter edge; let the caller figure out what it wants to
        ;; do with this
        (return parent-node key 0 edge)
        ;;
        (let* ((prefix-length (string-prefix-length label key))
               (suffix (substring/shared key prefix-length)))
          (cond
           ((= prefix-length (string-length label))
            ;; complete match; continue to next node
            (call-with-values (lambda () (node-search (edge-node edge) suffix))
              return))
           ((< 0 prefix-length)
            ;; incomplete match; no better matches are possible
            (return parent-node suffix prefix-length edge))
           (else
            ;; no match; there might be a better match
            #f))))))

  (define (sorted-insert vec item cmp<)
    (vector-unfold
     (lambda (i found?)
       (cond
        (found?
         (values (vector-ref vec (- i 1)) #t))
        ((and (not (= (vector-length vec) i))
              (cmp< (vector-ref vec i) item))
         (values (vector-ref vec i) #f))
        (else
         (values item #t))))
     (+ (vector-length vec) 1)
     #f))

  (define (edge< a b)
    (string< (edge-label a) (edge-label b)))

  (define (sorted-edge-insert vec item)
    (sorted-insert vec item edge<))

  ;; api

  (define (node-search node suffix)
    (if (= 0 (string-length suffix))
      ;; empty suffix; no edges could possibly match
      (values node suffix 0 #f)
      ;; binary search edges
      (let ((edges (node-edges node)))
        (let loop ((last #f)
                   (start 0)
                   (end (vector-length edges)))
          (let ((index (quotient (+ start end) 2)))
            (if (or (= start end)
                    (and last (= last index)))
              ;; none of the edges matched
              (values node suffix 0 #f)
              ;;
              (let ((edge (vector-ref edges index)))
                (call/cc
                 (lambda (return)
                   (edge-match return edge suffix node)
                   (if (string< suffix (edge-label edge))
                     (loop index start index)
                     (loop index index end)))))))))))

  (define (node-insert! root key value)
    (receive (parent-node new-suffix prefix-length common-edge) (node-search root key)
      (cond
       (common-edge
        ;; this is a partial match with a common edge
        (let ((common-prefix (substring (edge-label common-edge) 0 prefix-length))
              (old-suffix (substring (edge-label common-edge) prefix-length))
              (old-node (edge-node common-edge)))
          (set! (edge-node common-edge)
            (let ((edges (sorted-edge-insert (vector (make-edge old-suffix old-node))
                                             (make-edge new-suffix (make-node value #())))))
              (make-node #f edges)))
          (set! (edge-label common-edge) common-prefix)))
       ;; this parent-node is the same node as this key
       ((= 0 (string-length new-suffix))
        (set! (node-value parent-node) value))
       ;; this is a new edge for this parent-node
       (else
        (set! (node-edges parent-node)
          (sorted-edge-insert
           (node-edges parent-node)
           (make-edge new-suffix (make-node value #())))))))))
