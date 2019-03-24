(import (chicken format)
        (srfi 13)
        matchable)

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

;; api

(define (node-search node suffix)
  (if (= 0 (string-length suffix))
    ;; empty suffix; no edges could possibly match
    (values node suffix 0 #f)
    ;; binary search edges
    (let ((edges (node-edges node)))
      (let loop ((start 0)
                 (end (vector-length edges)))
        (let ((index (quotient (+ start end) 2)))
          (if (= start end)
            ;; none of the edges matched
            (values node suffix 0 #f)
            ;;
            (let ((edge (vector-ref edges index)))
              (call/cc
               (lambda (return)
                 (edge-match return suffix edge node)
                 (if (string< suffix (edge-label edge))
                   (loop start index)
                   (loop index end)))))))))))

(define (edge-match return key edge parent-node)
  (let* ((label (edge-label edge))
         (prefix-length (string-prefix-length label key))
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
      #f))))

(define (node-insert! root key value)
  (receive (parent-node new-suffix prefix-length common-edge)
      (node-search root key)
    (cond
     (common-edge
      ;; this is a partial match with a common edge
      (let ((common-prefix (substring (edge-label common-edge) 0 prefix-length))
            (old-suffix (substring (edge-label common-edge) prefix-length))
            (old-node (edge-node common-edge)))
        (set! (edge-node common-edge)
          (make-node #f
            (list
             (make-edge new-suffix
               (make-node value '()))
             (make-edge old-suffix
               old-node))))
        (set! (edge-label common-edge) common-prefix)))
     ;; this parent-node is the same node as this key
     ((= 0 (string-length new-suffix))
      (set! (node-value parent-node) value))
     ;; this is a new edge for this parent-node
     (else
      (set! (node-edges parent-node)
        (cons
         (make-edge new-suffix
           (make-node value '()))
         (node-edges parent-node)))))))
