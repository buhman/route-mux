(import (chicken format)
        (srfi 13)
        matchable)

;; model

(define-record-type node
  (make-node key value children)
  node?
  ;; string
  (key node-key)
  ;; any
  (value node-value)
  ;; '(node ...)
  (children node-children (setter node-children)))

(define-record-printer (node n out)
  (fprintf out "#,(node ~s ~s ~s)" (node-key n) (node-value n) (node-children n)))

;; api

(define (prefix-match? node path index)
  (let ((key (node-key node)))
    (cond
     ((string? key)
      (let ((end (+ (string-length key) index))
            (match? (string-prefix? key path 0 (string-length key) index)))
        (values match? end #f)))
     ;; a parameter; must be followed by a / or EOS
     ((symbol? key)
      (let* ((end (string-index path #\/ index))
             (end (or end (string-length path)))
             (param (substring/shared path index end))
             (match? (not (= index end))))
        (values match? end (cons key param))))
     (else
      (error "invalid radix key" key)))))

(define-syntax values-or
  (syntax-rules ()
    ((_ a b)
     (let-values (((a? a1) a)
                  ((b? b1) b))
       (if a?
         (values a? a1)
         (values b? b1))))))

(define (node-search root path)
  (let go ((index 0)
           (node root)
           (params '()))
    (let-values (((match? end param) (prefix-match? node path index)))
      (let ((params (if param (cons param params) params)))
        (cond
         ((not match?)
          (values #f params))
         ((= end (string-length path))
          (values node params))
         (else
          (let loop ((children (node-children node)))
            (match children
              (() (values #f params))
              ((child . rest)
               (values-or (go end child params)
                          (loop rest)))))))))))
