;; aggregate parsed routes to a path
(define (route->path-r route)
  (fold
   (lambda (box path)
     (let ((value (cdr box)))
       (if (string? value)
         (match path
           (() (list value))
           ((last . rest)
            (if (string? last)
              (cons (string-append last value) rest)
              (cons value (cons last rest)))))
         (cons value path))))
   '()
   route))

(define route->path (o reverse route->path-r))

(define (parse-param s #!optional (delim #\/))
  (let ((index (string-index s delim)))
    (if index
      (values (string-copy s 0 index) (substring/shared s index))
      ;; no terminator; this is the parameter
      (values s #f))))

(define (path-search node s)
  (let loop ((node node) (s s) (params '()))
    (receive (node suffix prefix-length edge) (node-search node s)
      (cond
       ((and edge (symbol? (edge-label edge)))
        ;; consume parameter
        (receive (param rest) (parse-param suffix)
          (let ((new-params (cons (cons (edge-label edge) param) params))
                (new-node (edge-node edge)))
            (if rest
              ;; continue searching, starting from the next node
              (loop new-node rest new-params)
              ;; s is a parameter that is not delimiter-terminated
              (values new-node #t new-params)))))
       (else
        (values node (= 0 (string-length suffix)) params))))))

(define (path-insert! node path value)
  (match path
    (()
     ;; verify we aren't shadowing an already-set value
     (assert (eq? (node-value node) #f)
             "path-insert!: insertion would shadow existing value" node)
     (set! (node-value node) value))
    ((p . ps)
     (cond
      ((symbol? p)
       (let ((edges (node-edges node)))
         (match edges
           (#()
            ;; make a new parameter edge
            (let ((new-node (make-node #f #())))
              (set! (node-edges node)
                (vector (make-edge p new-node)))
              ;; continue to next path component
              (path-insert! new-node ps value)))
           (#(edge)
            (assert (and (symbol? (edge-label edge)) (eq? (edge-label edge) p))
                    "path-insert!: parameter symbol collision at" node)
            ;; continue to next path component
            (path-insert! (edge-node edge) ps value)))))
      ((string? p)
       (receive (new-node suffix prefix-length edge) (node-search node p)
         (assert (not (and edge (symbol? (edge-label edge))))
                 "path-insert!: parameter string collision at" edge)
         (cond
          ((not (= 0 (string-length suffix)))
           ;; partial match for this string part, insert it
           (node-insert-at! new-node suffix prefix-length edge #f)
           (path-insert! node path value))
          (else
           ;; continue to next path component
           (path-insert! new-node ps value)))))))))
