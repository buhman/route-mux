(import (srfi 1)
        (srfi 13)
        matchable)

;; aggregate parsed routes to a path
(define (route->path-r route)
  (fold
   (lambda (box path)
     (let ((value (match box
                    (`(separator . ,value) value)
                    (`(segment . ,value) value)
                    (`(param . ,value) value))))
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
