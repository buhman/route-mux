(import (srfi 14)
        comparse
        matchable)

(define char-set:rfc3968
  (char-set-union char-set:letter
                  char-set:digit
                  (char-set #\- #\. #\_ #\~)))

;; combinators

(define (as-symbol parser)
  (bind (as-string parser)
        (o result string->symbol)))

(define (as-type parser s)
  (bind parser
        (lambda (val)
          (result (cons s val)))))

;; helpers

(define (uncons vs)
  (let loop ((v vs))
    (match v
      (() '())
      (((a . b) . rest)
       (cons a (cons b (loop rest)))))))

;; parsers

(define path-segment
  (one-or-more (in char-set:rfc3968)))

(define route-segment
  (as-type (as-string path-segment) 'segment))

(define route-param
  (preceded-by (is #\:)
               (as-type (as-symbol path-segment) 'param)))

(define route-separator
  (as-type (as-string (is #\/)) 'separator))

(define param-or-path
  (sequence* ((segment (any-of route-param
                               route-segment))
              (separator (maybe route-separator)))
    (result
     (cons segment separator))))

(define route-uri
  (sequence* ((sep route-separator)
              (segments (zero-or-more param-or-path))
              (_ end-of-input))
    (result (cons sep (uncons segments)))))
