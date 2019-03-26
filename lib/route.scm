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

;; parsers

(define segment
  (one-or-more (in char-set:rfc3968)))

(define route-segment-string
  (as-type (as-string segment) 'segment))

(define route-segment-param
  (preceded-by (is #\:)
               (as-type (as-symbol segment) 'param)))

(define route-separator
  (as-type (as-string (is #\/)) 'separator))

(define param-or-string
  (any-of route-segment-param route-segment-string))

(define route-segments
  (recursive-parser
   (sequence* ((ps param-or-string)
               (next (any-of (sequence* ((sep route-separator)
                                         (rest route-segments))
                               (result (cons sep rest)))
                             (result '()))))
     (result (cons ps next)))))

(define (listify s)
  (if s (list s) '()))

(define route-uri
  (sequence* ((sep route-separator)
              (segments (maybe route-segments))
              (end (maybe route-separator))
              (_ end-of-input))
    (cond
     ((and (not segments) end)
      (result #f))
     (segments
      (result (cons sep (append segments (listify end)))))
     (else
      (result (list sep))))))
