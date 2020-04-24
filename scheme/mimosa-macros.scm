;; Global macro file that can be included on demand
;; Somehow the export from utils does not work...
(define-macro (define-c-like-structure name . fields)
              (let* ((symbol-name (symbol->string name))
                     (pack-struct
                       (string->symbol (string-append "pack-" symbol-name)))
                     (unpack-struct
                       (string->symbol (string-append "unpack-" symbol-name)))
                     (make-struct
                       (string->symbol (string-append "make-" symbol-name)))
                     (width-struct
                       (string->symbol (string-append symbol-name "-width")))
                     (field-accessor (lambda (field)
                                       (string->symbol
                                         (string-append
                                           symbol-name
                                           "-"
                                           (symbol->string field)))))
                     (flatten (lambda (lst)
                                (if (pair? lst)
                                    (fold-right (lambda (e r)
                                                  (if (pair? e)
                                                      (append (flatten (car e)) (flatten (cdr e)) r)
                                                      (cons e r)))
                                                (list) lst)
                                    (list lst))))
                     (vect-idx 0)
                     (field-names (map car fields))
                     (field-width (map cadr fields)))
                `(begin
                   (define-structure ,name
                                     ,@field-names)
                   (define ,width-struct (+ ,@field-width))
                   (define (,unpack-struct strc vec base-offset)
                     ; fields that are shorter than 4 are packed into an int
                     ; fields that are longer than 4 are packed directly into a vect
                     ; fields that have a negative length are forced into a vect
                     (begin
                       ,@(let ((offset 0))
                           (map (lambda (f-index)
                                  (let ((f (list-ref field-names f-index))
                                        (w (list-ref field-width f-index)))
                                    ; copy the vector into the target vector
                                    (if (or (< w 0) (> w 4))
                                        `(begin ,@(map
                                                    (lambda (i)
                                                      (set! offset (+ offset 1))
                                                      `(vector-set!
                                                         vec
                                                         (+ ,(- offset 1) base-offset)
                                                         (vector-ref (,(field-accessor f) strc) ,i)))
                                                    (iota (abs w))))
                                        `(begin ,@(map (lambda (i)
                                                         (set! offset (+ offset 1))
                                                         `(vector-set!
                                                            vec
                                                            (+ ,(- offset 1) base-offset)
                                                            (bitwise-and #xFF (arithmetic-shift
                                                                                (,(field-accessor f) strc)
                                                                                ,(- 0 (* i 8)))))
                                                         )
                                                       (iota w)))
                                        ))) (iota (length field-names))))
                       vec))
                   (define (,pack-struct vec)
                     (,make-struct
                       ;; TODO: map does not guarantee left-to-right calls to function
                       ; fields that are shorter than 4 are packed into an int
                       ; fields that are longer than 4 are packed directly into a vect
                       ; fields that have a negative length are forced into a vect
                       ,@(map (lambda (extract)
                                (let ((offset vect-idx)
                                      (next-offset (+ vect-idx (abs extract))))
                                  (set! vect-idx next-offset)
                                  (if (<= 0 extract 4)
                                      `(+ ,@(map (lambda (i)
                                                   `(arithmetic-shift
                                                      (vector-ref vec ,(+ offset i))
                                                      ,(* i 8)))
                                                 (iota extract)))

                                          `(build-vector
                                             ,(abs extract)
                                             (lambda (i)
                                               (vector-ref vec (+ ,offset i)))))))
                                  field-width))))))


