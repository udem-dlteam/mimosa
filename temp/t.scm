(define << arithmetic-shift)
(define FILE-MODE-READ (<< 1 0))
(define FILE-MODE-TRUNC (<< 1 1))
(define FILE-MODE-APPEND (<< 1 2))
(define FILE-MODE-PLUS (<< 1 3))

(define (mask a b)
 (fx> (fxand a b) 0))


; Parse the file opening mode
; r, r+ : read, read-write from start respectively
; w, w+ : write, read-write from start (truncation)
; a, a+ : write, read-write, from end (append)
; c is the continuation that will
(define (parse-modes mode)
  ; We want to take the smallest mode
  ; in case of conflict, but leave the + there
  (let ((ored (fold (lambda (c n)
                      (fxior n (cond ((eq? #\+ c) FILE-MODE-PLUS)
                                     ((eq? #\a c) FILE-MODE-APPEND)
                                     ((eq? #\w c) FILE-MODE-TRUNC)
                                     ((eq? #\r c) FILE-MODE-READ))))
                    0 (string->list mode))))
    (fxior
      (fxand FILE-MODE-PLUS ored)
      (cond ((mask ored FILE-MODE-READ)
             FILE-MODE-READ)
            ((mask ored FILE-MODE-TRUNC)
             FILE-MODE_TRUNC)
            ((mask ored FILE-MODE-APPEND)
             FILE-MODE-APPEND)))))
