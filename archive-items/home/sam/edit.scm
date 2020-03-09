;;;============================================================================

;;; File: "edit.scm"

;;;============================================================================

;;; Minimal terminal-based text editor with some keybindings from emacs.
;;;
;;; > (edit "foo.scm")   ;; will start editing the file foo.scm
;;;
;;; Use the escape key to end the editing (this will save the file foo.scm).
;;; Move the cursor with the arrow keys or ctrl-b/f/p/n/a/e.
;;; Delete characters and lines with backspace, delete, ctrl-d or ctrl-k.
;;; You can only edit the first 79 characters of each line...

;;;============================================================================

(define (make-sstring len)
  (cons len (make-string (sstring-buf-len len))))

(define (string->sstring str)
  (cons (string-length str) str))

(define (sstring-buf-len len)
  (fxmax 8 (fxquotient (fx* 3 len) 2)))

(define (sstring-grow! sstr len)
  (let* ((buf (sstring-buf sstr))
         (buf-len (string-length buf)))
    (if (fx< buf-len len)
        (let* ((new-buf-len (sstring-buf-len len))
               (new-buf (make-string new-buf-len))
               (n (fxmin (sstring-length sstr) len)))
          (substring-move! buf 0 n new-buf 0)
          (set-cdr! sstr new-buf)))
    (set-car! sstr len)
    buf))

(define (sstring-length sstr)
  (car sstr))

(define (sstring-buf sstr)
  (cdr sstr))

(define (sstring-ref sstr i)
  (string-ref (sstring-buf sstr) i))

(define (sstring->string sstr)
  (substring (sstring-buf sstr) 0 (sstring-length sstr)))

(define (sstring-replace! sstr1 start1 end1 str2 start2 end2)
  (let* ((len (sstring-length sstr1))
         (len1 (fx- end1 start1))
         (len2 (fx- end2 start2))
         (change-len (fx- len2 len1))
         (new-len (fx+ len change-len))
         (buf (sstring-grow! sstr1 new-len))
         (new-buf (sstring-buf sstr1)))
    (substring-move! buf 0 start1 new-buf 0)
    (substring-move! buf end1 len new-buf (fx+ end1 change-len))
    (substring-move! str2 start2 end2 new-buf start1)))

(define (make-svector len)
  (cons len (make-vector (svector-buf-len len))))

(define (svector-buf-len len)
  (fxmax 8 (fxquotient (fx* 3 len) 2)))

(define (svector-grow! svect len)
  (let* ((buf (svector-buf svect))
         (buf-len (vector-length buf)))
    (if (fx< buf-len len)
        (let* ((new-buf-len (svector-buf-len len))
               (new-buf (make-vector new-buf-len))
               (n (fxmin (svector-length svect) len)))
          (subvector-move! buf 0 n new-buf 0)
          (set-cdr! svect new-buf)))
    (set-car! svect len)
    buf))

(define (svector-length svect)
  (car svect))

(define (svector-buf svect)
  (cdr svect))

(define (svector-ref svect i)
  (vector-ref (svector-buf svect) i))

(define (svector->vector svect)
  (subvector (svector-buf svect) 0 (svector-length svect)))

(define (svector-replace! svect1 start1 end1 vect2 start2 end2)
  (let* ((len (svector-length svect1))
         (len1 (fx- end1 start1))
         (len2 (fx- end2 start2))
         (change-len (fx- len2 len1))
         (new-len (fx+ len change-len))
         (buf (svector-grow! svect1 new-len))
         (new-buf (svector-buf svect1)))
    (subvector-move! buf 0 start1 new-buf 0)
    (subvector-move! buf end1 len new-buf (fx+ end1 change-len))
    (subvector-move! vect2 start2 end2 new-buf start1)))

(define (file-read path)
  (let* ((content
          (with-exception-catcher
           (lambda (e)
             "")
           (lambda ()
             (call-with-input-file
                 path
               (lambda (port)
                 (read-line port #f))))))
         (lines
          (list->vector
           (map (lambda (line)
                  (let* ((len (string-length line))
                         (sstr (make-sstring len)))
                    (sstring-replace! sstr 0 len line 0 len)
                    sstr))
                (reverse (##reverse-string-split-at content #\newline)))))
         (len
          (vector-length lines))
         (svect
          (make-svector len)))
    (svector-replace! svect 0 len lines 0 len)
    svect))

(define (file-write path svect)
  (directory-files) ;; this seems to fix a bug in the filesystem IO!
  (call-with-output-file
      path
    (lambda (port)
      (display (append-strings
                (vector->list
                 (vector-map sstring->string (svector->vector svect)))
                "\n")
               port))))

;;;----------------------------------------------------------------------------

(define (term-home ed)      (print port: (editor-out ed) "\033[H"))
(define (term-clear-eos ed) (print port: (editor-out ed) "\033[J"))
(define (term-clear-eol ed) (print port: (editor-out ed) "\033[K"))
(define (term-up1 ed)       (print port: (editor-out ed) "\033[A"))
(define (term-down1 ed)     (print port: (editor-out ed) "\033[B"))
(define (term-up ed n)      (print port: (editor-out ed) "\033[" n "A"))
(define (term-down ed n)    (print port: (editor-out ed) "\033[" n "B"))
(define (term-right ed n)   (print port: (editor-out ed) "\033[" n "C"))
(define (term-left ed n)    (print port: (editor-out ed) "\033[" n "D"))
(define (term-move ed y x)  (print port: (editor-out ed) "\033[" y ";" x "H"))
(define (term-cr ed)        (print port: (editor-out ed) "\r"))
(define (term-crlf ed)      (print port: (editor-out ed) "\r\n"))

(define (term-substring ed str start end)
  (write-substring str start end (editor-out ed)))

(define key-ESCAPE -1)
(define key-BACK   -2)
(define key-UP     -3)
(define key-DOWN   -4)
(define key-RIGHT  -5)
(define key-LEFT   -6)
(define key-HOME   -7)
(define key-DELETE -8)
(define key-END    -9)
(define key-F1     -10)
(define key-F2     -11)
(define key-F3     -12)
(define key-F4     -13)
(define key-RETURN -14)

(define-type editor
  id: F2091856-A496-4FDE-B1F5-7237FC093502
  in          ;; input port
  out         ;; output port
  rows        ;; nb of rows
  cols        ;; nb of columns
  input-state ;; key decoding state
  key-buf     ;; key buffer
  path        ;; path to file
  buffer      ;; edited content of file
  x           ;; cursor x
  y           ;; cursor y
  top)        ;; row of top line on screen

(define (raw-mode ed)
  (tty-mode-set! (editor-in ed)
                 #f ;; input-allow-special
                 #f ;; input-echo
                 #t ;; input-raw
                 #t ;; output-raw
                 0)) ;; speed

(define (cooked-mode ed)
  (tty-mode-set! (editor-in ed)
                 #t ;; input-allow-special
                 #t ;; input-echo
                 #f ;; input-raw
                 #f ;; output-raw
                 0)) ;; speed

(define (process-char ed c)
  (case (editor-input-state ed)
    ((0)
     ;; idle state
     (cond ((char=? c #\x08) ;; backspace
            (process-special-key ed key-BACK))
           ((char=? c #\x7f) ;; delete
            (process-special-key ed key-DELETE))
           ((char=? c #\x0d) ;; return
            (process-special-key ed key-RETURN))
           ((char=? c #\x1b) ;; escape
            (editor-input-state-set! ed 1)
            #t)
           ((char<? c #\x20) ;; special?
            (process-special-key ed (char->integer c)))
           (else
            (process-plain-key ed c))))
    ((1)
     ;; after escape
     (cond ((char=? c #\O)
            (editor-input-state-set! ed 2))
           ((char=? c #\[)
            (editor-input-state-set! ed 3))
           (else
            (editor-input-state-set! ed 0)))
     #t)
    ((2)
     ;; after escape O
     (editor-input-state-set! ed 0)
     (cond ((char=? c #\P)
            (process-special-key ed key-F1))
           ((char=? c #\Q)
            (process-special-key ed key-F2))
           ((char=? c #\R)
            (process-special-key ed key-F3))
           ((char=? c #\S)
            (process-special-key ed key-F4))
           (else
            #t)))
    (else ;; (3)
     ;; after escape [
     (editor-input-state-set! ed 0)
     (cond ((char=? c #\A)
            (process-special-key ed key-UP))
           ((char=? c #\B)
            (process-special-key ed key-DOWN))
           ((char=? c #\C)
            (process-special-key ed key-RIGHT))
           ((char=? c #\D)
            (process-special-key ed key-LEFT))
           ((char=? c #\H)
            (process-special-key ed key-HOME))
           ((char=? c #\F)
            (process-special-key ed key-END))
           (else
            #t)))))

(define (process-plain-key ed key)
  (let ((buf (editor-key-buf ed)))
    (string-set! buf 0 key)
    (process-plain-keys ed buf 0 1)))

(define (process-plain-keys ed keys start end)
  (op-insert-chars ed keys start end))

(define (process-special-key ed key)
  (cond ((fx= key key-RETURN)
         (op-insert-line-break ed))
        ((fx= key key-BACK)
         (op-delete-backward ed))
        ((fx= key key-DELETE)
         (op-delete-backward ed))
        ((fx= key key-ESCAPE)
         (editor-exit ed))
        ((fx= key key-UP)
         (op-line-forward ed -1))
        ((fx= key key-DOWN)
         (op-line-forward ed 1))
        ((fx= key key-LEFT)
         (op-char-forward ed -1))
        ((fx= key key-RIGHT)
         (op-char-forward ed 1))
        ((fx= key #x08) ;; backspace
         (op-delete-backward ed))
        ((fx= key #x7f) ;; delete
         (op-delete-forward ed))
        ((fx= key #x01) ;; ctrl-a
         (op-bol ed))
        ((fx= key #x02) ;; ctrl-b
         (op-char-forward ed -1))
        ((fx= key #x04) ;; ctrl-d
         (op-delete-forward ed))
        ((fx= key #x05) ;; ctrl-e
         (op-eol ed))
        ((fx= key #x06) ;; ctrl-f
         (op-char-forward ed 1))
        ((fx= key #x0b) ;; ctrl-k
         (op-kill-line ed))
        ((fx= key #x0e) ;; ctrl-n
         (op-line-forward ed 1))
        ((fx= key #x10) ;; ctrl-p
         (op-line-forward ed -1))
        (else
         #t)))

(define (op-bol ed)
  (editor-move-cursor ed 0 (editor-y ed))
  #t)

(define (op-eol ed)
  (let* ((buffer (editor-buffer ed))
         (len (svector-length buffer)))
    (editor-move-cursor
     ed
     (if (fx< (editor-y ed) len)
         (sstring-length (svector-ref buffer (editor-y ed)))
         0)
     (editor-y ed))
    #t))

(define (op-line-forward ed n)
  (editor-move-cursor ed (editor-x ed) (fx+ (editor-y ed) n))
  #t)

(define (op-char-forward ed n)
  (editor-move-cursor ed (fx+ (editor-x ed) n) (editor-y ed))
  #t)

(define (op-delete-backward ed)
  (op-delete ed (fx- (editor-x ed) 1)))

(define (op-delete-forward ed)
  (op-delete ed (editor-x ed)))

(define (op-kill-line ed)
  (let* ((buffer (editor-buffer ed))
         (x (editor-x ed))
         (y (editor-y ed))
         (sstr (svector-ref buffer y))
         (len (sstring-length sstr)))
    (if (fx= x len)
        (op-delete-forward ed)
        (begin
          (sstring-replace! sstr x len "" 0 0)
          (term-clear-eol ed)))))

(define (op-delete ed x-pos)
  (let ((y (editor-y ed)))
    (if (fx< x-pos 0)
        (if (fx> y 0)
            (merge-line-with-next ed (fx- y 1)))
        (let* ((buffer (editor-buffer ed))
               (sstr (svector-ref buffer y)))
          (if (fx= x-pos (sstring-length sstr))
              (merge-line-with-next ed y)
              (begin
                (sstring-replace! sstr x-pos (fx+ x-pos 1) "" 0 0)
                (editor-move-cursor ed x-pos y)
                (write-line-from ed sstr x-pos)
                (term-clear-eol ed)
                (editor-move-cursor ed x-pos y)))))
    #t))

(define (merge-line-with-next ed y-pos)
  (let* ((buffer (editor-buffer ed))
         (len (svector-length buffer)))
    (if (fx< y-pos (fx- len 1))
        (let* ((sstr1 (svector-ref buffer y-pos))
               (sstr2 (svector-ref buffer (fx+ y-pos 1)))
               (str2 (sstring->string sstr2))
               (x-pos (sstring-length sstr1)))
          (sstring-replace! sstr1 x-pos x-pos str2 0 (string-length str2))
          (svector-replace! buffer (fx+ y-pos 1) (fx+ y-pos 2) '#() 0 0)
          (editor-move-cursor ed x-pos y-pos)
          (write-line-from ed sstr1 x-pos)
          (editor-redraw ed (fx- (fx+ y-pos 1) (editor-top ed)))
          (term-clear-eos ed)
          (editor-move-cursor ed x-pos y-pos)))))

(define (op-insert-chars ed str start end)
  (or (fx>= start end)
      (let* ((buffer (editor-buffer ed))
             (x (editor-x ed))
             (y (editor-y ed))
             (sstr (svector-ref buffer y))
             (added (fx- end start)))
        (sstring-replace! sstr x x str start end)
        (write-line-from ed sstr x)
        (editor-move-cursor ed
                            (fxmin (fx+ x added)
                                   (fx- (editor-cols ed) 1))
                            y)
        #t)))

(define (write-line-from ed sstr pos)
  (let ((end (fxmin (sstring-length sstr) (fx- (editor-cols ed) 1))))
    (write-substring (sstring-buf sstr) pos end (editor-out ed))
    (editor-x-set! ed end)))

(define (op-insert-line-break ed)
  (let* ((buffer (editor-buffer ed))
         (x (editor-x ed))
         (y (editor-y ed))
         (sstr (svector-ref buffer y))
         (len (sstring-length sstr))
         (new-line (string->sstring (substring (sstring-buf sstr) x len))))
    (sstring-replace! sstr x len "" 0 0)
    (svector-replace! buffer (fx+ y 1) (fx+ y 1) (vector new-line) 0 1)
    (term-clear-eos ed)
    (if (fx<= y (fx+ (editor-top ed) (editor-rows ed)))
        (editor-redraw ed (fx- (fx+ y 1) (editor-top ed))))
    (editor-move-cursor ed 0 (fx+ y 1))))

(define (editor-event-loop ed)
  (let ((in (editor-in ed))
        (key-buf (editor-key-buf ed)))
    (let read-next-chunk ()
      (let process-chunk ((n
                           (read-substring key-buf
                                           0
                                           (string-length key-buf)
                                           in
                                           1))) ;; return as soon as >= 1 char
        (let* ((n-1
                (fx- n 1))
               (end
                (if (char=? (string-ref key-buf n-1) #\x1b) n-1 n)))
          (let loop1 ((i 0))
            (if (fx< i end)
                (if (fx= (editor-input-state ed) 0)
                    (let loop2 ((i i) (j i))
                      (if (fx< i end)
                          (let ((c (string-ref key-buf i)))
                            (if (and (char>=? c #\space)
                                     (not (char=? c #\x7f)))
                                (loop2 (fx+ i 1) j)
                                (and (process-plain-keys ed key-buf j i)
                                     (process-char ed c)
                                     (loop1 (fx+ i 1)))))
                          (and (process-plain-keys ed key-buf j i)
                               (loop1 i))))
                    (and (process-char ed (string-ref key-buf i))
                         (loop1 (fx+ i 1))))
                (if (fx= n end)
                    (read-next-chunk)
                    ;; there was a lone escape
                    (and (process-special-key ed key-ESCAPE)
                         (read-next-chunk))))))))))

(define (editor-redraw ed start-row)
  (let* ((buffer
          (editor-buffer ed))
         (stop-row
          (fxmin (editor-rows ed)
                 (fx- (svector-length buffer)
                      (editor-top ed)))))
    (let loop ((i start-row))
      (if (fx< i stop-row)
          (let ((sstr (svector-ref buffer (fx+ i (editor-top ed)))))
            (if (fx= i 0)
                (begin
                  (term-home ed)
                  (term-clear-eos ed))
                (term-crlf ed))
            (term-substring ed
                            (sstring-buf sstr)
                            0
                            (fxmin (fx- (editor-cols ed) 1)
                                   (sstring-length sstr)))
            (loop (fx+ i 1)))))))

(define (editor-move-cursor ed new-x new-y)
  (let* ((buffer (editor-buffer ed))
         (len (svector-length buffer)))
    (cond ((fx< new-y 0)
           (set! new-y 0)
           (set! new-x 0))
          ((fx>= new-y len)
           (if (fx> len 0)
               (begin
                 (set! new-y (fx- len 1))
                 (set! new-x (fxmin
                              (fx- (editor-cols ed) 1)
                              (sstring-length (svector-ref buffer new-y)))))
               (begin
                 (set! new-y 0)
                 (set! new-x 0))))
          (else
           (set! new-x (fxmin (fxmax 0 new-x)
                              (fxmin
                               (fx- (editor-cols ed) 1)
                               (sstring-length (svector-ref buffer new-y)))))))
    (let* ((x (fxmin (editor-x ed) (fx- (editor-cols ed) 1)))
           (y (editor-y ed)))

      (define (redraw)
        (editor-top-set! ed new-y)
        (editor-redraw ed 0)
        (term-home ed)
        (if (fx> new-x 0)
            (term-right ed new-x)))

      (cond ((fx< new-y (editor-top ed))
             (redraw))

            ((fx< new-y (fx+ (editor-top ed) (editor-rows ed)))
             (term-move ed
                        (fx+ (fx- new-y (editor-top ed)) 1)
                        (fx+ new-x 1)))

            ((fx= new-y (fx+ (editor-top ed) (editor-rows ed)))
             (if (not (fx= y (fx- new-y 1)))
                 (term-move ed (editor-rows ed) 1))
             (editor-top-set! ed (fx- new-y (fx- (editor-rows ed) 1)))
             (editor-redraw ed (fx- (editor-rows ed) 1))
             (term-cr ed)
             (if (fx> new-x 0)
                 (term-right ed new-x)))

            (else
             (redraw)))

      (editor-x-set! ed new-x)
      (editor-y-set! ed new-y))))

(define (editor-exit ed)
  (term-home ed)
  (term-clear-eos ed)
  (cooked-mode ed)
  #f)

(define (editor-file-read ed path)
  (let ((svect (file-read path)))
    (editor-path-set! ed path)
    (editor-buffer-set! ed svect)))

(define (editor-file-write ed path)
  (file-write path (editor-buffer ed)))

(define (edit path #!optional (output-path path))
  (let ((ed
         (make-editor (current-input-port)
                      (current-output-port)
                      24
                      80
                      0
                      (make-string 4)
                      #f
                      #f
                      0
                      0
                      0)))
    (editor-file-read ed path)
    (raw-mode ed)
    (editor-redraw ed 0)
    (term-home ed)
    (editor-event-loop ed)
    (editor-file-write ed output-path)
    output-path))

;;;============================================================================
