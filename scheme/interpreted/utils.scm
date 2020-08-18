; Mimosa
; Université de Montréal
; Marc Feeley, Samuel Yvon
(define-library
  (utils)
  (import (gambit))
  (export
    ++
    --
    //
    <<
    >>
    ID
    O
    TIME-UNIT-MICROSECS
    TIME-UNIT-MS
    TIME-UNIT-NSECS
    TIME-UNIT-SECONDS
    TODO
    apply-on
    assock
    assocv
    b-chop
    bcd->binary
    bipartition
    both
    build-vector
    call-if
    any?
    contradiction
    day-month-year->days-since-epoch
    day-month-year->epoch-seconds
    displayn
    fields
    filter
    first-index
    flatten
    fxhalve
    gambit-set-repl-channels!
    hour-minute-seconds->seconds
    if-not
    ilog2
    lazy
    lwrap
    map-with-index
    mask
    o
    partial
    replace-error
    s<<
    s>>
    safe-substring
    split-string
    string->u8vector
    string-trim
    tautology
    u8vector->string
    infinite-loop
    uint16
    uint32
    uint8
    until-has-elapsed
    wint16
    wint32
    wint8
    zip
    )
  (begin
    (define tautology (lambda _ #t)) ;; always true pred

    (define contradiction (lambda _ #f)) ;; always false pred

    (define (lazy val) (lambda _ val))

    (define (// a b)
      (floor (/ a b)))

    (define (hour-minute-seconds->seconds h m s)
      (+ (* 3600 h) (* 60 m) s))

    ; This wonderful algorithm has been translated to Scheme,
    ; by me, from https://howardhinnant.github.io/date_algorithms.html
    (define (day-month-year->days-since-epoch d m y)
      (let* ((y (- y (if (<= m 2) m 0)))
             (era (// (- y 399) 400))
             (yoe (- y (* 400 era)))
             (doy (+ (// (+ 2 (* 153 (+ m (if (> m 2) -3 9 )))) 5) d -1))
             (doe (+ doy (- (// yoe 100))  (// yoe 4) (* 365 yoe))))
        (+ doe -719468 (* era 146097))))

    (define (day-month-year->epoch-seconds d m y)
      (* 24 3600 (day-month-year->days-since-epoch d m y)))

    (define (<< n shl)
      (fxarithmetic-shift n shl))

    (define (>> n shr)
      (fxarithmetic-shift-right n shr))

    (define (s<< n shl)
      (arithmetic-shift n shl))

    (define (s>> n shr)
      (arithmetic-shift n (- shr)))

    (define (TODO)
      (display "STUB")
      #t)

    (define (assock key tbl)
      (let ((v (assoc key tbl)))
        (if v (cdr v) v)))

    (define (assocv key tbl)
      (let ((v (assoc key tbl)))
        (if v (car v) v)))

    (define (assocv key tbl)
      (cdr (assoc key tbl)))

    ; Combine two functions (\circ)
    (define (o g f)
      (lambda params
        (g (apply f params))))

    ; Combine many functions (\circ with many parameters)
    (define (O fns)
      (lambda (n)
        (if (= (length fns) 0)
            n
            ((o (car fns) (O (cdr fns))) n))))

    (define (incn n)
      (lambda (k)
        (+ k n)))

    (define ++ (incn 1))

    (define -- (incn -1))

    (define CURRENT-THREAD-CHANNELS-ADDR 28)

    ; Set the channels on the thread's current repl
    ; it takes three channels (in, out, err)
    (define (gambit-set-repl-channels! chan1 chan2 chan3)
      (##vector-set! (current-thread) CURRENT-THREAD-CHANNELS-ADDR
       (##make-repl-channel-ports chan1 chan2 chan3)))

    (define (nanoseconds->time nsecs)
      (let ((seconds (* nsecs 1e-9)))
        (seconds->time seconds)))

    (define (microseconds->time usecs)
      (nanoseconds->time (* 1000 usecs)))

    (define (milliseconds->time msecs)
      (let ((seconds (* msecs 1e-3)))
        (seconds->time seconds)))

    ; Build a vector according to a procedure
    (define (build-vector sz proc)
      (list->vector (map proc (iota sz))))

    ; Remove spaces out of a list
    (define (list-trim msg)
      (if (= (length msg) 0)
          (list)
          (let ((f (car msg)))
            (if (eq? f #\space)
                (list-trim (cdr msg))
                (cons f (list-trim (cdr msg)))))))

    ; It's not trim, it's removing spaces
    (define (string-trim msg)
      (list->string (list-trim (string->list msg))))

    ; Why does this not work...
    (define-macro (lwrap expr)
                  `(lambda () ,expr))

    (define TIME-UNIT-SECONDS seconds->time)
    (define TIME-UNIT-NSECS nanoseconds->time)
    (define TIME-UNIT-MICROSECS microseconds->time)
    (define TIME-UNIT-MS milliseconds->time)

    (define (until-has-elapsed n unit)
      (let ((now (time->seconds (current-time)))
            (unit-secs (time->seconds (unit n))))
        (seconds->time (+ now unit-secs))))

    (define (mask v m)
      (fx> (fxand v m) 0))

    (define (b-chop v)
      (bitwise-and #xFF v))

    (define (fxhalve n)
      (fxarithmetic-shift-right n 1))

    (define (filter lst predicate)
      (fold-right (lambda (e r)
                    (if (predicate e)
                        (cons e r)
                        r))
                  (list) ; base
                  lst
                  ))

    (define (zip a b)
      (if (null? a)
          (list)
          (if (null? b)
              (cons (car a) (zip (cdr a) b))
              (let ((ea (car a))
                    (eb (car b))
                    (ra (cdr a))
                    (rb (cdr b)))
                (cons (list ea eb) (zip ra rb))
                ))))

    (define flatten
      (lambda (ipt)
        (if (null? ipt)
            '()
            (let ((c (car ipt)))
              (if (pair? c)
                  (flatten c)
                  (cons c (flatten (cdr ipt))))))))

    (define (ilog2aux n tot)
      (if (= n 1) tot (ilog2aux (floor (/ n 2)) (++ tot))))

    (define (ilog2 n)
      (ilog2aux n 0))

    (define (bcd->binary byte)
      (+ (* 10 (fxarithmetic-shift-right byte 4)) (fxand byte #x0F)))

    (define (fields obj)
      (let ((x (##type-all-fields (##structure-type obj))))
        (map (lambda (i) (list-ref x (* i 3))) (iota (quotient (length x) 3)))))


    (define-macro (extract-uint vec offset w)
                  (cons '+ (map (lambda (i)
                                  (list 'arithmetic-shift
                                        (list 'vector-ref 'vec (list '+ 'offset i))
                                        (* i 8))) (iota w))))

    (define-macro (write-uint vec offset val w)
                  `(begin
                     ,@(map
                         (lambda (i)
                           `(vector-set! ,vec (+ offset ,i) (fxand #xFF (arithmetic-shift val ,(- 0 (* i 8))))))
                         (iota w))))

    (define (uint32 vec offset)
      (extract-uint vec offset 4))

    (define (uint16 vec offset)
      (extract-uint vec offset 2))

    (define (uint8 vec offset)
      (extract-uint vec offset 1))

    (define (wint32 vec offset val)
      (write-uint vec offset val 4)
      vec)

    (define (wint16 vec offset val)
      (write-uint vec offset val 2)
      vec)

    (define (wint8 vec offset val)
      (write-uint vec offset val 1)
      vec)

    (define (both) a b (lambda (n)
                         (begin
                           (a n)
                           (b n))))

    (define (first-index-aux l comp e i default)
      (cond ((not (pair? l))
             default)
            ((comp e (car l))
             i)
            (else
              (first-index (cdr l) e (++ i) default))))

    (define (first-index l comp e default)
      (first-index-aux l comp e 0 default))

    (define (split-string separator str)
      (fold-right
        (lambda (c r)
          (if (eq? c separator)
              (cons "" r)
              (cons (string-append (string c) (car r)) (cdr r))
              ))
        (list "")
        (string->list str)))

    (define (ID i) i)

    (define (u8vector->string v)
      (vector->string (vector-map integer->char v)))

    (define (string->u8vector s)
      (vector-map (o (lambda (int) (bitwise-and #xFF int)) char->integer) (string->vector s)))

    ; Partition a list 'l' in two parts according to the truth of predicate 'p'
    ; and call the continuation 'c' with the two lists as the only two arguments,
    ; where the first list satisfies the predicate and the second does not.
    ; It preserves the order elements are found in the list
    (define (bipartition l p c)
      (let ((len (length l)))
        (if (= 0 len)
            (c '() '())
            (let ((e (car l)))
              (bipartition
                (cdr l)
                p
                (lambda (yes no)
                  (if (p e)
                      (c (cons e yes) no)
                      (c yes (cons e no)))))))))


    ;; If 'val is not the sym symbol as 'sym, apply
    ;; f to val, otherwise return it
    (define (if-not val sym f)
      (if (eq? val sym)
          sym
          (f val)))

    (define (partial f . args)
      (lambda other-args
        (apply f (append args other-args))))

    ;; If the argument evals to #t, call it
    (define (call-if function?)
      (if function?
          (function?)
          #f))

    ;; Create a lambda that will apply it's argument
    ;; on the arguments of apply-on
    (define (apply-on . args)
      (lambda (fn)
        (apply fn args)
        ))

    (define (safe-substring s start end)
      (let ((l (string-length s)))
        (if (>= start l)
            ""
            (substring s start (min l end)))))

    (define (map-with-index f l)
      (let ((len (length l)))
        (map
          (lambda (index)
            (let ((element (list-ref l index)))
              (f index element)))
          (iota len))))


    (define (any? l)
      ;; or is a macro :(
      (fold (lambda (e r) (or e r)) #f l))

    (define (infinite-loop) (infinite-loop))

    ))
