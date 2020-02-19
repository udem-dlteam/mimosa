;; The mimosa project

; (define LOG_FILE (open-output-file (list path: "mimosa.log" truncate: #f append: #t)))

(define LOG_WARN 'LOG_WARN)
(define LOG_INFO 'LOG_INFO)
(define LOG_VERBOSE 'LOG_VERBOSE)
(define LOG_ERROR 'LOG_ERROR)

(define (log-level-str level)
 (cond ((eq? LOG_WARN level)
        "[WARN]")
       ((eq? LOG_INFO level)
        "[INFO]")
       ((eq? LOG_VERBOSE level)
        "[VERBOSE]")
       ((eq? LOG_ERROR level)
        "[ERROR]")
       (else
        (log-level-str LOG_INFO))))

(define (mlog msg level)
 (msg))
  ; (let ((level-str (log-level-str level)))
  ;   (write-string (string-append level-str " " msg) LOG_FILE)
  ;   (flush-output-port LOG_FILE)
  ;   msg))

(define (assock key tbl)
 (car (assoc key tbl)))

(define (assocv key tbl)
  (cdr (assoc key tbl)))

(define (o g f)
 (lambda (n)
   (g (f n))))

(define (incn n)
  (lambda (k)
    (+ k n)))

(define ++ (incn 1))

(define CURRENT-THREAD-CHANNELS-ADDR 28)

; Set the channels on the thread's current repl
; it takes three channels (in, out, err)
(define (gambit-set-repl-channels! chan1 chan2 chan3)
  (##vector-set! (current-thread) CURRENT-THREAD-CHANNELS-ADDR 
   (##make-repl-channel-ports chan1 chan2 chan3)))
