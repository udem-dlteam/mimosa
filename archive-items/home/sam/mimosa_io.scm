; The mimosa project

(define STDIN (open-output-file "/sys/stdin"))

(define (write-char-stdin c)
 (begin
  (write-char c STDIN)
  (flush-output-port STDIN)
  c))
