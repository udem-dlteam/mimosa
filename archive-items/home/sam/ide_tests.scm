;; The mimosa project

(define (cut)
 (open-input-file "/cut"))

(define (test-ide lba)
  (begin
    (cut)
    (ide-setup)
    (let* ((ctrl (vector-ref IDE-CTRL-VECT 0))
           (devices (ide-controller-devices ctrl))
           (device (car devices)))
      (ide-read-sectors device lba 1))))

(define (t-ide-read lba)
  (let* ((ctrl (vector-ref IDE-CTRL-VECT 0))
         (devices (ide-controller-devices ctrl))
         (device (car devices)))
    (ide-read-sectors device lba 1)))

(define (dsk1)
  (let* ((ctrl (vector-ref IDE-CTRL-VECT 0))
         (devices (ide-controller-devices ctrl))
         (device (car devices)))
    device))

; (define (ide-write-sectors device lba buffer count)
(define (ide-write-test)
  (let ((vect (make-vector 512 #x0))
        (msg "This is the test"))
    (for-each (lambda (i)
        (vector-set! vect i (char->integer (string-ref msg i)))) (iota (string-length msg)))
    (ide-write-sectors (dsk1) 0 vect 1))) 
