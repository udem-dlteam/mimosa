;; The mimosa project


(define (test-ide lba cont)
 (begin
  (ide-setup)
  (let* ((ctrl (vector-ref IDE-CTRL-VECT 0))
        (devices (ide-controller-devices ctrl))
        (device (car devices)))
   (ide-read-sectors device lba 1 cont))))
