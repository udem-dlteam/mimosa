;; Mimosa
;; Université de Montréal, Marc Feeley, Samuel Yvon
(define-library
  (pci)
  (import
    (errors)
    (gambit)
    (utils)
    (low-level)
    (debug))
  (export
    offset->register
    read-conf
    device-at?
    list-devices)
  (begin
    (define CLASS-MASS-STORAGE #x01)
    (define SUBCLASS-IDE-CONTROLLER #x01)
    (define CONFIG-ADDR #xCF8)
    (define CONFIG-DATA #xCFC)
    (define BUS-COUNT 256)
    (define DEV-PER-BUS 32)
    (define FUNC-PER-DEV 8)
    (define HEADER-INFO-OFFSET 8)

    ;; Convert an offset to a register number
    (define (offset->register offset)
      (>> offset 2))

    ;; Convert a register number to an offset
    (define (register->offset register)
      (<< register 2))

    ;; Make a value to place in the config address register
    (define (make-configuration-address rewrite-data? bus device function offset)
      (bitwise-ior
        (arithmetic-shift (if rewrite-data? 1 0) 31)
        (arithmetic-shift (bitwise-and #xFF bus) 16)
        (arithmetic-shift (bitwise-and #xFF device) 11)
        (arithmetic-shift (bitwise-and #xFF function) 8)
        (bitwise-and #xFF offset)
        ))

    ;; Read the pci config of a device at
    ;; the given bus, device and function.
    ;; The last parameter is the offset
    ;; to read from.
    (define (read-conf bus device function offset)
      (let ((address (make-configuration-address #t bus device function offset)))
        (outl address CONFIG-ADDR)
        (inl CONFIG-DATA)))


    ;; Verifies if a device is present at the given bus, device
    ;; and function
    (define (device-at? bus device function)
      (let ((vendor (read-conf bus device function #x00)))
        (not (= #xFFFFFFFF vendor))))

    ;; Make a list the devices that satisfy the predicate.
    ;; The predicate takes in argument the class code and subclass code
    ;; of the device.
    ;; The result is given as a list of list, where each sublist
    ;; is of the form '(bus device function)
    ;; It starts at the given bus device and function, and takes
    ;; a currently found list
    (define (list-device-at bus device function pred found)
      ; (debug-write "---")
      ; (debug-write bus)
      ; (debug-write device)
      ; (debug-write function)
      (let* ((current ;; at the current address
               (if (device-at? bus device function)
                   (let* ((info-line (read-conf bus device function HEADER-INFO-OFFSET))
                          (class (bitwise-and
                                   #xFF
                                   (arithmetic-shift info-line (- 24))))
                          (subclass (bitwise-and
                                      #xFF
                                      (arithmetic-shift info-line (- 16)))))
                     (if (pred class subclass)
                         (list bus device function)
                         'NOTHING))
                   'NOTHING))
             (found (if (not (eq? 'NOTHING current))
                        (cons current found)
                        found)))
        (cond ((fx< function (-- FUNC-PER-DEV))
               (list-device-at bus device (++ function) pred found))
              ((fx< device (-- DEV-PER-BUS))
               (list-device-at bus (++ device) 0 pred found))
              ((fx< bus (-- BUS-COUNT))
               (list-device-at (++ bus) 0 0 pred found))
              (else found))))

    ;; Make a list the devices that satisfy the predicate.
    ;; The predicate takes in argument the class code and subclass code
    ;; of the device.
    ;; The result is given as a list of list, where each sublist
    ;; is of the form '(bus device function)
    (define (list-devices pred)
     (list-device-at #x0000 #x0000 #x0000 pred '()))

    ))
