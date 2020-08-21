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
    (define CLASS-NETWORK #x02)
    (define SUBCLASS-IDE-CONTROLLER #x01)
    (define SUBCLASS-ETHERNET #x00)
    (define CONFIG-ADDR #xCF8)
    (define CONFIG-DATA #xCFC)
    (define BUS-COUNT 256)
    (define DEV-PER-BUS 32)
    (define FUNC-PER-DEV 8)
    (define HEADER-HW-INFO-OFFSET 0)
    (define HEADER-INFO-OFFSET 8)
    (define HEADER-PCI-INFO-OFFSET #x0C)
    (define HEADER-0-INT-OFFSET #x3C)
    (define HEADER-0-BAR0 #x10)
    (define HEADER-0-BAR1 #x14)
    (define HEADER-0-BAR2 #x18)
    (define HEADER-0-BAR3 #x1C)
    (define HEADER-0-BAR4 #x20)
    (define HEADER-0-BAR5 #x24)
    (define BAR-LIST
      (list
        HEADER-0-BAR0
        HEADER-0-BAR1
        HEADER-0-BAR2
        HEADER-0-BAR3
        HEADER-0-BAR4
        HEADER-0-BAR5))


    (define-type pci-device
     bus
     device
     function
     class
     subclass
     )

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

    ;; Make a list the devices detected on the PCI buses
    ;; The result is given as a list of list, where each sublist
    ;; is of the form '(bus device function)
    ;; It starts at the given bus device and function, and takes
    ;; a currently found list
    (define (list-device-at bus device function found)
      (let* ((current ;; at the current address
               (if (device-at? bus device function)
                   (let* ((info-line
                            (read-conf bus device function HEADER-INFO-OFFSET))
                          (class
                            (bitwise-and
                              #xFF
                              (arithmetic-shift info-line -24)))
                          (subclass
                            (bitwise-and
                              #xFF
                              (arithmetic-shift info-line -16))))
                     (make-pci-device
                       bus
                       device
                       function
                       class
                       subclass))
                   'NOTHING))
             (found (if (not (eq? 'NOTHING current))
                        (cons current found)
                        found)))
        (cond ((fx< function (-- FUNC-PER-DEV))
               (list-device-at bus device (++ function) found))
              ((fx< device (-- DEV-PER-BUS))
               (list-device-at bus (++ device) 0 found))
              ((fx< bus 0) ;; TODO: otherlines might be uninit, stay on 0 for now
               (list-device-at (++ bus) 0 0 found))
              (else found))))

    (define devices (list))

    ;; Make a list the devices that satisfy the predicate.
    ;; The predicate takes in argument the class code and subclass code
    ;; of the device.
    ;; The result is given as a list of list, where each sublist
    ;; is of the form '(bus device function)
    (define (list-devices pred)
      (if (null? devices)
       (set! devices (list-device-at #x00 #x00 #x00 '())))

      (filter devices (lambda (dev)
                       (pred (pci-device-class dev) (pci-device-subclass dev)))))

    ))
