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
    (define PCI_CONFIG_ADDR #xCF8)
    (define PCI_CONFIG_DATA #xCFC)
    (define PCI-BUS-COUNT 256)
    (define PCI-DEV-PER-BUS 32)
    (define PCI-FUNC-PER-DEV 8)

    ;; Convert an offset to a register number
    (define (offset->register offset)
      (>> offset 2))

    ;; Convert a register number to an offset
    (define (register->offset register)
      (<< register 2))

    ;; Make a value to place in the config address register
    (define (make-configuration-address rewrite-data? bus device function offset)
      (fxior
        (<< (if rewrite-data? 1 0) 31)
        (<< (fxand #xFF bus) 16)
        (<< (fxand #xFF device) 11)
        (<< (fxand #xFF function) 8)
        (fxand #xFF offset)
        ))

    ;; Read the pci config of a device at
    ;; the given bus, device and function.
    ;; The last parameter is the offset
    ;; to read from.
    (define (read-conf bus device function offset)
      (let ((address (make-configuration-address #t bus device function offset)))
        (outl address PCI-CONFIG-ADDR)
        (inl PCI_CONFIG_DATA)))


    ;; Verifies if a device is present at the given bus, device
    ;; and function
    (define (device-at? bus device function)
      (let ((vendor (read-conf bus device function #x00)))
        (not (fx= #xFFFFFFFF vendor))))

    (define (list-device-at bus device function pred found)
      (let* ((current ;; at the current address
               (if (device-at? bus device function)
                   (let* ((info-line (read-conf bus device function HEADER-INFO-OFFSET))
                          (class (fxand #xFF (>> info-line 24)))
                          (subclass (fxand #xFF (>> info-line 16))))
                     (if (pred class subclass)
                         (list bus device function)
                         'NOTHING))
                   'NOTHING))
             ((found (if (not (eq? 'NOTHING current))
                         (cons current found)
                         found))))
        (cond ((fx< function (-- PCI-FUNC-PER-DEV))
               (list-device-at bus device (++ function) found))
              (fx< device (-- PCI-DEV-PER-BUS)
                   (list-device-at bus (++ device) 0 found))
              ((fx< bus (-- PCI-BUS-COUNT))
               (list-device-at (++ bus) 0 0 found))
              (else found))))

    ;; Make a list the devices that satisfy the predicate
    ;; The predicate takes in argument the class code and subclass code
    ;; of the device
    (define (list-device pred)
     (list-device-at #x0000 #x0000 #x0000 pred '()))


    ))
