(define-library (net)
 (import
   (utils)
   (errors)
   (gambit)
   (pci)
   (rtc)
   (debug))
 (export
  net-setup)
 (begin

  (define drivers (list))

  (define (register-driver vendor-id device-id init)
   (set! drivers
    (cons (cons (list vendor-id device-id) init) drivers)))

  (define (is-ethernet-controller? class subclass)
   (and (= class pci#CLASS-NETWORK)
        (= subclass pci#SUBCLASS-ETHERNET)))

  ;; List of pci#pci-device (s)
  ;; that are detected
  (define hardware (list))

  (define-type network-device
   pci-dev
   write
   read
   device-id
   vendor-id
   rsvd ;; this field is reserved for implementations
   )

  (define (pci-device->network-device dev)
    (let* ((bus (pci#pci-device-bus dev))
           (device (pci#pci-device-device dev))
           (function (pci#pci-device-function dev))
           (info-line (pci#read-conf bus device function pci#HEADER-HW-INFO-OFFSET)))
      (let* ((device-id (bitwise-and #xFFFF (arithmetic-shift info-line -16)))
             (vendor-id (bitwise-and #xFFFF info-line))
             (net-dev (make-network-device
                        dev
                        'TODO
                        'TODO
                        device-id
                        vendor-id
                        'TODO))
             (setup (assoc (list vendor-id device-id) drivers)))
       (if setup
        ((cdr setup) net-dev)
        net-dev
        ))))

  (define (net-setup)
   (set! hardware (map pci-device->network-device (pci#list-devices is-ethernet-controller?))))

  ))
