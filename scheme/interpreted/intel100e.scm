(define-library (intel100e)
 (import
  (utils)
  (errors)
  (gambit)
  (pci)
  (debug)
  (net)
  )
 (export
  intel100e-setup)
 (begin

   (define-type intel100e-dev
                memory-port
                flash-port
                io-port
                msi-x-port
                rsvdA
                rsvdB
                )

   (define (setup-card bus device function net-dev)
     (let ((bars (map (partial pci#read-conf bus device function) pci#BAR-LIST))
          )
      (net#network-device-rsvd-set!
        net-dev
        (make-intel100e-dev
         (arithmetic-shift (list-ref bars 0) -4) ;; memory bar
         (arithmetic-shift (list-ref bars 1) -4) ;; flash bar
         (arithmetic-shift (list-ref bars 2) -2) ;; io bar
         (arithmetic-shift (list-ref bars 3) -4) ;; msi-x bar
         (list-ref bars 4) ;; rsvdA
         (list-ref bars 5) ;; rsvdB
         ))
      net-dev
     ))



   (define (init-device net-dev)
     (debug-write "Init intel100e card")
     (let* ((pci-dev (net#network-device-pci-dev net-dev))
            (bus (pci#pci-device-bus pci-dev))
            (device (pci#pci-device-device pci-dev))
            (function (pci#pci-device-function pci-dev))
            (header-info-line (pci#read-conf bus device function pci#HEADER-INFO-OFFSET))
            (header-type (bitwise-and #xFF (arithmetic-shift header-info-line -16))))
      (if (= #x00 header-type)
       (setup-card bus device function net-dev)
       (begin
        (debug-write "Error while setting up intel100e device.")
        net-dev
        ))))

   (define (intel100e-setup)
     (net#register-driver #x8086 #x100e init-device))


   ))
