(define-library
  (rtc)
  (import
    (errors)
    (low-level)
    (gambit)
    (utils)
    (debug))
  (export
    rtc-current-time
    rtc-current-date)
  (begin
    ;; Definitions for MC 146818 real-time clock chip.
    ;;
    (define RTC-PORT-ADDR #x70)
    (define RTC-PORT-DATA #x71)
    (define RTC-IRQ 8)
    (define RTC-SEC          0)
    (define RTC-SEC-ALARM    1)
    (define RTC-MIN          2)
    (define RTC-MIN-ALARM    3)
    (define RTC-HOUR         4)
    (define RTC-HOUR-ALARM   5)
    (define RTC-DAY-IN-WEEK  6)
    (define RTC-DAY-IN-MONTH 7)
    (define RTC-MONTH        8)
    (define RTC-YEAR         9)
    (define RTC-REGA         10)
    (define RTC-REGB         11)
    (define RTC-REGC         12)
    (define RTC-REGD         13)
    (define RTC-REGA-UIP     (fxarithmetic-shift 1 7))
    (define RTC-REGA-OSC     (fxarithmetic-shift 7 4))
    (define RTC-REGA-OSC-ON  (fxarithmetic-shift 2 4))
    (define RTC-REGA-OSC-OFF (fxarithmetic-shift 0 4))
    (define RTC-REGA-8192HZ  3)
    (define RTC-REGA-4096HZ  4)
    (define RTC-REGA-2048HZ  5)
    (define RTC-REGA-1024HZ  6)
    (define RTC-REGA-512HZ   7)
    (define RTC-REGA-256HZ   8)
    (define RTC-REGA-128HZ   9)
    (define RTC-REGA-64HZ    10)
    (define RTC-REGA-32HZ    11)
    (define RTC-REGA-16HZ    12)
    (define RTC-REGA-8HZ     13)
    (define RTC-REGA-4HZ     14)
    (define RTC-REGA-2HZ     15)
    (define RTC-REGB-SET    (fxarithmetic-shift 1 7)) ;; Disable Clock Updates
    (define RTC-REGB-PIE    (fxarithmetic-shift 1 6)) ;; Periodic Interrupt Enable
    (define RTC-REGB-AIE    (fxarithmetic-shift 1 5)) ;; Alarm Interrupt Enable
    (define RTC-REGB-UIE    (fxarithmetic-shift 1 4)) ;; Update Ended Interrupt Enable
    (define RTC-REGB-SQWE   (fxarithmetic-shift 1 3)) ;; Square Wave Enable
    (define RTC-REGB-DM     (fxarithmetic-shift 1 2)) ;; Data Mode mask
    (define RTC-REGB-DM-BIN (fxarithmetic-shift 1 2)) ;; Binary Data Mode
    (define RTC-REGB-DM-BCD (fxarithmetic-shift 0 2)) ;; Binary Coded Decimal Data Mode
    (define RTC-REGB-2412   (fxarithmetic-shift 1 1)) ;; 24/12 hour mode mask
    (define RTC-REGB-24     (fxarithmetic-shift 1 1)) ;; 24 hour mode
    (define RTC-REGB-12     (fxarithmetic-shift 0 1)) ;; 12 hour mode
    (define RTC-REGB-DSE    (fxarithmetic-shift 1 0)) ;; Daylight Savings Enable
    (define RTC-REGC-IRQF   (fxarithmetic-shift 1 7)) ;; Interrupt Request Flag
    (define RTC-REGC-PF     (fxarithmetic-shift 1 6)) ;; Periodic Interrupt Flag
    (define RTC-REGC-AF     (fxarithmetic-shift 1 5)) ;; Alarm Interrupt Flag
    (define RTC-REGC-UF     (fxarithmetic-shift 1 4)) ;; Update Ended Interrupt Flag
    (define RTC-REGD-VRT    (fxarithmetic-shift 1 7)) ;; Valid Ram and Time

    (define-macro (rtc-command data)
     `(begin
       (outb ,data RTC-PORT-ADDR)
       (bcd->binary (inb RTC-PORT-DATA))))

    ; Get the current time from the real time clock
    ; The result is passed to the C continuation in the format
    ; hour min sec
    (define (rtc-current-time c)
      (disable-interrupts) ; System interrupts
      (let ((secs (rtc-command RTC-SEC))
            (mins (rtc-command RTC-MIN))
            (hours (rtc-command RTC-HOUR)))
        (enable-interrupts) ; System interrupts
        (c hours mins secs)))

   (define (rtc-current-date c)
    (disable-interrupts)
    (let ((day (rtc-command RTC-DAY-IN-MONTH))))
    ; TODO
    (enable-interrupts)
    (c day month year)))
    ))
