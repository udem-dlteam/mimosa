(define-library
  (rtc)
  (import
    (errors)
    (low-level)
    (gambit)
    (utils)
    (debug))
  (export
    pp-time
    pp-date
    rtc-current-time
    rtc-current-date)
  (begin
    ;; Definitions for MC 146818 real-time clock chip.
    ;;
    (define RTC-UIP-SLEEP 0.000250)
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
    (define RTC-REG-A         10)
    (define RTC-REG-B         11)
    (define RTC-REG-C         12)
    (define RTC-REG-D         13)
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

    ; Block of instructions that are executed
    ; atomically; no runtime or system interrupts
    ; are allowed during the block. The expression
    ; evaluates to the last expression.
    (define-macro (atomic . exprs)
     (let ((r (gensym)))
      `(begin
             (##disable-interrupts!) ;; Gambit ints
             (disable-interrupts) ;; System ints
             (let ((,r (begin
                        ,@exprs
                        )))
               (enable-interrupts)
               (##enable-interrupts!)
               ,r
               ))))

    (define-macro (rtc-command data)
     `(begin
       (outb ,data RTC-PORT-ADDR)
       (bcd->binary (inb RTC-PORT-DATA))))

    ; Fetches some data from the RTC. The result
    ; is a list of results with the first argument being a boolean indicating
    ; if the UIP flag was raised
    (define (rtc-commands-with-uip . commands)
      (let ((results (map
                       (lambda (data)
                         (outb data RTC-PORT-ADDR)
                         (bcd->binary (inb RTC-PORT-DATA)))
                       commands)))
        (cons (mask (inb RTC-REG-A) RTC-REGA-UIP) results)))

    (define pp-time
     (lambda (secs mins hours)
      (string-append
       (number->string hours) ":" (number->string mins) ":" (number->string secs))))

    (define pp-date
     (lambda (day month year)
      (string-append
       (number->string day) "/" (number->string month) "/" (number->string year))))

    ; Get the current time from the real time clock
    ; The result is passed to the C continuation in the format
    ; sec min hour
   (define (rtc-current-time c)
     (let ((data (atomic (rtc-commands-with-uip
                           RTC-SEC
                           RTC-MIN
                           RTC-HOUR))))
      (if (car data)
       (begin
        (thread-sleep! RTC-UIP-SLEEP)
        (rtc-current-time c))
       (apply c (cdr data)))))

   (define (rtc-current-date c)
     (let ((data (atomic (rtc-commands-with-uip
                           RTC-DAY-IN-MONTH
                           RTC-MONTH
                           RTC-YEAR))))
      (if (car data)
       (begin
        (thread-sleep! RTC-UIP-SLEEP)
        (rtc-current-date c))
       (c (cadr data) (caddr data) (+ 2000 (cadddr data))))))

    ))
