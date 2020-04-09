;; The mimosa project
(declare (not safe))
(define-library (keyboard)
(import
 (gambit)
 (utils)
 (debug)
 (low-level))
(export handle-kbd-int)
(begin
  (define STDIN (open-output-file "/sys/stdin"))
  (define DEAD 'DEAD)
  (define NULL 'NULL)
  (define PS2-MOUSE-BUFF-SIZE 3)
  (define PS2-PORT-A #x60)
  (define PS2-PORT-B #x61)
  (define PS2-PORT-C #x62)
  (define PS2-PORT-CMD #x64)
  (define PS2-PORT-STATUS #x64)
  (define PS2-B-SW1 (<< 1 7) )
  (define PS2-B-TKT (<< 1 6) )
  (define PS2-B-NME (<< 1 5) )
  (define PS2-B-NMI (<< 1 4) )
  (define PS2-B-REC (<< 1 3) )
  (define PS2-B-SW2 (<< 1 2) )
  (define PS2-B-SPK (<< 1 1) )
  (define PS2-B-RSG (<< 1 0) )
  (define PS2-C-PAR (<< 1 7) )
  (define PS2-C-ERW (<< 1 6) )
  (define PS2-C-TIM (<< 1 5) )
  (define PS2-C-TON (<< 1 4) )
  (define PS2-C-HS5 (<< 1 3) )
  (define PS2-C-HS4 (<< 1 2))
  (define PS2-C-HS3 (<< 1 1))
  (define PS2-C-HS2-HS6 (<< 1 0))
  (define PS2-PARE (<< 1 7) )
  (define PS2-TIM  (<< 1 6) )
  (define PS2-AUXB (<< 1 5) )
  (define PS2-KEYL (<< 1 4) )
  (define PS2-C-D  (<< 1 3) )
  (define PS2-SYSF (<< 1 2) )
  (define PS2-INPB (<< 1 1) )
  (define PS2-OUTB (<< 1 0) )
  (define PS2-CMD-CONFIG           #x60)
  (define PS2-CMD-DISABLE-MOUSE    #xa7)
  (define PS2-CMD-ENABLE-MOUSE     #xa8)
  (define PS2-CMD-DISABLE-KEYBOARD #xad)
  (define PS2-CMD-ENABLE-KEYBOARD  #xae)
  (define PS2-CMD-WRITE-MOUSE      #xd4)
  (define PS2-MOUSE-CMD-SET-SCALE11       #xe6)
  (define PS2-MOUSE-CMD-SET-SCALE21       #xe7)
  (define PS2-MOUSE-CMD-SET-RESOLUTION    #xe8)
  (define PS2-MOUSE-CMD-GET-STATUS        #xe9)
  (define PS2-MOUSE-CMD-SET-STREAM-MODE   #xea)
  (define PS2-MOUSE-CMD-GET-REPORT        #xeb)
  (define PS2-MOUSE-CMD-SET-REMOTE-MODE   #xf0)
  (define PS2-MOUSE-CMD-GET-ID            #xf2)
  (define PS2-MOUSE-CMD-SET-SAMPLE-RATE   #xf3)
  (define PS2-MOUSE-CMD-ENABLE-REPORTING  #xf4)
  (define PS2-MOUSE-CMD-DISABLE-REPORTING #xf5)
  (define PS2-MOUSE-CMD-SET-DEFAULTS      #xf6)
  (define PS2-MOUSE-CMD-RESEND            #xfe)
  (define PS2-MOUSE-CMD-RESET             #xff)
  (define PS2-MOUSE-ACK      #xfa)
  (define PS2-MOUSE-BAT-ACK  #xaa)
  (define PS2-MOUSE-ID-PS2   0)
  (define PS2-MOUSE-ID-IMPS2 3)
  (define PS2-MOUSE-10-SAMPLES-PER-SEC  10)
  (define PS2-MOUSE-20-SAMPLES-PER-SEC  20)
  (define PS2-MOUSE-40-SAMPLES-PER-SEC  40)
  (define PS2-MOUSE-60-SAMPLES-PER-SEC  60)
  (define PS2-MOUSE-80-SAMPLES-PER-SEC  80)
  (define PS2-MOUSE-100-SAMPLES-PER-SEC 100)
  (define PS2-MOUSE-200-SAMPLES-PER-SEC 200)
  (define PS2-MOUSE-1-COUNTS-PER-MM 0)
  (define PS2-MOUSE-2-COUNTS-PER-MM 1)
  (define PS2-MOUSE-4-COUNTS-PER-MM 2)
  (define PS2-MOUSE-8-COUNTS-PER-MM 3)
  (define PS2-CONFIG-SCAN-CONVERT       (<< 1 6))
  (define PS2-CONFIG-AUXB-CLOCK-DISABLE (<< 1 5))
  (define PS2-CONFIG-KBD-CLOCK-DISABLE  (<< 1 4))
  (define PS2-CONFIG-SYSF               (<< 1 2))
  (define PS2-CONFIG-ENABLE-IRQ12       (<< 1 1))
  (define PS2-CONFIG-ENABLE-IRQ1        (<< 1 0))
  (define KBD-SCANCODE-ESC       #x01)
  (define KBD-SCANCODE-1         #x02)
  (define KBD-SCANCODE-2         #x03)
  (define KBD-SCANCODE-3         #x04)
  (define KBD-SCANCODE-4         #x05)
  (define KBD-SCANCODE-5         #x06)
  (define KBD-SCANCODE-6         #x07)
  (define KBD-SCANCODE-7         #x08)
  (define KBD-SCANCODE-8         #x09)
  (define KBD-SCANCODE-9         #x0A)
  (define KBD-SCANCODE-0         #x0B)
  (define KBD-SCANCODE-MINUS     #x0C)
  (define KBD-SCANCODE-EQUAL     #x0D)
  (define KBD-SCANCODE-BSPACE    #x0E)
  (define KBD-SCANCODE-TAB       #x0F)
  (define KBD-SCANCODE-Q         #x10)
  (define KBD-SCANCODE-W         #x11)
  (define KBD-SCANCODE-E         #x12)
  (define KBD-SCANCODE-R         #x13)
  (define KBD-SCANCODE-T         #x14)
  (define KBD-SCANCODE-Y         #x15)
  (define KBD-SCANCODE-U         #x16)
  (define KBD-SCANCODE-I         #x17)
  (define KBD-SCANCODE-O         #x18)
  (define KBD-SCANCODE-P         #x19)
  (define KBD-SCANCODE-LBRACK    #x1A)
  (define KBD-SCANCODE-RBRACK    #x1B)
  (define KBD-SCANCODE-ENTER     #x1C)
  (define KBD-SCANCODE-CTRL      #x1D)
  (define KBD-SCANCODE-A         #x1E)
  (define KBD-SCANCODE-S         #x1F)
  (define KBD-SCANCODE-D         #x20)
  (define KBD-SCANCODE-F         #x21)
  (define KBD-SCANCODE-G         #x22)
  (define KBD-SCANCODE-H         #x23)
  (define KBD-SCANCODE-J         #x24)
  (define KBD-SCANCODE-K         #x25)
  (define KBD-SCANCODE-L         #x26)
  (define KBD-SCANCODE-SEMICOLON #x27)
  (define KBD-SCANCODE-QUOTE     #x28)
  (define KBD-SCANCODE-BQUOTE    #x29)
  (define KBD-SCANCODE-LSHIFT    #x2A)
  (define KBD-SCANCODE-BSLASH    #x2B)
  (define KBD-SCANCODE-Z         #x2C)
  (define KBD-SCANCODE-X         #x2D)
  (define KBD-SCANCODE-C         #x2E)
  (define KBD-SCANCODE-V         #x2F)
  (define KBD-SCANCODE-B         #x30)
  (define KBD-SCANCODE-N         #x31)
  (define KBD-SCANCODE-M         #x32)
  (define KBD-SCANCODE-COMMA     #x33)
  (define KBD-SCANCODE-PERIOD    #x34)
  (define KBD-SCANCODE-SLASH     #x35)
  (define KBD-SCANCODE-RSHIFT    #x36)
  (define KBD-SCANCODE-PRTSC     #x37)
  (define KBD-SCANCODE-ALT       #x38)
  (define KBD-SCANCODE-SPACE     #x39)
  (define KBD-SCANCODE-CAPS      #x3A)
  (define KBD-SCANCODE-F1        #x3B)
  (define KBD-SCANCODE-F2        #x3C)
  (define KBD-SCANCODE-F3        #x3D)
  (define KBD-SCANCODE-F4        #x3E)
  (define KBD-SCANCODE-F5        #x3F)
  (define KBD-SCANCODE-F6        #x40)
  (define KBD-SCANCODE-F7        #x41)
  (define KBD-SCANCODE-F8        #x42)
  (define KBD-SCANCODE-F9        #x43)
  (define KBD-SCANCODE-F10       #x44)
  (define KBD-SCANCODE-NUM       #x45)
  (define KBD-SCANCODE-SCRL      #x46)
  (define KBD-SCANCODE-HOME      #x47)
  (define KBD-SCANCODE-UP        #x48)
  (define KBD-SCANCODE-PGUP      #x49)
  (define KBD-SCANCODE-NUMMINUS  #x4A)
  (define KBD-SCANCODE-LEFT      #x4B)
  (define KBD-SCANCODE-CENTER    #x4C)
  (define KBD-SCANCODE-RIGHT     #x4D)
  (define KBD-SCANCODE-PLUS      #x4E)
  (define KBD-SCANCODE-END       #x4F)
  (define KBD-SCANCODE-DOWN      #x50)
  (define KBD-SCANCODE-PGDN      #x51)
  (define KBD-SCANCODE-INS       #x52)
  (define KBD-SCANCODE-DEL       #x53)
  (define KBD-SCANCODE-UNDEF1    #x54)
  (define KBD-SCANCODE-UNDEF2    #x55)
  (define KBD-SCANCODE-UNDEF3    #x56)
  (define KBD-SCANCODE-F11       #x57)
  (define KBD-SCANCODE-F12       #x58)
  (define KBD-SCANCODE-LWINDOW   #x5B )
  (define KBD-SCANCODE-RWINDOW   #x5C )
  (define KBD-SCANCODE-MENU      #x5D )
  (define scancodes
    (vector
      0 DEAD DEAD DEAD NULL  ; not a valid scan code
      #x011b #x011b #x011b #x0100 NULL  ; ESC
      #x0231 #x0221 DEAD #x7800 NULL   ; 1
      #x0332 #x0340 #x0300 #x7900 NULL  ; 2
      #x0433 #x0423 DEAD #x7a00 NULL  ; 3
      #x0534 #x0524 DEAD #x7b00 NULL  ; 4
      #x0635 #x0625 DEAD #x7c00 NULL  ; 5
      #x0736 #x075e #x071e #x7d00 NULL  ; 6
      #x0837 #x0826 DEAD #x7e00 NULL  ; 7
      #x0938 #x092a DEAD #x7f00 NULL  ; 8
      #x0a39 #x0a28 DEAD #x8000 NULL  ; 9
      #x0b30 #x0b29 DEAD #x8100 NULL   ;0
      #x0c2d #x0c5f #x0c1f #x8200 NULL
      #x0d3d #x0d2b DEAD #x8300 NULL
      #x0e08 #x0e08 #x0e7f DEAD NULL
      #x0f09 #x0f00 DEAD DEAD NULL
      #x1071 #x1051 #x1011 #x1000 NULL
      #x1177 #x1157 #x1117 #x1100 NULL
      #x1265 #x1245 #x1205 #x1200 NULL
      #x1372 #x1352 #x1312 #x1300 NULL
      #x1474 #x1454 #x1414 #x1400 NULL
      #x1579 #x1559 #x1519 #x1500 NULL
      #x1675 #x1655 #x1615 #x1600 NULL
      #x1769 #x1749 #x1709 #x1700 NULL
      #x186f #x184f #x180f #x1800 NULL
      #x1970 #x1950 #x1910 #x1900 NULL
      #x1a5b #x1a7b #x1a1b DEAD NULL
      #x1b5d #x1b7d #x1b1d DEAD NULL
      #x1c0d #x1c0d #x1c0a DEAD NULL
      DEAD DEAD DEAD DEAD NULL
      #x1e61 #x1e41 #x1e01 #x1e00 NULL
      #x1f73 #x1f53 #x1f13 #x1f00 NULL
      #x2064 #x2044 #x2004 #x2000 NULL
      #x2166 #x2146 #x2106 #x2100 NULL
      #x2267 #x2247 #x2207 #x2200 NULL
      #x2368 #x2348 #x2308 #x2300 NULL
      #x246a #x244a #x240a #x2400 NULL
      #x256b #x254b #x250b #x2500 NULL
      #x266c #x264c #x260c #x2600 NULL
      #x273b #x273a DEAD DEAD NULL
      #x2827 #x2822 DEAD DEAD NULL
      #x2960 #x297e DEAD DEAD NULL
      DEAD DEAD DEAD DEAD NULL
      #x2b5c #x2b7c #x2b1c DEAD NULL
      #x2c7a #x2c5a #x2c1a #x2c00 NULL
      #x2d78 #x2d58 #x2d18 #x2d00 NULL
      #x2e63 #x2e43 #x2e03 #x2e00 NULL
      #x2f76 #x2f56 #x2f16 #x2f00 NULL
      #x3062 #x3042 #x3002 #x3000 NULL
      #x316e #x314e #x310e #x3100 NULL
      #x326d #x324d #x320d #x3200 NULL
      #x332c #x333c DEAD DEAD NULL
      #x342e #x343e DEAD DEAD NULL
      #x352f #x353f DEAD DEAD NULL
      DEAD DEAD DEAD DEAD NULL
      #x372a #x372a DEAD DEAD NULL
      DEAD DEAD DEAD DEAD NULL
      #x3920 #x3920 #x3900 #x3920 NULL
      DEAD DEAD DEAD DEAD NULL
      #x3b00 #x5400 #x5e00 #x6800 "\033OP"
      #x3c00 #x5500 #x5f00 #x6900 "\033OQ"
      #x3d00 #x5600 #x6000 #x6a00 "\033OR"
      #x3e00 #x5700 #x6100 #x6b00 "\033OS"
      #x3f00 #x5800 #x6200 #x6c00 "\033[15~"
      #x4000 #x5900 #x6300 #x6d00 "\033[17~"
      #x4100 #x5a00 #x6400 #x6e00 "\033[18~"
      #x4200 #x5b00 #x6500 #x6f00 "\033[19~"
      #x4300 #x5c00 #x6600 #x7000 "\033[20~"
      #x4400 #x5d00 #x6700 #x7100 "\033[21~"
      DEAD DEAD DEAD DEAD NULL
      DEAD DEAD DEAD DEAD NULL
      #x4700 #x4737 #x7700 DEAD "\033[H"
      #x4800 #x4838 DEAD DEAD "\033[A"
      #x4900 #x4939 #x8400 DEAD NULL
      #x4a2d #x4a2d DEAD DEAD NULL
      #x4b00 #x4b34 #x7300 DEAD "\033[D"
      #x4c00 #x4c35 DEAD DEAD NULL
      #x4d00 #x4d36 #x7400 DEAD "\033[C"
      #x4e2b #x4e2b DEAD DEAD NULL
      #x4f00 #x4f31 #x7500 DEAD "\033[F"
      #x5000 #x5032 DEAD DEAD "\033[B"
      #x5100 #x5133 #x7600 DEAD NULL
      #x5200 #x5230 DEAD DEAD "\033[2~"
      #x5300 #x532e DEAD DEAD NULL
      DEAD DEAD DEAD DEAD NULL
      DEAD DEAD DEAD DEAD NULL
      DEAD DEAD DEAD DEAD NULL
      DEAD DEAD DEAD DEAD "\033[23~"
      DEAD DEAD DEAD DEAD "\033[24~"
      ))

  (define key-modifier-normal 0 )
  (define key-modifier-with-shift 1)
  (define key-modifier-with-ctrl 2)
  (define key-modifier-with-alt 3)
  (define key-modifier-seq 4)
  (define keymap (vector 0 0 0 0))

  (define (write-char-stdin c)
    (begin
      (write-char c STDIN)
      (flush-output-port STDIN)
      c))

  (define (kbd-int->scancode pos modifier)
    (let ((idx (* 5 pos)))
      (vector-ref scancodes (+ idx modifier))))

  (define (pressed? code)
    (let* ((shifted (>> code 5))
           (key (vector-ref keymap shifted))
           (mask (##fxarithmetic-shift 1 (##fxand code #x1F))))
      (not (= (fxand key mask) 0))))

  (define (update-active-modifier code action)
    (let* ((shifted (>> code 5))
           (key (vector-ref keymap shifted))
           (mask (##fxarithmetic-shift 1 (##fxand code #x1F))))
      ;; TODO maybe the not is not working properly
      (vector-set! keymap shifted (action mask key))))

  (define (handle-visible-key data)
    (let ((code (cond ((or (pressed? KBD-SCANCODE-LSHIFT) (pressed? KBD-SCANCODE-RSHIFT))
                       (kbd-int->scancode data key-modifier-with-shift))
                      ((pressed? KBD-SCANCODE-CTRL)
                       (kbd-int->scancode data key-modifier-with-ctrl))
                      ((pressed? KBD-SCANCODE-ALT)
                       (kbd-int->scancode data key-modifier-with-alt))
                      (else
                        (kbd-int->scancode data key-modifier-normal)))))
      ; Update the keypress; if not dead, process it
      (update-active-modifier data ##fxior)
      (if (not (eq? code DEAD))
          (write-char-stdin (integer->char (fxand code #xFF))))))

  (define (handle-kbd-int data)
    (cond ((fx<= KBD-SCANCODE-ESC data KBD-SCANCODE-F12)
           (begin
             (handle-visible-key data)))
          ((fx>= (fxior KBD-SCANCODE-F12 #x80) data (fxior KBD-SCANCODE-ESC #x80))
           (begin
             (update-active-modifier
               (fxand data #x7F)
               (lambda (mask key)
                 (##fxand (##fxnot mask) key)))))
          (else (debug-write "else"))))
  ))
