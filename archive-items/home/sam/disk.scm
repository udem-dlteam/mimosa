;; The mimosa project
; TODO: I do not know if a table is sync safe
; Ill assume yes for now
(define-library (disk)
                (import (gambit)
                        (errors)
                        (ide)
                        (utils)
                        (debug)
                        (low-level))
                (export disk-list
                        disk-ide-device
                        with-sector
                        disk-acquire-block
                        disk-release-block
                        disk-read-sectors
                        init-disks
                        MRO
                        MRW
                        MODE-READ-ONLY
                        MODE-READ-WRITE
                        disk-absent?
                        sector-vect)
    (begin
      (define READ-AHEAD #t)
      (define BIG-READ-THRESH 5)
      (define MRO 'MODE-READ-ONLY)
      (define MRW 'MODE-READ-WRITE)
      (define MODE-READ-ONLY MRO)
      (define MODE-READ-WRITE MRW)
      (define DISK-CACHE-MAX-SZ 1024)
      (define DISK-IDE 0)
      (define MAX-NB-DISKS 32)
      (define DISK-LOG2-BLOCK-SIZE 9)

      ; Disk types
      (define DISK-TYPE-IDE 'IDE-DISK)
      (define (disk-absent? disk)
       (eq? disk 'NO-DISK))

      (define-type sector
                   lba
                   dirty?
                   mut
                   vect
                   ref-count
                   disk-l
                   flush-cont
                   has-chance?
                   )

      (define-type disk
                   ide-device
                   mut
                   clock-hand
                   cache
                   chance-vect
                   type)

      (define disk-list (make-list MAX-NB-DISKS 'NO-DISK))

      (define (disk-get-older disk)
        (let ((time (disk-time disk)))
          (disk-time-set! disk (modulo (++ time) DISK-LIFE-EXPECTENCY))
          time))

      (define (evict-and-replace disk new-sector)
        (let ((clock-hand (disk-clock-hand disk))
              (chances (disk-chance-vect disk))
              (cache (disk-cache disk)))
          (let search ((clock-hand clock-hand))
           (let* ((lba (vector-ref chances clock-hand))
                  (sect (table-ref cache lba))
                  (has-chance? (sector-has-chance? sect)))
            (if (not has-chance?)
             (begin
              (vector-set! chances clock-hand (sector-lba new-sector))
              (table-set! cache lba) ; erase the old value
              (table-set! cache (sector-lba new-sector) new-sector) ; insert a new one
              (disk-clock-hand-set! disk (modulo (++ clock-hand) DISK-CACHE-MAX-SZ))) ; new value
             (begin
              (sector-has-chance?-set! sect #f) ; eat the chance
              (search (modulo (++ clock-hand) DISK-CACHE-MAX-SZ))))) ; forward
           )))

      (define (disk-fetch-and-set! disk lba)
        (let* ((mut (disk-mut disk))
               (cache (disk-cache disk))
               (chance-vect (disk-chance-vect disk))
               (clock-hand (disk-clock-hand disk))
               (dev (disk-ide-device disk))
               (used (table-length cache))
               (cleanup (lambda () (mutex-unlock! mut))))
          (mutex-lock! mut)
          (ide-read-sectors
            dev
            lba
            1
            (lambda (raw-vect)
              (let ((sect (create-sector raw-vect lba disk)))
                (if (= used DISK-CACHE-MAX-SZ)
                    (evict-and-replace disk sect)
                    (begin
                      (vector-set! chance-vect clock-hand lba)
                      (disk-clock-hand-set! disk (modulo (++ clock-hand) DISK-CACHE-MAX-SZ))
                      (table-set! cache lba sect)))
                (cleanup)
                sect))
            (lambda (err)
              (cleanup)
              err))
          ))

      (define (disk-read-sector disk lba)
        (let* ((cache (disk-cache disk))
               (dev (disk-ide-device disk))
               (cached (table-ref cache lba #f)))
          (if cached
              cached
              (disk-fetch-and-set! disk lba))))

      (define (queries-to-make lba count)
        (let* ((d (/ count IDE-MAX-SECTOR-READ))
              (q (floor d))
              (t (ceiling d))
              (r (modulo count IDE-MAX-SECTOR-READ)))
          (map
            (lambda (i)
              (if (< i q)
                  (cons (+ lba (* i IDE-MAX-SECTOR-READ)) IDE-MAX-SECTOR-READ)
                  (cons (+ lba (* i IDE-MAX-SECTOR-READ)) r)))
            (iota t))))

      (define (disk-read-sectors disk lba count)
        (let* ((dev (disk-ide-device disk))
               (q (queries-to-make lba count))
               (mut (disk-mut disk)))
          (mutex-lock! mut)
          (let ((r (fold
                     (lambda (query results)
                       (let ((lba (car query))
                             (qtt (cdr query)))
                         (ide-read-sectors
                           dev
                           lba
                           qtt
                           (lambda (raw-vect) (vector-append results raw-vect))
                           (lambda (err)
                             (debug-write "ERR:")
                             (debug-write (symbol->string err))
                             (make-vector (* 512 qtt) 0)))
                         ))
                     (make-vector 0 0)
                     q)))
            (mutex-unlock! mut)
            r)))

      (define (flush-block disk sector)
        (let* ((smut (sector-mut sector))
               (dmut (disk-mut disk))
               (dev (disk-ide-device disk))
               (lba (sector-lba sector))
               (v (sector-vect sector))
               (cleanup (lambda ()
                          (sector-flush-cont-set! sector #f)
                          (sector-dirty?-set! sector #f)
                          (mutex-unlock! dmut)
                          (mutex-unlock! smut); todo not sure if necessary
                          #t)))
          (debug-write "Flushing...")
          (mutex-lock! smut)
          (mutex-lock! dmut)
          (ide-write-sectors
            dev
            lba
            v
            1
            cleanup
            (lambda (err) (cleanup) err))))

      (define (create-sector v lba disk)
        (make-sector
          lba
          #f
          (make-mutex)
          v
          0
          (lambda () disk)
          #f
          #t))

      (define (create-disk dev type)
        (make-disk
          dev
          (make-mutex)
          0
          (make-table size: DISK-CACHE-MAX-SZ)
          (make-vector DISK-CACHE-MAX-SZ #f)
          type
          ))

      (define (disk-acquire-block disk lba mode)
        (let* ((sector (disk-read-sector disk lba))
               (refs (sector-ref-count sector))
               (mut (sector-mut sector)))
          (mutex-lock! mut)
          (if (eq? MODE-READ-WRITE mode)
           (sector-dirty?-set! sector #t))
          (sector-has-chance?-set! sector #t)
          (mutex-unlock! mut)
          sector))


      (define (disk-release-block sect)
        (let* ((lba (sector-lba sect))
               (mut (sector-mut sect))
               (disk ((sector-disk-l sect)))
               (refs (sector-ref-count sect)))
          (mutex-lock! mut)
          (sector-ref-count-set! sect (- refs 1))
          (mutex-unlock! mut)
          (if (and (= refs 1) (sector-dirty? sect))
              (prepare-flushing disk sect))
          #t))


      (define (prepare-flushing disk sect)
        (let ((c (sector-flush-cont sect))
              (mut (sector-mut sect)))
          (if (not c)
           (let* ((c (lambda () (flush-block disk sect)))
                 (t (make-thread c)))
              (mutex-lock! mut)
              (sector-flush-cont-set! sect c)
              (mutex-unlock! mut)
              (thread-start! t)))))

      (define (init-disks)
        (let ((disk-idx 0))
          (for-each
            (lambda (ctrl)
              (for-each (lambda (dev)
                          (begin
                            (list-set! disk-list disk-idx (create-disk dev DISK-TYPE-IDE))
                            (set! disk-idx (++ disk-idx))))
                        (filter (ide-controller-devices ctrl)
                                (o not device-absent?))))
            (vector->list IDE-CTRL-VECT))
          disk-list))

      (define (init-disks)
        (let* ((ide-devices (ide#list-devices))
               (zipped (zip disk-list ide-devices)))
          (set! disk-list (map (lambda (e)
                                 (if (pair? e)
                                     (create-disk (cadr e) DISK-TYPE-IDE)
                                     e)) zipped))))

      ; For a disk, a block address, apply function
      ; fn on the sector vector
      (define (with-sector dsk lba mode fn)
        (let* ((sect (disk-acquire-block dsk lba mode))
               (rslt (fn (sector-vect sect))))
          (disk-release-block sect)
          rslt))
))
