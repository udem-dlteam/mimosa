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
      ; Not a scientific measure, just a big number
      (define DISK-LIFE-EXPECTENCY (arithmetic-shift 1 32))
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
                   age
                   )

      (define-type disk
                   ide-device
                   mut
                   cache
                   type
                   time)

      (define disk-list (make-list MAX-NB-DISKS 'NO-DISK))

      (define (disk-get-older disk)
        (let ((time (disk-time disk)))
          (disk-time-set! disk (modulo (++ time) DISK-LIFE-EXPECTENCY))
          time))

      (define (evict-from-cache cache)
       (debug-write "Evicting")
       ; A better implementation would be a red black tree
       ; While it will take constant time evicting since it is a
       ; fixed size table, this is not a great implementation
       (let ((youngest-lba -1)
             (youth (++ DISK-LIFE-EXPECTENCY)))
        (table-for-each
         (lambda (lba sector)
          (let ((age (sector-age sector)))
           (if (< age youth)
            (begin
             (debug-write age)
             (set! youngest-lba lba)
             (set! youth age)))))
         cache)
        (if (< youngest-lba 0)
         (debug-write "Failed to find an entry to evict"))
        (table-set! cache youngest-lba) ; remove from cache
        ))

      (define (disk-fetch-and-set! disk lba)
        (let* ((mut (disk-mut disk))
              (cache (disk-cache disk))
              (used (table-length cache))
              ;; todo: check overflow
              (dev (disk-ide-device disk)))
          (mutex-lock! mut)
          (if (= used DISK-CACHE-MAX-SZ)
            (evict-from-cache cache))
          (let ((cleanup (lambda () (mutex-unlock! mut))))
          (ide-read-sectors
            dev
            lba
            1
            (lambda (raw-vect)
              (let ((sect (create-sector raw-vect lba disk)))
                (table-set! cache lba sect)
                (cleanup)
                sect))
            (lambda (err)
              (cleanup)
              err))
          )))

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
          0))

      (define (create-disk dev type)
        (make-disk
          dev
          (make-mutex)
          (make-table size: DISK-CACHE-MAX-SZ)
          type
          0))

      (define (disk-acquire-block disk lba mode)
        (let* ((sector (disk-read-sector disk lba))
               (age (sector-age sector))
               (refs (sector-ref-count sector))
               (mut (sector-mut sector)))
          (mutex-lock! mut)
          (if (eq? MODE-READ-WRITE mode)
           (sector-dirty?-set! sector #t))
          (sector-age-set! sector (max age (disk-get-older disk)))
          (sector-ref-count-set! sector (++ refs))
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
