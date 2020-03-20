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
                (begin
      (define BIG-READ-THRESH 5)
      (define MRO 'MODE-READ-ONLY)
      (define MRW 'MODE-READ-WRITE)
      (define MODE-READ-ONLY MRO)
      (define MODE-READ-WRITE MRW)
      (define DISK-CACHE-MAX-SZ 4098)
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
                   )

      (define-type disk
                   ide-device
                   mut
                   cache
                   cache-used
                   type
                   )

      (define disk-list (make-list MAX-NB-DISKS 'NO-DISK))

      (define (disk-fetch-and-set! disk lba)
        (let ((mut (disk-mut disk))
              (cache (disk-cache disk))
              (used (disk-cache-used disk));; todo: check overflow
              (dev (disk-ide-device disk)))
          (mutex-lock! mut)
          (let ((cleanup (lambda () (mutex-unlock! mut))))
          (ide-read-sectors
            dev
            lba
            1
            (lambda (raw-vect)
              (let ((sect (create-sector raw-vect lba disk)))
                (table-set! cache lba sect)
                (disk-cache-used-set! disk (++ used))
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
            (mutex-unlock! mut) r)))

      (define (flush-block disk sector)
        (let* ((smut (sector-mut sector))
               (dmut (disk-mut disk))
               (dev (disk-ide-device disk))
               (lba (sector-lba sector))
               (v (sector-vect sector))
               (cleanup (lambda ()
                          (sector-dirty?-set sector #f)
                          (mutex-unlock! dmut)
                          (mutex-unlock! smut); todo not sure if necessary
                          #t)))
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
          (lambda () disk))) 

      (define (create-disk dev type)
        (make-disk 
          dev
          (make-mutex)
          (make-table size: DISK-CACHE-MAX-SZ)
          0
          type))

      (define (disk-acquire-block disk lba mode)
        (let* ((sector (disk-read-sector disk lba))
               (refs (sector-ref-count sector))
               (mut (sector-mut sector)))
          (mutex-lock! mut)
          (if (eq? MODE-READ-WRITE mode)
           (sector-dirty?-set! sector #t))
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
              (flush-block disk sect))
          #t))

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
      (define (disk-apply-sector dsk lba mode fn)
        (let* ((sect (disk-acquire-block dsk lba mode))
               (rslt (fn (sector-vect sect))))
          (disk-release-block sect)
          rslt))
)
    (export disk-list
                        disk-ide-device
                        disk-apply-sector
                        disk-acquire-block
                        disk-release-block
                        disk-read-sectors
                        init-disks
                        MRO
                        MRW
                        MODE-READ-ONLY
                        MODE-READ-WRITE
                        disk-absent?
                        sector-vect))
