; Mimosa
; Université de Montréal
; Marc Feeley, Samuel Yvon
(define-library
  (disk)
  (import
    (gambit)
    (errors)
    (ide)
    (utils)
    (debug)
    (low-level))
  (export
    disk-list
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
    ; Enable read-ahead during disk reads
    (define READ-AHEAD #t)
    (define MRO 'MODE-READ-ONLY)
    ; The following two definitions are for controlling
    ; the disc locks and control wether the cached block
    ; is marked dirty
    (define MRW 'MODE-READ-WRITE)
    (define MODE-READ-ONLY MRO)
    (define MODE-READ-WRITE MRW)
    ; Max size of the disk cache. It will start
    ; evicting blocks after the limit is reached
    (define DISK-CACHE-MAX-SZ 1024)
    ; Maximum number of supported disks
    (define MAX-NB-DISKS 32)
    (define DISK-LOG2-BLOCK-SIZE 9)

    ; Disk types
    (define DISK-TYPE-IDE 'IDE-DISK)

    ; Disks that are not detected are flagged as 'NO-DISK
    (define (disk-absent? disk) (eq? disk 'NO-DISK))

    (define-type sector
                 lba ; the LBA of the cached sector
                 dirty? ; if the vector is dirty
                 mut ; the mutex for sector access
                 vect ; the vector that contains the data from the sector; size of 2^{DISK-LOG2-BLOCK-SIZE}
                 ref-count ; the number of references to the sector cache
                 disk-l ; a lambda that wraps the disk; (disk-l) => disk structure
                 flush-cont; continuation for disk flushing
                 has-chance? ; if the sector has a chance before being evicted (second-chance algorithm)
                 )

    (define-type disk
                 ide-device; the IDE device that represents the disk
                 mut ; mut for access to the the disk
                 clock-hand ; index in the chance-vect for what sector must be evicted
                 cache ; the cache (a table) for the disk
                 chance-vect ; vector of sectors used for managing the second chance algorithm
                 type ; type of disk
                 read-ahead-queue ; queue of read-ahead being performed
                 )

    ; All the disks that are detected are put in this list;
    ; Disks that are not detected are set as 'NO-DISK
    (define disk-list (make-list MAX-NB-DISKS 'NO-DISK))

    ; This function takes a disk and a new sector cached block
    ; and finds the least pertinent cached sector to evict and replace
    ; it by the argument. It implements the "second chance" LRU approximation
    ; algorithm.
    ; See https://en.wikipedia.org/wiki/Page_replacement_algorithm#Second-chance
    (define (evict-and-replace disk new-sector)
      (let ((chances (disk-chance-vect disk))
            (cache (disk-cache disk)))
        (let search ((clock-hand (disk-clock-hand disk)))
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
                  (sector-has-chance?-set! sect #f) ; white pants a chance
                  (search (modulo (++ clock-hand) DISK-CACHE-MAX-SZ))))) ; forward
          )))

    ; Fetch a sector from a disk, identified by it's address,
    ; and cache it into the disk cache.
    (define (disk-fetch-and-cache! disk lba)
      (let* ((mut (disk-mut disk))
             (cache (disk-cache disk))
             (chance-vect (disk-chance-vect disk))
             (clock-hand (disk-clock-hand disk))
             (dev (disk-ide-device disk))
             (used (table-length cache))
             (cleanup-and-ret (lambda (val) (mutex-unlock! mut) val)))
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
              (cleanup-and-ret sect)))
          cleanup-and-ret)))

    ; Perform the read ahead, setup threading structures
    ; A call to this method expects and requires the disk mutex
    ; to be held
    (define (do-read-ahead disk lba)
      (let ((rh-queue (disk-read-ahead-queue disk)))
        (if (not (table-ref rh-queue lba #f)) ; if it's already there, ignore
            (let* ((mut (disk-mut disk))
                   (cv (make-condition-variable))
                   (l (lambda _
                        ; (debug-write "Disk thread started")
                        (disk-fetch-and-cache! disk lba)
                        (mutex-lock! mut)
                        (condition-variable-signal! cv)
                        (mutex-unlock! mut))))
              (table-set! rh-queue lba cv)
              (thread-start!  (make-thread l))))))

    ; Check if a sector identified by an address is
    ; cached. If so, it will return the sector, otherwise
    ; it will return false.
    (define (check-cache disk lba)
      (let* ((mut (disk-mut disk))
             (cache (disk-cache disk))
             (clean-and-ret (lambda (r)
                              (mutex-unlock! mut)
                              r)))
        (mutex-lock! mut)
        (let ((cached (table-ref cache lba #f))
              (rh-queue (disk-read-ahead-queue disk)))
          (if cached
              (begin
                (table-set! rh-queue lba)
                (clean-and-ret cached))
              (let ((qued-cv (table-ref rh-queue lba #f)))
                (if qued-cv
                    (let chk ((cached (table-ref cache lba #f)))
                      ; (debug-write "W8 for query to complete")
                      (if (not chk)
                          (begin
                            (mutex-unlock! mut qued-cv)
                            (mutex-lock! mut)
                            (chk (table-ref cache lba #f)))
                          (begin
                            (table-set! rh-queue lba) ; rmv
                            (clean-and-ret cached))))
                    (clean-and-ret #f))
                )))))

    ; Read a sector identified by an address from a disk
    (define (disk-read-sector disk lba)
      (let* ((mut (disk-mut disk))
             (cache (disk-cache disk))
             (dev (disk-ide-device disk))
             (cached (check-cache disk lba)))
        (let ((result (if cached
                          cached
                          (disk-fetch-and-cache! disk lba))))
          (mutex-lock! mut)
          (if (and READ-AHEAD (not (table-ref cache (++ lba) #f)))
              ; Read it in background
              (do-read-ahead disk (++ lba)))
          (mutex-unlock! mut)
          result)))

    ; Return the number of disk reads (of a sector) required to be made
    ; to read count sectors starting from lba
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

    ; Read multiples sectors from a disk
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

    ; Flush a cached sector to the disk
    (define (flush-block disk sector)
      (let* ((smut (sector-mut sector))
             (dmut (disk-mut disk))
             (dev (disk-ide-device disk))
             (lba (sector-lba sector))
             (v (sector-vect sector))
             (cleanup-and-ret (lambda (v)
                                (sector-flush-cont-set! sector #f)
                                (sector-dirty?-set! sector #f)
                                (mutex-unlock! dmut)
                                (mutex-unlock! smut); todo not sure if necessary
                                v)))
        (mutex-lock! smut)
        (mutex-lock! dmut)
        (ide-write-sectors
          dev
          lba
          v
          1
          (partial cleanup #t)
          cleanup-and-ret)))

    ; Create a sector caching block from a vector, an address and a disk
    (define (create-sector vect lba disk)
      (make-sector
        lba
        #f
        (make-mutex)
        vect
        0
        (lazy disk)
        #f
        #t))

    ; Create a disk from a device and a disk-type
    (define (create-disk dev type)
      (make-disk
        dev
        (make-mutex)
        0
        (make-table size: DISK-CACHE-MAX-SZ)
        (make-vector DISK-CACHE-MAX-SZ #f)
        type
        (make-table) ; background q for read-ahead
        ))

    ; Acquire a block from a disk. The mode
    ; ensures that the flushing of the disk is done appropriately.
    ; Prefered access are through `with-sector`
    (define (disk-acquire-block disk lba mode)
      (let* ((sector (disk-read-sector disk lba))
             (refs (sector-ref-count sector))
             (mut (sector-mut sector)))
        (mutex-lock! mut)
        (if (eq? MODE-READ-WRITE mode)
            (sector-dirty?-set! sector #t))
        (sector-has-chance?-set! sector #t)
        (sector-ref-count-set! sector (++ refs))
        (mutex-unlock! mut)
        sector))

    ; Release a block acquired
    ; Prefered access are through `with-sector`
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


    ; Prepare the flush of a sector so it can
    ; be flushed asynchronously
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

    ; Init the disk caching system.
    ; This must be called before the disks are used.
    (define (init-disks)
      (let* ((ide-devices (ide#list-devices))
             (zipped (zip disk-list ide-devices)))
        (set! disk-list (map (lambda (e)
                               (if (pair? e)
                                   (create-disk (cadr e) DISK-TYPE-IDE)
                                   e)) zipped))))

    ; For a disk, a block address, apply function
    ; fn on the sector vector. This is the best way
    ; of accessing the disks since this safely wraps
    ; the access to the diskss
    (define (with-sector dsk lba mode fn)
      (let* ((sect (disk-acquire-block dsk lba mode))
             (rslt (fn (sector-vect sect))))
        (disk-release-block sect)
        rslt))

    ))
