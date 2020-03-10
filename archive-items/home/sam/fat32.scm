; The mimosa project
(define-library (fat32)
    (import (disk) (gambit) (utils) (debug))
    (export 
      f-tests
      pack-BPB
      pack-entry
      entry-name
      read-file
      file-exists?
      list-directory
      write-file)
    (begin 

      ; (define-macro (define-struct-unpack name fields)
      ;               (let ((fill-struct (string-append "unpack-" (symbol->string name)))
      ;                     (make-struct (string-append "make-" (symbol->string name)))
      ;                     (vect-idx 0))
      ;                 (begin
      ;                   (list 'define (list (string->symbol fill-struct) 'vec)
      ;                         (cons 
      ;                           (string->symbol make-struct)
      ;                           (map (lambda (extract)
      ;                                  (let ((offset vect-idx)
      ;                                        (next-offset (+ vect-idx extract)))
      ;                                    (set! vect-idx next-offset) 
      ;                                    (if (<= extract 4)
      ;                                        (cons '+ (map (lambda (i)
      ;                                                        (list 'arithmetic-shift
      ;                                                              (list 'vector-ref 'vec (+ offset i))
      ;                                                              (* i 8)
      ;                                                              )) (iota extract)))

      ;                                        (list 'build-vector extract  
      ;                                              (list 'lambda (list 'i) (list 'vector-ref 'vec (list '+ offset 'i))))
      ;                                        )))
      ;                                fields))))))

      (define-macro (define-struct-pack name fields)
                    (let ((fill-struct (string-append "pack-" (symbol->string name)))
                          (make-struct (string-append "make-" (symbol->string name)))
                          (vect-idx 0))
                      (begin
                        (list 'define (list (string->symbol fill-struct) 'vec)
                              (cons 
                                (string->symbol make-struct)
                                (map (lambda (extract)
                                       (let ((offset vect-idx)
                                             (next-offset (+ vect-idx extract)))
                                         (set! vect-idx next-offset) 
                                         (if (<= extract 4)
                                             (cons '+ (map (lambda (i)
                                                             (list 'arithmetic-shift
                                                                   (list 'vector-ref 'vec (+ offset i))
                                                                   (* i 8)
                                                                   )) (iota extract)))

                                             (list 'build-vector extract  
                                                   (list 'lambda (list 'i) (list 'vector-ref 'vec (list '+ offset 'i))))
                                             )))
                                     fields))))))

      (define EOF 'EOF)
      (define FAT12-FS 0)
      (define FAT16-FS 1)
      (define FAT32-FS 2)
      (define FAT-32-EOF #x0FFFFFF8)
      (define FAT32-FIRST-CLUSTER 2)
      (define FAT-UNUSED-ENTRY #xE5)
      (define FAT-LAST-LONG-ENTRY #x40)
      (define FAT-CHARS-PER-LONG-NAME-ENTRY 13)
      (define FAT-NAME-MAX 1024)
      (define DT-UNKNOWN 0)
      (define DT-DIR 1)
      (define DT-REG 2)
      (define FAT-ATTR-READ-ONLY #x01)
      (define FAT-ATTR-HIDDEN #x02)
      (define FAT-ATTR-SYSTEM #x04)
      (define FAT-ATTR-VOLUME-ID #x08)
      (define FAT-ATTR-DIRECTORY #x10)
      (define FAT-ATTR-ARCHIVE #x20)
      (define FAT-ATTR-LONG-NAME
        (fxior FAT-ATTR-READ-ONLY
               FAT-ATTR-HIDDEN 
               FAT-ATTR-SYSTEM 
               FAT-ATTR-VOLUME-ID))
      (define FAT-NAME-LENGTH 11)
      (define FAT-DIR-ENTRY-SIZE 32)
      (define TYPE-FOLDER 'FOLDER)
      (define TYPE-FILE 'FILE)

      (define-structure fat-file
                        fs
                        first-clus
                        curr-clus
                        curr-section-start ; cluster of the current section
                        curr-section-length ; length of the current section
                        curr-section-pos ; position inside the section
                        pos
                        len
                        parent-first-clus
                        entry-pos
                        type
                        )

      (define-macro (folder? ff)
       `(eq? (fat-file-type ,ff) TYPE-FOLDER))

      (define-macro (file? ff)
       `(eq? (fat-file-type ,ff) TYPE-FILE))

      (define-structure BPB
                        jmp-boot
                        oem-name
                        bps
                        sec-per-cluster
                        reserved-sector-count
                        fats
                        root-entry-count
                        total-sectors-16
                        media
                        fat-size-16
                        sector-per-track
                        number-heads
                        hidden-sectors
                        total-sector-32
                        fat-size-32
                        ext-flags
                        fs-version
                        root-cluster
                        fs-info
                        boot-sector
                        reserved
                        drv-num
                        reserved1
                        boot-sig
                        vol-id
                        vol-lab
                        fs-type
                        )

      (define-macro (BPB-first-data-sector bpb)
                    `(let* ((root-entry-count (BPB-root-entry-count ,bpb))
                            (bps (BPB-bps ,bpb))
                            (lg2-bps (ilog2 bps))
                            (fatsz (BPB-fat-size-32 ,bpb))
                            (fats (BPB-fats ,bpb))
                            (rzvd (BPB-reserved-sector-count ,bpb))
                            (root-dir-sectors (s>> (- (+ (* root-entry-count FAT-DIR-ENTRY-SIZE) (s<< 1 lg2-bps)) 1) lg2-bps))
                            )
                       (+ root-dir-sectors rzvd (* fatsz fats))))

      (define-structure entry
                        name
                        attr
                        ntres
                        create-time-tenth
                        create-time
                        create-date
                        last-access-date
                        cluster-hi
                        last-write-time
                        last-write-date
                        cluster-lo
                        file-size)

      (define-structure lfn
                        ord
                        name1
                        attr
                        type
                        checksum
                        name2
                        cluster-lo
                        name3)

      ; Create the function pack-BPB that takes a vector and 
      ; initialises the BPB as you would in C (map the memory to the structure)
      (define-struct-pack BPB
                          (3 8 2 1 2 1 2 2 1 2 2 2 4 4 4 2 2 4 2 2 12 1 1 1 4 11 8)) 

      (define-struct-pack entry
                          (11 1 1 1 2 2 2 2 2 2 2 4))

      (define-struct-pack lfn (1 10 1 1 1 12 2 4))

      (define-structure filesystem
                        disk
                        bpb
                        lg2bps
                        lg2spc
                        first-data-sector
                        )

      (define fs-vector (make-vector 32 'NO-FS))

      (define (build-fs disk)
        (let ((bpb (disk-apply-sector disk 0 pack-BPB)))
          (make-filesystem
           disk
           bpb
           (ilog2 (BPB-bps bpb))
           (ilog2 (BPB-sec-per-cluster bpb))
           (BPB-first-data-sector bpb)
           )))

      (define (open-root-dir fs)
        (let*
          ((bpb (filesystem-bpb fs))
           (bps (BPB-bps bpb))
           (root-clus (BPB-root-cluster bpb))
           (sec-per-cluster (BPB-sec-per-cluster bpb)))
          (make-fat-file
            fs               ; fs
            root-clus        ; first cluster
            root-clus        ; curr cluster
            (BPB-first-data-sector bpb) ; section start
            (* bps sec-per-cluster) ; section len
            0 ; section pos
            0 ; abs pos
            0 ; len
            0 ; parent first clsu
            0 ; entry pos in parent entry
            TYPE-FOLDER ; type
            )))

      (define (next-cluster file)
        (let* ((fs (fat-file-fs file))
               (disk (filesystem-disk fs))
               (bpb (filesystem-bpb fs))
               (curr-clus (fat-file-curr-clus file))
               (offset (<< curr-clus 2))
               (rsvd (BPB-reserved-sector-count bpb))
               (lg2bps (filesystem-lg2bps fs))
               (lba (fx+ rsvd (>> offset lg2bps)))
               (offset (fxand offset (fxnot (<< (fxnot 0) lg2bps)))))
          (disk-apply-sector disk lba
                             (lambda (vect)
                               (let ((cluster (and #x0fffffff (uint32 vect offset))))
                                 (if (>= cluster FAT-32-EOF)
                                     EOF
                                     cluster))))))

        ; Set the next cluster and call the proper continuation (success or fail)
        ; with the file as an argument
        (define (set-next-cluster! file next-clus succ fail)
          (if (eq? next-clus EOF)
              (fail file)
              (let ((fs (fat-file-fs file))
                    (lg2spc (filesystem-lg2spec fs))
                    (fst-data-sect (filesystem-first-data-sector fs))
                    (lg2bps (filesystem-lg2bps fs)))
                (fat-file-curr-clus-set! file next-clus)
                (fat-file-curr-section-length-set! file (s<< 1 (+ lg2bps lg2spc)))
                (fat-file-curr-section-start-set! file (+ (s<< (- next-cluster 2)
                                                          lg2bps ) fst-data-sect))
                (fat-file-curr-section-pos-set! file 0)
                (succ file))))

      (define (read-bytes-aux! file fs buff idx count)
        (if (= 0 count) ; read all
            buff
            (let* ; fetch
              ; TODO deal with cluster boundary
              ; (if (>= (fat-file-curr-section-pos) (fat-file-curr-section-length file))
              ;  ( ((next-clus (next-cluster file)))
              ;   )
              ; Careful! A section is NOT a sector!
              ((cluster-start (fat-file-curr-section-start file))
               (disk (filesystem-disk fs))
               (cluster-pos (fat-file-curr-section-pos file))
               (pos (fat-file-pos file))
               (cluster-len (fat-file-curr-section-length file))
               (lg2spc (filesystem-lg2spc fs))
               (lg2bps (filesystem-lg2bps fs))
               (bps (s<< 1 lg2bps))
               (spc (s<< 1 lg2spc))
               (bpc (s<< 1 (+ lg2bps lg2spc))) ; bytes per cluster
               ; How many bytes left in cluster?
               (left-in-cluster (- cluster-len cluster-pos))
               ; How many bytes left in sector?
               (left-in-sector (- bps (modulo cluster-pos bps)))
               (sz (min count left-in-cluster left-in-sector))
               (lba (+ cluster-start (// cluster-pos bps)))
               (offset (modulo cluster-pos bps)))
              ; (debug-write sz)
              ; (debug-write left-in-cluster)
              ; (debug-write left-in-sector)
              (disk-apply-sector disk lba (lambda (vect)
                                            (for-each (lambda (i) (vector-set! 
                                                                    buff
                                                                    (+ i idx)
                                                                    (vector-ref 
                                                                      vect 
                                                                      (+ i offset)))) 
                                                      (iota sz))))
              (fat-file-curr-section-pos-set! file (+ sz cluster-pos))
              (fat-file-pos-set! file (+ sz pos))
              (read-bytes-aux! file fs buff (+ idx sz) (- count sz)))))

      (define (read-bytes! file count)
        (let ((fs (fat-file-fs file))
              (buff (make-vector count #x0)))
         (read-bytes-aux! file fs buff 0 (if (not (folder? file))
                                             (min count (- (fat-file-len file) (fat-file-pos file)))
                                             count)))) 

      (define (make-fat-time hours minute seconds)
        ; According to the FAT specification, the FAT time is set that way:
        ; Bits 15-11: the hour
        ; Bits 10-5 : the minutes
        ; Bits 4-0  : the half seconds
        (fxior 
          (<< (fxand #x1F hours) 11)
          (<< (fxand #x3F minutes) 5)
          (fxand #x1F (>> seconds 1))))

      ; Unpack a fat time entry and call
      ; the fn function with arguments hours, minutes and second
      (define (unpack-fat-time fat-time fn)
        ; Bits 15-11: the hour
        ; Bits 10-5 : the minutes
        ; Bits 4-0  : the half seconds
        (let ((hours (fxand #x1F (>> fat-time 11)))
              (minutes (fxand #x3F (>> fat-time 5)))
              (seconds (fxand #x1F fat-time)))
          (fn hours minutes seconds)))


      (define (make-fat-date year month day)
        ; According to the FAT specification, the FAT date is set that way:
        ; Bits 15-9: Year relative to 1980
        ; Bits 8-5: Month
        ; Bits 4-0: Day of month
        (let ((year ((max year 1980) 1980))) ; year below 1980 is incorrect
          (fxior
            (<< (fxand year #x7F) 9)
            (<< (fxand month #xF) 5)
            (fxand day #x1F))))

      ; Unpack a fat date and call the fn function
      ; with the arguments year month day of month
      (define (unpack-fat-date fate-date fn)
        ; According to the FAT specification, the FAT date is set that way:
        ; Bits 15-9: Year relative to 1980
        ; Bits 8-5: Month
        ; Bits 4-0: Day of month
        (fn 
          (fx+ 1980 (fxand #x7F (>> fat-date 9)))
          (fxand #xF (>> fat-date 5))
          (fxand #x1F fat-date)))

      (define (f-tests disk)
        (let ((fs (build-fs disk)))
            (read-bytes! (open-root-dir fs) 100)))
))
