; The mimosa project
(define-library (fat32)
    (import (disk) (gambit) (utils))
    (export 
      fat32-tests
      fill-BPB
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

      (define-struct fat-file
                     first-clus
                     curr-clus
                     curr-section-start
                     curr-section-length
                     curr-section-pos
                     len
                     parent-first-clus
                     entry-pos
                     )

      (define-structure BPB
                        jmp-boot
                        oem-name
                        bps
                        sec-per-cluster
                        reserved-sector-count
                        fats
                        root-entry-cluster
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

      (define-structure entry
                        name
                        attr
                        ntres
                        create-time
                        create-date
                        last-access-date
                        cluster-hi
                        last-write-time
                        last-write-date
                        cluster-lo
                        file-size)

      (define-struct lfn
                     ord
                     name1
                     attr
                     type
                     checksum
                     name2
                     cluster-lo
                     name3)

      ; Create the function fill-BPB that takes a vector and 
      ; initialises the BPB as you would in C (map the memory to the structure)
      (define-struct-pack BPB
                          (3 8 2 1 2 1 2 2 1 2 2 2 4 4 4 2 2 4 2 2 12 1 1 1 4 11 8)) 

      (define-struct-pack entry
                          (11 1 1 1 2 2 2 2 2 2 2 4))

      (define-struct-pack lfn (1 10 1 1 1 12 2 4))

      (define-structure fs
                        disk
                        bpb
                        )
        
      (define fs-vector (make-vector 32 'NO-FS))

      (define (build-fs disk)
       (let ((bpb (disk-apply-sector fill-BPB)))
         (make-fs disk bpb)
         ))

      (define (open-root-dir fs)
        TODO 

       
       
       
       
       )

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


      (define (fat32-tests disk)
        (let ((fs (build-fs disk)))
          (begin
           (open-root-dir fs)
            )))
      ))
