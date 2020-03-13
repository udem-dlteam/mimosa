; The mimosa project
(define-library (fat32)
    (import (disk) (gambit) (utils) (debug))
    (export 
      f-tests
      f-test
      pack-BPB
      pack-entry
      entry-name
      read-file
      file-exists?
      list-directory
      write-file)
    (begin 
      (define ERR-EOF 'ERR-EOF)
      (define ERR-NO-MORE-ENTRIES 'ERR-NO-MORE-ENTRIES)

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

    (define-macro (define-c-struct name . fields)
                  (let* ((symbol-name (symbol->string name))
                         (fill-struct
                           (string->symbol (string-append "pack-" symbol-name)))
                         (make-struct
                           (string->symbol (string-append "make-" symbol-name)))
                         (width-struct
                           (string->symbol (string-append symbol-name "-width")))
                         (vect-idx 0)
                         (field-names (map car fields))
                         (field-width (map cadr fields))
                         )
                    `(begin
                       (define-structure ,name 
                                         ,@field-names)
                       (define (,width-struct) (+ ,@field-width))
                       (define (,fill-struct vec)
                         `(,make-struct
                            ;; TODO: map does not guarantee left-to-right calls to function
                            ,@(map (lambda (extract)
                                     (let ((offset vect-idx)
                                           (next-offset (+ vect-idx extract)))
                                       (set! vect-idx next-offset)
                                       (if (<= 0 extract 4)
                                           `(+ ,@(map (lambda (i)
                                                        `(arithmetic-shift
                                                           (vector-ref vec ,(+ offset i))
                                                           ,(* i 8)))
                                                      (iota extract)))

                                       `(build-vector
                                          ,(abs extract)
                                          (lambda (i)
                                            (vector-ref vec (+ ,offset i)))))))
                               field-width))))))

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
      (define DT-REG 3)
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

      (define-macro (folder? ff)
                    `(eq? (fat-file-type ,ff) TYPE-FOLDER))

      (define-macro (file? ff)
                    `(eq? (fat-file-type ,ff) TYPE-FILE))

      (define-macro (valid-entry? entry)
                    (let ((signal (gensym)))
                      `(let ((,signal (vector-ref (entry-name ,entry) 0)))
                         (not (or (fx= #x00 ,signal) (fx= #xE5 ,signal))))))

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


      (define-c-struct BPB
                        (jmp-boot 3)
                        (oem-name 8)
                        (bps 2)
                        (sec-per-cluster 1)
                        (reserved-sector-count 2)
                        (fats 1)
                        (root-entry-count 2)
                        (total-sectors-16 2)
                        (media 1)
                        (fat-size-16 2)
                        (sector-per-track 2)
                        (number-heads 2)
                        (hidden-sectors 4)
                        (total-sector-32 4)
                        (fat-size-32 4)
                        (ext-flags 2)
                        (fs-version 2)
                        (root-cluster 4)
                        (fs-info 2)
                        (boot-sector 2)
                        (reserved 12)
                        (drv-num 1)
                        (reserved1 1)
                        (boot-sig 1)
                        (vol-id 4) 
                        (vol-lab 11)
                        (fs-type 8)
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

      ; A FAT directory entry as defined by the FAT 32 spec
      (define-c-struct entry
                        (name 11)
                        (attr 1)
                        (ntres 1)
                        (create-time-tenth 1)
                        (create-time 2)
                        (create-date 2)
                        (last-access-date 2)
                        (cluster-hi 2)
                        (last-write-time 2)
                        (last-write-date 2)
                        (cluster-lo 2)
                        (file-size 4))

      (define-macro (is-lfn? entry)
                    (let ((attr (gensym)))
                      `(let ((,attr (entry-attr ,entry)))
                         (fx= FAT-ATTR-LONG-NAME ,attr))))

      ; A LFN structure as defined by the FAT 32 spec
      (define-c-struct lfn
                       (ord 1)
                       (name1 10)
                       (attr 1)
                       (type 1)
                       (checksum 1)
                       (name2 12)
                       (cluster-lo 2)
                       (name3 -4)
                       )



      ; A logical directory entry. The name is a string, and it abstracts
      ; away the fact that it comes from a long file name
      (define-structure logical-entry
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
                        file-size
                        lfn?
                        )


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
               (lg2bps (filesystem-lg2bps fs))
               (entries-per-sector (>> (<< 1 lg2bps) 2))
               (rsvd (BPB-reserved-sector-count bpb))
               (cluster (fat-file-curr-clus file))
               (lba (+ (// cluster entries-per-sector) rsvd))
               (offset (modulo cluster entries-per-sector)))
          (disk-apply-sector disk lba (lambda (vect)
                                        (let ((cluster (fxand #x0FFFFFFF (uint32 vect (<< offset 2)))))
                                          (if (>= cluster FAT-32-EOF)
                                              EOF
                                              cluster))))))

      ; Set the next cluster and call the proper continuation (success or fail)
      ; with the file as an argument
      (define (set-next-cluster! file next-clus succ fail)
        (if (eq? next-clus EOF)
            (fail ERR-EOF)
            (let* ((fs (fat-file-fs file))
                   (lg2spc (filesystem-lg2spc fs))
                   (fst-data-sect (filesystem-first-data-sector fs))
                   (lg2bps (filesystem-lg2bps fs)))
              (debug-write next-clus)
              (fat-file-curr-clus-set! file next-clus)
              (fat-file-curr-section-start-set!  file
                                                 (+ (s<< (- next-clus 2) lg2spc) fst-data-sect))
              (fat-file-curr-section-length-set! file (s<< 1 (+ lg2bps lg2spc)))
              (fat-file-curr-section-pos-set! file 0)
              (debug-write "SET NEXT CLUS CLUS POS")
              (debug-write (fat-file-curr-section-pos file))
              (succ file))))


      ; Search the entry with the file name name, and call
      ; the succ continuation with the logical entry or fail if the
      ; entry does not seem to exist.
      (define (search-entry file name succ fail)
        (search-entry-aux file name (list) succ (lambda (err-no) ;; todo create FNF
                                                  (fail 'FNF-ERR))))

      (define (search-entry-aux file name lfns succ fail)
        (read-entries file (lambda (e next)
                             (if (lfn? e)
                                 (search-entry-aux
                                   file
                                   name
                                   (cons e lfns)
                                   succ 
                                   fail)
                                 (let* ((logical-ents (entry-list->logical-entries (reverse (cons e lfns)))))
                                   (if (string=? name (logical-entry-name (car logical-ents)))
                                       (succ (car logical-ents))
                                       (search-entry-aux file name '() succ fail))
                                   ))) fail))

      ; Read entries as if the file is a FAT directory. 
      ; Takes the argument cont, a lambda that receives the entry and a function
      ; to read the next entry and fail a function that is called in case of failure
      (define (read-entries file succ fail)
        (let* ((cont (lambda () (read-entries file succ fail)))
               (vect (read-bytes! file entry-width fail))
               (e (pack-entry vect)) ; TODO only do one of them for efficiency
               (l (pack-lfn vect)))
          (cond ((is-lfn? e)
                 (succ l cont))
                ((valid-entry? e)
                 (succ e cont))
                (else
                  (fail ERR-NO-MORE-ENTRIES)))))

      (define (read-all-entries file)
        (read-entries
          file
          (lambda (entry next) (cons entry (next)))
          (lambda (err) (list))))

      (define (entry->logical-entry entry) ;; todo check if lfn
        (make-logical-entry
          (short-name->string (entry-name entry))
          (entry-attr entry)
          (entry-ntres entry)
          (entry-create-time-tenth entry)
          (entry-create-time entry)
          (entry-create-date entry)
          (entry-last-access-date entry)
          (entry-cluster-hi entry)
          (entry-last-write-time entry)
          (entry-last-write-date entry)
          (entry-cluster-lo entry)
          (entry-file-size entry)
          #f))

      (define (short-name->string sn-vect)
        (fold-right (lambda (c v)
                      (if (and (eq? c #\space) (> (string-length v) 0) (eq? #\. (string-ref v 0)))
                          v
                          (string-append (if (eq? c #\space)
                                             "."
                                             (string c)) v))) 
                    ""
                    (map (o char-downcase integer->char)
                         (vector->list sn-vect))))

      (define (lfn-name->string vect)
        (list->string (fold-right (lambda (c r)
                                    (if (or (eq? #\null c) (eq? #\xFF c))
                                        r
                                        (cons c r)))
                                  (list)
                                  (vector->list vect))))

      (define-macro (build-cluster logical)
                    `(fxior (<< (logical-entry-cluster-hi ,logical) 16)
                            (logical-entry-cluster-lo ,logical)))

      (define-macro (entry-type entry)
                    `(if (mask (entry-attr ,entry) FAT-ATTR-DIRECTORY)
                         TYPE-FOLDER
                         TYPE-FILE))

      (define-macro (logical-entry-type logical)
                    `(if (mask (logical-entry-attr ,logical) FAT-ATTR-DIRECTORY)
                         TYPE-FOLDER
                         TYPE-FILE))

      (define (logical-entry->file parent pos logical)
        (let* ((fs (fat-file-fs parent))
               (fds (filesystem-first-data-sector fs))
               (cluster (build-cluster logical))
               (lg2bps (filesystem-lg2bps fs))
               (lg2spc (filesystem-lg2spc fs)))
          (debug-write "Build")
          (debug-write cluster)
          (make-fat-file
            fs
            cluster
            cluster
            (+ fds (<< (- cluster 2) lg2spc)) 
            (<< 1 (+ lg2spc lg2bps))
            0
            0
            (logical-entry-file-size logical)
            (fat-file-first-clus parent)
            pos
            (logical-entry-type logical)
            )))

      (define (list-dir folder)
        (map logical-entry-name
             (entry-list->logical-entries (read-all-entries folder))))

      ; Follow a path (folder list) and return the last part
      ; described by the path
      (define (follow-path from to)
        (let ((l (length to)))
          (if (= l 0)
              from
              (search-entry
                from
                (car to)
                (lambda (f) 
                  (follow-path 
                    (logical-entry->file
                      from
                      (- (fat-file-pos from) entry-width)
                      f)
                    (cdr to)))
                (lambda (err) 'FNF-ERR)))))

      (define (extract-lfn-data lfn)
        (let ((text (list
                      (lfn-name1 lfn)
                      (lfn-name2 lfn)
                      (lfn-name3 lfn))))
          (fold-right
            string-append "" (map lfn-name->string
                                  (map (lambda (v) (vector-map integer->char v)) text)))))

      (define (entry-list->logical-entries entry-list)
        ; The fat spec garantess this structure:
        ; nth LFN
        ; nth-1 LFN
        ; ....
        ; 1st LFN
        ; entry
        (begin
          (fold-right (lambda (entry rest)
                        (if (lfn? entry)
                            (let* ((underlying-entry (car rest))
                                   (next-part (extract-lfn-data entry))
                                   (name (logical-entry-name underlying-entry)))
                              (logical-entry-name-set!
                                underlying-entry
                                (if (logical-entry-lfn? underlying-entry)
                                    (begin
                                      (logical-entry-lfn-set! underlying-entry #t)
                                      (string-append name next-part))
                                    next-part))
                              rest)
                            ; not LFN, leave it as is
                            (cons (entry->logical-entry entry) rest))) 
                      (list) entry-list)))

      (define (read-bytes-aux! file fs buff idx count fail)
        (if (> count 0) 
            (let* ((succ (lambda (file)
                           (let* ((cluster-start (fat-file-curr-section-start file))
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
                             (disk-apply-sector
                               disk
                               lba
                               (lambda (vect)
                                 (for-each (lambda (i) (vector-set! buff
                                                                    (+ i idx)
                                                                    (vector-ref vect 
                                                                                (+ i offset)))) (iota sz))))
                             (fat-file-curr-section-pos-set! file (+ sz cluster-pos))
                             (fat-file-pos-set! file (+ sz pos))
                             (read-bytes-aux! file
                                              fs
                                              buff
                                              (+ idx sz)
                                              (- count sz)
                                              fail))))
                   (cluster-pos (fat-file-curr-section-pos file))
                   (cluster-len (fat-file-curr-section-length file)))
              (if (>= cluster-pos cluster-len)
                  ; out of bounds of the sector, go fetch the next one
                  (set-next-cluster! file (next-cluster file) succ fail)
                  (succ file)))
            buff))

      (define (read-bytes! file count fail)
        (let ((fs (fat-file-fs file))
              (buff (make-vector count #x0)))
          (read-bytes-aux! file
                           fs
                           buff
                           0
                           (if (not (folder? file))
                               (min count (- (fat-file-len file) (fat-file-pos file)))
                               count)
                           fail 
                           ))) 

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

      (define p (list "home" "sam"))

      (define (f-test disk)
        (open-root-dir (build-fs disk)))

      (define (f-tests disk)
        (let ((fs (build-fs disk)))
          ; (search-entry (open-root-dir fs) "BOOT.SYS" ID ID)
          (list-dir (follow-path (open-root-dir fs) p))))

      ))
