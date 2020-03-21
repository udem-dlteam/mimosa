; The mimosa project
(define-library (fat32)
    (import
      (errors)
      (disk)
      (gambit)
      (utils)
      (debug))
    (export 
      TYPE-FOLDER
      TYPE-FILE
      pack-BPB
      pack-entry
      entry-name
      file-read!
      file-write!
      create-fat-file
      open-fat-file
      fat-file-exists?
      list-directory
      simplify-path
      mount-partitions
      filesystem-list
      look-for-n-available-entries!
      make-lfns
      write-file
      a-tests
      b-tests
      c-tests
      d-tests
      f-tests
      f-test)
    (begin 
      ; --------------------------------------------------
      ; --------------------------------------------------
      ; --------------------------------------------------
      ;         Definitions, constants and globals
      ; --------------------------------------------------
      ; --------------------------------------------------
      ; --------------------------------------------------
      (define FAT-LINK-MASK #x0FFFFFFF)
      (define FILE-MODE-READ (arithmetic-shift 1 0))
      (define FILE-MODE-TRUNC (arithmetic-shift 1 1))
      (define FILE-MODE-APPEND (arithmetic-shift 1 2))
      (define FILE-MODE-PLUS (arithmetic-shift 1 3))
      (define FILE-MODE-BINARY (arithmetic-shift 1 4))
      (define FAT32-ENTRY-WIDTH 4)
      (define (mode-requires-existence? mode)
        (mask mode FILE-MODE-READ))
      (define EOF 'EOF)
      (define FAT12-FS 0)
      (define FAT16-FS 1)
      (define FAT32-FS 2)
      (define FAT-32-EOF #x0FFFFFF8)
      (define FAT32-FIRST-CLUSTER 2)
      (define FAT-UNUSED-ENTRY #xE5)
      (define FAT-LAST-LONG-ENTRY #x40)
      (define FAT-SHORT-FILE-NAME-MAX-LEN 11)
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
      (define FAT-32-FS-TYPE "FAT32")
      (define TYPE-FILE 'FILE)
      (define filesystem-list (list))
      ; --------------------------------------------------
      ; --------------------------------------------------
      ; --------------------------------------------------
      ;                      MACROS
      ; --------------------------------------------------
      ; --------------------------------------------------
      ; --------------------------------------------------
      (define-macro (define-c-struct name . fields)
                    (let* ((symbol-name (symbol->string name))
                           (pack-struct
                             (string->symbol (string-append "pack-" symbol-name)))
                           (unpack-struct
                             (string->symbol (string-append "unpack-" symbol-name)))
                           (make-struct
                             (string->symbol (string-append "make-" symbol-name)))
                           (width-struct
                             (string->symbol (string-append symbol-name "-width")))
                           (field-accessor (lambda (field)
                                             (string->symbol 
                                               (string-append
                                                 symbol-name
                                                 "-"
                                                 (symbol->string field)))))
                           (flatten (lambda (lst)
                                      (if (pair? lst)
                                          (fold-right (lambda (e r)
                                                        (if (pair? e)
                                                            (append (flatten (car e)) (flatten (cdr e)) r)
                                                            (cons e r)))
                                                      (list) lst)
                                          (list lst))))
                           (vect-idx 0)
                           (field-names (map car fields))
                           (field-width (map cadr fields)))
                      `(begin
                         (define-structure ,name 
                                           ,@field-names)
                         (define ,width-struct (+ ,@field-width))
                         (define (,unpack-struct strc vec base-offset)
                           ; fields that are shorter than 4 are packed into an int
                           ; fields that are longer than 4 are packed directly into a vect
                           ; fields that have a negative length are forced into a vect
                           (begin
                             ,@(let ((offset 0))
                                 (map (lambda (f-index)
                                        (let ((f (list-ref field-names f-index))
                                              (w (list-ref field-width f-index)))
                                          ; copy the vector into the target vector
                                          (if (or (< w 0) (> w 4))
                                              `(begin ,@(map 
                                                          (lambda (i)
                                                            (set! offset (+ offset 1))
                                                            `(vector-set!
                                                               vec
                                                               (+ ,(- offset 1) base-offset)
                                                               (vector-ref (,(field-accessor f) strc) ,i)))
                                                          (iota (abs w))))
                                              `(begin ,@(map (lambda (i)
                                                               (set! offset (+ offset 1))
                                                               `(vector-set!
                                                                  vec
                                                                  (+ ,(- offset 1) base-offset)
                                                                  (bitwise-and #xFF (arithmetic-shift
                                                                                      (,(field-accessor f) strc)
                                                                                      ,(- 0 (* i 8)))))
                                                               )
                                                             (iota w)))
                                              ))) (iota (length field-names))))
                             vec))
                         (define (,pack-struct vec)
                           (,make-struct
                             ;; TODO: map does not guarantee left-to-right calls to function
                             ; fields that are shorter than 4 are packed into an int
                             ; fields that are longer than 4 are packed directly into a vect
                             ; fields that have a negative length are forced into a vect
                             ,@(map (lambda (extract)
                                      (let ((offset vect-idx)
                                            (next-offset (+ vect-idx (abs extract))))
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
      (define-macro (folder? ff)
                    `(eq? (fat-file-type ,ff) TYPE-FOLDER))

      (define-macro (file? ff)
                    `(eq? (fat-file-type ,ff) TYPE-FILE))

      (define-macro (valid-entry? entry)
                    (let ((signal (gensym)))
                      `(let ((,signal (vector-ref (entry-name ,entry) 1)))
                         (not (or (fx= #x00 ,signal) (fx= #xE5 ,signal))))))

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

      (define-macro (BPB-entries-per-sector bpb)
                    `(// (BPB-bps bpb) FAT32-ENTRY-WIDTH))

      (define-macro (is-lfn? entry)
                    (let ((attr (gensym)))
                      `(let ((,attr (entry-attr ,entry)))
                         (fx= FAT-ATTR-LONG-NAME ,attr))))

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

      (define-macro (relative-move-function file mvmt fn)
                    `(,(if (eq? fn '+)
                           `simulate-forward-move
                           `simulate-backward-move)
                       ,file
                       ,mvmt
                       (lambda (new-cluster cluster-pos section-start)
                         (fat-file-curr-clus-set! ,file new-cluster)
                         (fat-file-curr-section-start-set! ,file section-start)
                         (fat-file-curr-section-pos-set! ,file cluster-pos)
                         (fat-file-pos-set! ,file (,fn (fat-file-pos ,file) ,mvmt)))))

      (define-macro (entry-deleted? entry)
                    `(fx=
                       (if (lfn? ,entry)
                           (lfn-ord ,entry)
                           (vector-ref (entry-name ,entry) 0))
                       #xE5))

      (define-macro (entry-empty? entry)
                    `(fx=
                       (if (lfn? ,entry)
                           (lfn-ord ,entry)
                           (vector-ref (entry-name ,entry) 0))
                       #x00))

      ; --------------------------------------------------
      ; --------------------------------------------------
      ; --------------------------------------------------
      ;                  STRUCTURES
      ; --------------------------------------------------
      ; --------------------------------------------------
      ; --------------------------------------------------
      (define-structure fat-file
                        fs
                        first-clus
                        curr-clus
                        curr-section-start ; cluster of the current section
                        curr-section-length ; length of the current section
                        curr-section-pos ; position inside the section
                        pos
                        len
                        entry-cluster
                        entry-offset
                        type
                        mode
                        entry
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
                       (fs-type 8))

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

      (define-c-struct lfn
                       (ord 1)
                       (name1 10)
                       (attr 1)
                       (type 1)
                       (checksum 1)
                       (name2 12)
                       (cluster-lo 2)
                       (name3 -4))

      ; A FAT directory entry as defined by the FAT 32 spec
      ; A LFN structure as defined by the FAT 32 spec

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
                        cache-write-mut
                        fat-cache ; does not need to be a hash table... a vector would have been good
                        )

      (define-structure cache-link
                        next
                        prev)

      ; --------------------------------------------------
      ; --------------------------------------------------
      ; --------------------------------------------------
      ;                  Utils for FAT
      ; --------------------------------------------------
      ; --------------------------------------------------
      ; --------------------------------------------------

      (define (safe-substring s start end)
        (let ((l (string-length s)))
          (if (>= start l)
              ""
              (substring s start (min l end)))))

      (define (checksum name)
        (fold
          (lambda (e r)
            (bitwise-and
              #xFF
              (+
                (if (odd? r)
                    #x80
                    0) 
                e 
                (>> r 1)))) 
          0 
          (vector->list name)))

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

      (define (extract-lfn-data lfn)
        (let ((text (list
                      (lfn-name1 lfn)
                      (lfn-name2 lfn)
                      (lfn-name3 lfn))))
          (fold-right
            string-append "" (map lfn-name->string
                                  (map (lambda (v) (vector-map integer->char v)) text)))))

      ; Simplify a path and return it as a list of folder, possibly with a file at the end,
      ; that need to be traversed to get to the end of the path. Relative movements
      ; are taken into account and applied to simplify the path
      (define (simplify-path path)
        (let ((split (split-string #\/ path)))
          (fold-right
            (lambda (c r)
              (if (and (> (length r) 0) (string=? ".." (car r)))
                  (cdr r)
                  (cons c r))) 
            (list) split)))

      ; Take a string that represents a file name and convert it into a 
      ; vector that is suited for the name field of a directory entry.
      (define (string->short-name-vect str)
        (let ((split (split-string #\. str)))
          (let ((v (make-vector 11 (char->integer #\space)))
                (name (string-upcase (car split)))
                (ext (string-upcase (cadr split))))
            (vector-copy! v 0 (string->u8vector name) 0 (min 8 (string-length name)))
            (vector-copy! v 8 (string->u8vector ext) 0 (min 3 (string-length ext)))
            v)))

      (define (remove-spaces v)
        (fold-right
          (lambda (e r)
            (if (eq? e #\space)
                r
                (cons e r)))
          (list)
          (vector->list v)))

      ; Take a short name, from a directory entry, and make it into a file name.
      ; this method will add the dot for the extension of the file
      (define (short-name->string vect)
        (let* ((v (vector-map integer->char vect))
               (name (subvector v 0 8))
               (ext (subvector v 8 11)))
          (let ((short-name (remove-spaces name))
                (short-ext (remove-spaces ext)))
            (if (= 0 (length short-ext))
                (list->string short-name)
                (list->string (append short-name short-ext))))))

      (define (lfn-name->string vect)
        (list->string (fold-right (lambda (c r)
                                    (if (or (eq? #\null c) (eq? #\xFF c))
                                        r
                                        (cons c r)))
                                  (list)
                                  (vector->list vect))))

      ; --------------------------------------------------
      ; --------------------------------------------------
      ; --------------------------------------------------
      ;                    Driver init
      ; --------------------------------------------------
      ; --------------------------------------------------
      ; --------------------------------------------------

      (define (make-fat-cache dsk bpb)
        (let* ((fat-sz (BPB-fat-size-32 bpb))
               (rsvd (BPB-reserved-sector-count bpb))
               (entries-per-sector (BPB-entries-per-sector bpb))
               (no-of-entries (* entries-per-sector fat-sz))
               (sectors (map (lambda (s)
                               (disk-apply-sector dsk (+ s rsvd) MRO ID)) (iota fat-sz)))
               (cache (make-table)))
          ; Cache is now filled in
          (for-each (lambda (l)
                      (let* ((offset (* 4 (modulo l entries-per-sector)))
                             (block (// l entries-per-sector))
                             (value (uint32 (list-ref sectors block) offset))) 
                        (table-set!
                          cache
                          l
                          (make-cache-link
                            (fxand #x0FFFFFFF value)
                            0))))
                    (iota no-of-entries))
          (for-each (lambda (l)
                      (let* ((link (table-ref cache l))
                             (n (cache-link-next link)))
                        (if (fx< n FAT-32-EOF)
                            (cache-link-prev-set! (table-ref cache n) l))))
                    (iota no-of-entries)) 
          cache))

      (define (build-fs dsk)
        (let ((bpb (disk-apply-sector dsk 0 MRO pack-BPB)))
          (make-filesystem
            dsk
            bpb
            (ilog2 (BPB-bps bpb))
            (ilog2 (BPB-sec-per-cluster bpb))
            (BPB-first-data-sector bpb)
            (make-mutex) ; cache write mutex
            (make-fat-cache dsk bpb)))) 

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
            0 ; parent first clus
            0 ; entry pos in parent entry
            TYPE-FOLDER ; type
            FILE-MODE-READ
            0)))

      (define (mount-partition disk default)
        (if (disk-absent? disk)
            default
            (let* ((fs (build-fs disk))
                   (bpb (filesystem-bpb fs))
                   (fs-type (u8vector->string (BPB-fs-type bpb))))
              (if (string=? 
                    (substring fs-type 0 (string-length FAT-32-FS-TYPE))
                    FAT-32-FS-TYPE)
                  fs
                  default)))) 

      (define (mount-partitions disk-list)
        (let ((filesystems (fold (lambda (e r)
                                   (let ((fs (mount-partition e #f)))
                                     (if fs (cons fs r) r)))
                                 (list)
                                 disk-list)))
          (set! filesystem-list filesystems)
          filesystems))

      ; --------------------------------------------------
      ; --------------------------------------------------
      ; --------------------------------------------------
      ;              Movements of the cursor
      ; --------------------------------------------------
      ; --------------------------------------------------
      ; --------------------------------------------------

      (define (get-cached-cluster file)
        (let* ((fs (fat-file-fs file))
               (cache (filesystem-fat-cache fs)))
          (table-ref cache (fat-file-curr-clus file))))

      (define (next-cluster file)
        (let* ((link (get-cached-cluster file))
               (n (cache-link-next link)))
          (if (fx>= n FAT-32-EOF)
              EOF
              n)))

      (define (previous-cluster file)
        (let* ((link (get-cached-cluster file))
               (n (cache-link-prev link)))
          (if (fx>= n FAT-32-EOF) EOF n)))

      (define (fat-reset-cursor! file)
        (let* ((fs (fat-file-fs file))
               (lg2spc (filesystem-lg2spc fs))
               (lg2bps (filesystem-lg2bps))
               (first-cluster (fat-file-first-clus f)))
          (fat-file-curr-clus-set! file first-cluster)
          (fat-file-curr-section-start-set!
            file
            (+ (<< (- first-clus 2) lg2spc) (filesystem-first-data-sector fs)))
          (fat-file-curr-section-length-set! file (<< 1 (+ lg2bps lg2spc)))
          (fat-file-curr-section-pos-set! file 0)
          (fat-file-pos-set! file 0)))

      (define (file-set-cursor-absolute! file pos)
        (let ((current-pos (fat-file-pos file)))
          (cond ((= current-pos pos)
                 #t)
                ((= 0 pos)
                 (file-reset-cursor! file))
                (else
                  (let ((delta (- pos current-pos)))
                    (file-move-cursor! file delta))))))

      (define (traverse-fat-chain relative-to mvmt chain)
        (if (= mvmt 0)
            relative-to
            (let ((link (table-ref chain relative-to)))
              (if (> mvmt 0)
                  (traverse-fat-chain (cache-link-next link) (-- mvmt) chain)
                  (traverse-fat-chain (cache-link-prev link) (++ mvmt) chain)))))

      (define-macro (simulate-direction-move direction f rewind c)
                    `(let* ((section-pos (fat-file-curr-section-pos file))
                            (section-len (fat-file-curr-section-length file))
                            (diff (max 0 (- rewind section-pos))))
                       (if (> diff 0)
                           ; need to go over boundary
                           (let* ((fs (fat-file-fs file))
                                  (lg2spc (filesystem-lg2spc fs))
                                  (lg2bps (filesystem-lg2bps fs))
                                  (lg2bpc (+ lg2spc lg2bps))
                                  (start-cluster (fat-file-curr-clus file))
                                  (cluster-mvmt (>> diff lg2bpc))
                                  ,(if (eq? direction 'BACKWARDS)
                                       `(cluster-leftover (- (fat-file-curr-section-length file)
                                                             (modulo diff (<< 1 lg2bpc))))    
                                       `(cluster-leftover (modulo diff (<< 1 lg2bpc))))
                                  ,(if (eq? direction 'BACKWARDS)
                                       `(new-cluster (traverse-fat-chain
                                                       start-cluster
                                                       (- -1 cluster-mvmt)
                                                       (filesystem-fat-cache fs)
                                                       ))   
                                       `(new-cluster (traverse-fat-chain
                                                       start-cluster
                                                       (+ 1 cluster-mvmt)
                                                       (filesystem-fat-cache fs)))))
                             (c
                               new-cluster
                               cluster-leftover
                               (+ (<< (- new-cluster 2) lg2spc)
                                  (filesystem-first-data-sector fs))))
                           (c
                             (fat-file-curr-clus file)
                             (,(if (eq? direction 'BACKWARDS) `- `+) section-pos rewind)
                             (fat-file-curr-section-start file)))))

      (define (simulate-backward-move file rewind c)
        (simulate-direction-move 'BACKWARDS file rewind c))

      (define (simulate-forward-move file wind c)
        (simulate-direction-move 'FORWARDS file wind c))

      (define (file-move-cursor-backward! file rewind)
        (relative-move-function file rewind -))

      (define (file-move-cursor-forward! file wind)
        (relative-move-function file wind +))

      (define (simulate-move file delta c)
        (if (> delta 0)
            (simulate-forward-move file delta c)
            (simulate-backward-move file (abs delta) c)))

      (define (file-move-cursor! file delta)
        (if (> delta 0)
            (file-move-cursor-forward! file delta)
            (file-move-cursor-backward! file (abs delta))))

      ; Set the next cluster, move to it and return the FAT array
      ; with the file as an argument
      (define (set-next-cluster! file next-clus)
        (if (eq? next-clus EOF)
            ERR-ARGS
            (let* ((fs (fat-file-fs file))
                   (cache (filesystem-fat-cache fs))
                   (disk (filesystem-disk fs))
                   (bpb (filesystem-bpb fs))
                   (lg2bps (filesystem-lg2bps fs))
                   (entries-per-sector (>> (<< 1 lg2bps) 2))
                   (rsvd (BPB-reserved-sector-count bpb))
                   (cluster (fat-file-curr-clus file))
                   (lba (+ (// cluster entries-per-sector) rsvd))
                   (offset (modulo cluster entries-per-sector)))
              (cache-link-next-set! 
                (table-ref cache cluster)
                next-clus)
              (disk-apply-sector
                disk
                lba
                MRW
                (lambda (vect) 
                  (wint32 vect (<< offset 2) next-clus)
                  next-clus)))))

      ; Move to the next cluster and call the proper continuation (success or fail)
      ; with the file as an argument
      (define (move-next-cluster! file next-clus succ fail)
        (if (eq? next-clus EOF)
            (fail ERR-EOF)
            (let* ((fs (fat-file-fs file))
                   (lg2spc (filesystem-lg2spc fs))
                   (fst-data-sect (filesystem-first-data-sector fs))
                   (lg2bps (filesystem-lg2bps fs)))
              (fat-file-curr-clus-set! file next-clus)
              (fat-file-curr-section-start-set!
                file
                (+ (s<< (- next-clus 2) lg2spc) fst-data-sect))
              (fat-file-curr-section-length-set! file (s<< 1 (+ lg2bps lg2spc)))
              (fat-file-curr-section-pos-set! file 0)
              (succ file))))

      ; --------------------------------------------------
      ; --------------------------------------------------
      ; --------------------------------------------------
      ;                Search / Traversal 
      ; --------------------------------------------------
      ; --------------------------------------------------
      ; --------------------------------------------------

      ; Search the entry with the file name name, and call
      ; the succ continuation with the logical entry or fail if the
      ; entry does not seem to exist.
      (define (search-entry file name succ fail)
        (search-entry-aux
          file
          name
          (list)
          succ
          (lambda (err-no) (fail ERR-FNF))
          ))

      (define (search-entry-aux file name lfns succ fail)
        (read-entries
          file 
          (lambda (e next)
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
                  )))
          fail))

      ; Read entries as if the file is a FAT directory. 
      ; Takes the argument cont, a lambda that receives the entry and a function
      ; to read the next entry and fail a function that is called in case of failure
      (define (read-entries file succ fail)
        (let* ((cont (lambda () (read-entries file succ fail)))
               (vect (read-bytes! file entry-width fail)))
          (if (vector? vect) ; if an error occurs, it's not gonna be a vector
              (let ((e (pack-entry vect))
                    (l (pack-lfn vect)))
                (cond ((is-lfn? e)
                       (succ l cont))
                      ((valid-entry? e)
                       (succ e cont))
                      (else (fail ERR-NO-MORE-ENTRIES))))
              vect)))

      (define (read-all-entries file)
        (read-entries
          file
          (lambda (entry next) (cons entry (next)))
          (lambda (err) (list))))

      (define (entry->logical-entry entry)
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

      (define (string->lfn-vector name len)
        (let* ((half (// len 2))
               (v (make-vector len 0))
               (l (string-length name)))
          (for-each (lambda (i)
                      (cond ((= i l)
                             (begin
                               (vector-set! v (<< i 1) 0)
                               (vector-set! v (++ (<< i 1)) 0)))
                            ((> i l)
                             (begin
                               (vector-set! v (<< i 1) #xFF)
                               (vector-set! v (++ (<< i 1)) #xFF)))
                            (else
                              (vector-set!
                                v
                                (<< i 1)
                                (char->integer (string-ref name i))))))
                    (iota half)) v))

      ; Return an absolute position in the 'folder' where
      ; 'n' consecutive entries are available. The success continuation
      ; takes the absolute position as a single parameter.
      ; The fail continuation takes two parameters, the first one being
      ; the error code and the second one being an absolute position
      ; that we can start writing to. This can happen if
      ; there are available entries to write to at the end of a file,
      ; but the file boundary does not allow more entries to be considered
      ; part of the file. The boundary can simply be extended (by writing to the file)
      ; to allow 
      ; This function resets the file cursor to 0 at the start.
      (define (look-for-n-available-entries! folder n succ fail)
        (let ((ignored (file-set-cursor-absolute! folder 0))
              (conseq 0))
          (read-entries
            folder
            (lambda (e next)
              (if (entry-deleted? e)
                  (if (= (++ conseq) n)
                      ; Done: find the position
                      (succ (- (fat-file-pos folder) (* n entry-width)))
                      (begin
                        (set! conseq (++ conseq))
                        (next)))
                  ; Not deleted
                  (if (entry-empty? e)
                      ; All the rest is free, we can use it
                      (succ (- (fat-file-pos folder) (* (++ conseq) entry-width)))
                      ; Reset
                      (begin
                        (set! conseq 0)
                        (next)))))
            (lambda (err)
              (fail
                err
                (cond ((eq? ERR-EOF err) ; we did not move the cursor
                       (- (fat-file-pos folder)
                          (* conseq entry-width)))
                      ((eq? ERR-NO-MORE-ENTRIES err) ; the cursor moved before we read 0
                       (- (fat-file-pos folder)
                          (* (++ conseq) entry-width)))
                      (else -1)))))))

      ; Create a collection of long file name starting at ordinal 'ith'
      ; for the remaining name part 'name' and the checksum 'chksm'
      (define (make-lfns name ith chksm)
        (let ((l (string-length name)))
          (if (= l 0)
              (list)
              ; Copy into a LFN and truncate the rest of the string
              (cons 
                (make-lfn
                  (fxior ith (if (<= l FAT-CHARS-PER-LONG-NAME-ENTRY)
                                 FAT-LAST-LONG-ENTRY
                                 0))
                  (string->lfn-vector (safe-substring name 0 5) 10)
                  FAT-ATTR-LONG-NAME
                  0
                  chksm
                  (string->lfn-vector (safe-substring name 5 11) 12)
                  0
                  (string->lfn-vector (safe-substring name 11 13) 4))
                (make-lfns (safe-substring name 13 l) (+ ith 1) chksm)))))

      ; Create the root entry for a file
      (define (make-root-entry file)
        (let ((le (fat-file-entry file))) 
          (make-entry
            (string->short-name-vect (logical-entry-name le))
            (logical-entry-attr le) ; TODO: upate
            (logical-entry-ntres le) ; does not change
            (logical-entry-create-time-tenth le) ; does not change
            (logical-entry-create-time le) ; does not change
            (logical-entry-create-date le) ; does not change
            (logical-entry-last-access-date le) ; TODO: update
            (logical-entry-cluster-hi le) ; does not change
            (logical-entry-last-write-time le) ; TODO: update
            (logical-entry-last-write-date le) ; TODO: update
            (logical-entry-cluster-lo le) ; does not change
            (fat-file-len file) ; get the latest information
            )))

      ; Returns a list in the following order:
      ; root entry
      ; LFN 1
      ; ...
      ; nth LFN
      (define (fat-file->entries file)
        (let* ((underlying-entry (fat-file-entry file))
               (name (logical-entry-name underlying-entry))
               (name-l (string-length name))
               (requires-lfn? (> name-l FAT-SHORT-FILE-NAME-MAX-LEN))
               (hdr (make-root-entry file)))
          (cons 
            hdr 
            (if requires-lfn?
                (make-lfns name 1 (checksum (entry-name hdr)))
                (list)))))

      ; Create a list of subitems in a directory
      (define (list-directory folder)
        (let ((all (read-all-entries folder)))
          (map logical-entry-name (entry-list->logical-entries all))))

      ; Create a file after we just read the entry of said file
      ; The cursor is simulated backwards so we get the cluster of the
      ; parent correctly
      (define (logical-entry->file parent pos logical)
        (let* ((fs (fat-file-fs parent))
               (fds (filesystem-first-data-sector fs))
               (cluster (build-cluster logical))
               (lg2bps (filesystem-lg2bps fs))
               (lg2spc (filesystem-lg2spc fs)))
          (simulate-move
            parent
            (- entry-width)
            (lambda (parent-cluster parent-section-pos section-start)
              (make-fat-file
                fs
                cluster
                cluster
                (+ fds (<< (- cluster 2) lg2spc)) 
                (<< 1 (+ lg2spc lg2bps))
                0
                0
                (logical-entry-file-size logical)
                parent-cluster 
                parent-section-pos
                (logical-entry-type logical)
                FILE-MODE-READ
                logical)))))

      ; Follow a path (folder list) and return the last part
      ; described by the path
      (define (follow-path from to succ fail)
        (let ((l (length to)))
          (if (= l 0)
              (succ from)
              (search-entry
                from ; we search from there
                (car to) ; next child
                (lambda (f) ; on success
                  (follow-path 
                    (logical-entry->file
                      from
                      (- (fat-file-pos from) entry-width)
                      f)
                    (cdr to)
                    succ
                    fail))
                (lambda (err)
                  (if (= l 1)
                      (fail err)
                      (fail ERR-PATH))) ; on failure
                ))))

      ; Take a list of entries from a directory, and converts 
      ; it into a list of logical entries. It will merge
      ; long file names entries into the file. It does not check
      ; for checksum validity.
      (define (entry-list->logical-entries entry-list)
        ; The fat spec garantess this structure:
        ; nth LFN
        ; nth-1 LFN
        ; ....
        ; 1st LFN
        ; entry
        (fold-right
          (lambda (entry rest)
            (if (lfn? entry)
                ; Append the name
                (let* ((underlying-entry (car rest))
                       (next-part (extract-lfn-data entry))
                       (name (logical-entry-name underlying-entry)))
                  ; If it was not LFN before, we erase
                  (logical-entry-name-set!
                    underlying-entry
                    (string-append
                      (if (logical-entry-lfn? underlying-entry)
                          name
                          "")
                      next-part))
                  (if (not (logical-entry-lfn? underlying-entry))
                      (logical-entry-lfn?-set! underlying-entry #t))
                  rest)
                ; not LFN, leave it as is
                (cons (entry->logical-entry entry) rest))) 
          (list)
          entry-list))

      (define (find-first-free-cluster-aux cache index len succ fail)
        (if (< index len)
            (let* ((link (table-ref cache index))
                   (link-next (cache-link-next link)))
              (if (fx= link-next 0)
                  (succ index link)
                  (find-first-free-cluster-aux cache (++ index) len succ fail)))
            (fail ERR-EOF)
       ))

      ; Find the first free cluster in the FS file table.
      ; the fail continuation is called in case of error,
      ; the succ continuation is called with the cluster number and the link
      ; when it found one. 
      ; The link will be marked with an EOF tag to prevent race conditions.
      (define (find-first-free-cluster fs succ fail)
        (let* ((cache (filesystem-fat-cache fs))
               (cache-mut (filesystem-cache-write-mut fs))
               (l (table-length cache)))
          (mutex-lock! cache-mut); acquire
          (find-first-free-cluster-aux
            cache
            0
            l
            ; free the mutex when the continuation is called
            (lambda (cluster link)
              (cache-link-next-set! link FAT-32-EOF)
              (update-cluster-link-on-disk fs cluster link)
              (mutex-unlock! cache-mut)
              (succ cluster link))
            (lambda (err)
              (mutex-unlock! cache-mut)
              (fail err)))))

      ; --------------------------------------------------
      ; --------------------------------------------------
      ; --------------------------------------------------
      ;                Reading / Writing 
      ; --------------------------------------------------
      ; --------------------------------------------------
      ; --------------------------------------------------

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
                                  (left-in-file (if (folder? file)
                                                    (++ count)
                                                    (- (fat-file-len file) pos)))
                                  (sz (min count left-in-cluster left-in-sector left-in-file))
                                  (lba (+ cluster-start (// cluster-pos bps)))
                                  (offset (modulo cluster-pos bps)))
                             (disk-apply-sector
                               disk
                               lba
                               MRO
                               (lambda (vect)
                                 (for-each
                                   (lambda (i) (vector-set!
                                                 buff
                                                 (+ i idx)
                                                 (vector-ref 
                                                   vect 
                                                   (+ i offset))))
                                   (iota sz))))
                             (fat-file-curr-section-pos-set!
                               file
                               (+ sz cluster-pos))
                             (fat-file-pos-set! file (+ sz pos))
                             (read-bytes-aux!
                               file
                               fs
                               buff
                               (+ idx sz)
                               (- count sz)
                               fail))))
                   (cluster-pos (fat-file-curr-section-pos file))
                   (cluster-len (fat-file-curr-section-length file)))
              (if (>= cluster-pos cluster-len)
                  ; out of bounds of the sector, go fetch the next one
                  (move-next-cluster! file (next-cluster file) succ fail)
                  (succ file)))
            buff))

      (define (read-bytes! file count fail)
        (let* ((fs (fat-file-fs file))
               (count (if (= -1 count)
                          (fat-file-len file)
                          count))
               (buff (make-vector count #x0)))
          (read-bytes-aux!
            file
            fs
            buff
            0
            (if (not (folder? file))
                (min count (- (fat-file-len file) (fat-file-pos file)))
                count)
            fail 
            ))) 

      (define (update-cluster-link-on-disk fs cluster link)
        (let* ((bpb (filesystem-bpb fs))
               (rsvd (BPB-reserved-sector-count bpb))
               (entries-per-sector (BPB-entries-per-sector bpb))
               (offset (modulo cluster entries-per-sector))
               (lba-offset (// cluster entries-per-sector))
               (d (filesystem-disk fs)))
          (disk-apply-sector
            d
            (+ lba-offset rsvd) 
            MRW
            (lambda (vect)
              (wint32
                vect
                (<< offset 2) 
                (cache-link-next link))))))

      (define (unlink-chain-aux fs cache cluster)
        (let* ((bpb (filesystem-bpb fs))
               (link (table-ref cache cluster))
               (rsvd (BPB-reserved-sector-count bpb))
               (entries-per-sector (BPB-entries-per-sector bpb))
               (offset (modulo cluster entries-per-sector))
               (lba-offset (// cluster entries-per-sector))
               (next (cache-link-next cluster))
               (d (filesystem-disk fs)))
          (disk-apply-sector
            d
            (+ lba-offset rsvd) 
            MRW
            (lambda (vect)
              (cache-link-next-set! link 0) ; erase in the cache
              (wint32 vect (<< offset 2) 0) ; erase on disk
              ; stack the env to flush to disk the minimal amount of time
              (unlink-chain-aux fs cache next) 
              ))))

      (define (unlink-chain fs cluster)
        (let ((mut (filesystem-cache-write-mut fs))
              (cache (filesystem-fat-cache fs)))
          (mutex-lock! mut)
          (unlink-chain-aux fs cache cluster)
          (mutex-unlock! mut)))

      ; Fetch the next cluster 
      (define (fetch-or-allocate-next-cluster file c fail)
        (move-next-cluster!
          file
          (next-cluster file)
          c
          (lambda (err-code) ; check the error code. If EOF, we allocate a new cluster
            (if (eq? err-code ERR-EOF)
                (begin ; create a new cluster
                  (find-first-free-cluster 
                    (fat-file-fs file)
                    (lambda (cluster link)
                      (cache-link-prev-set! link (fat-file-curr-clus file))
                      (set-next-cluster! file cluster))
                    fail)
                  (c file))
                (fail err-code)))))

      ; Write 'len' bytes from the vector 'vect' starting at offset 'offset'
      ; the continuation fail is called if the data could not be written.
      ; data is written at the current cursor position of the file 'file'
      ; If necessary, the sector boundary will be moved and extended
      (define (write-bytes! file vect offset len fail)
        (if (> len 0)
            (let* ((wrt (lambda (f)
                          (let* ((cluster-start (fat-file-curr-section-start f))
                                 (fs (fat-file-fs file))
                                 (disk (filesystem-disk fs))
                                 (cluster-pos (fat-file-curr-section-pos f))
                                 (pos (fat-file-pos file))
                                 (cluster-len (fat-file-curr-section-length f))
                                 (lg2spc (filesystem-lg2spc fs))
                                 (lg2bps (filesystem-lg2bps fs))
                                 (bps (s<< 1 lg2bps))
                                 (spc (s<< 1 lg2spc))
                                 (bpc (s<< 1 (+ lg2bps lg2spc))) ; bytes per cluster
                                 ; How many bytes left in cluster?
                                 (left-in-cluster (- cluster-len cluster-pos))
                                 ; How many bytes left in sector?
                                 (left-in-sector (- bps (modulo cluster-pos bps)))
                                 (sz (min len left-in-cluster left-in-sector))
                                 (lba (+ cluster-start (// cluster-pos bps)))
                                 (dest-offset (modulo cluster-pos bps)))
                            (disk-apply-sector
                              disk
                              lba
                              MRW
                              ; write to vector
                              (lambda (v)
                                (vector-copy!
                                  v
                                  dest-offset
                                  vect
                                  offset
                                  (+ offset len))))
                            (fat-file-pos-set! f (+ sz pos))
                            (fat-file-curr-section-pos-set! f (+ cluster-pos sz))
                            (write-bytes! f vect (+ offset sz) (- len sz) fail))))
                   (cluster-pos (fat-file-curr-section-pos file))
                   (cluster-len (fat-file-curr-section-length file))) 
              (if (>= cluster-pos cluster-len)
                  (fetch-or-allocate-next-cluster file wrt fail) ; fetch and then write
                  (wrt file))) ; write
            #t))

      (define (read-string! file len fail)
        (vector->string
          (vector-map integer->char (read-bytes! file len fail))))

      (define (file-read! file len fail)
        (let ((m (fat-file-mode file)))
          (if (mask m FILE-MODE-BINARY)
              (read-bytes! file len fail)
              (read-string! file len fail))))

      (define (update-file-len file len)
        (fat-file-len-set! file len)
        (flush-fat-file-to-disk
          file 
          (make-root-entry file)))

      (define (file-write! file vect offset len fail)
        (let ((result (write-bytes! file vect offset len fail)))
          (let ((pos (fat-file-pos file))
                (max-len (fat-file-len file)))
            (if (> pos max-len)
                (update-file-len file pos)))
          result))

      ; Truncate a file
      ; This involves two things: erasing the cluster chain to free the blocks,
      ; and setting the file length to zero.
      (define (truncate-file file)
        (let* ((fs (fat-file-fs file))
               (chain (filesystem-fat-cache fs))
               (cluster (fat-file-first-clus file))
               (link (table-ref chain cluster))
               (next (cache-link-next link)))
          (if (not (mask FAT-32-EOF next))
              ; We want to keep the current block
              (unlink-chain fs chain next))
          (update-file-len file 0)))

      ; Parse the file opening mode
      ; r, r+ : read, read-write from start respectively
      ; w, w+ : write, read-write from start (truncation)
      ; a, a+ : write, read-write, from end (append)
      ; c is the continuation that will
      (define (parse-modes mode)
        ; We want to take the smallest mode
        ; in case of conflict, but leave the + there
        (let ((ored (fold (lambda (c n)
                            (fxior
                              n
                              (cond ((eq? #\+ c) FILE-MODE-PLUS)
                                    ((eq? #\a c) FILE-MODE-APPEND)
                                    ((eq? #\w c) FILE-MODE-TRUNC)
                                    ((eq? #\b c) FILE-MODE-BINARY)
                                    ((eq? #\r c) FILE-MODE-READ))))
                          0 (string->list mode))))
          (fxior
            (fxand FILE-MODE-PLUS ored)
            (fxand FILE-MODE-BINARY ored)
            (cond ((mask ored FILE-MODE-READ)
                   FILE-MODE-READ)
                  ((mask ored FILE-MODE-TRUNC)
                   FILE-MODE-TRUNC)
                  ((mask ored FILE-MODE-APPEND)
                   FILE-MODE-APPEND)))))

      (define (flush-fat-file-to-disk file entry)
        (let* ((fs (fat-file-fs file))
               (entry-cluster (fat-file-entry-cluster file))
               (entry-offset  (fat-file-entry-offset file))
               (lba (+ (filesystem-first-data-sector fs)
                       (- entry-cluster 2)
                       (// entry-offset (<< 1 (filesystem-lg2bps fs)))))
               (leftover-offset (modulo entry-offset (filesystem-lg2bps fs))))
          (disk-apply-sector
            (filesystem-disk fs)
            lba
            MRW
            (lambda (vect)
              (unpack-entry entry vect entry-offset)))))

      (define (make-empty-fat-file
                fs
                parent
                clus
                type
                name)
        (let* ((bpb (filesystem-bpb fs))
               (bps (BPB-bps bpb))
               (root-clus (BPB-root-cluster bpb))
               (sec-per-cluster (BPB-sec-per-cluster bpb)))
          (make-fat-file
            fs
            clus
            clus
            (BPB-first-data-sector bpb)
            (+ (- clus 2) (* bps sec-per-cluster))
            0
            0
            0
            (fat-file-first-clus parent) 
            0 
            type
            (fxior FILE-MODE-TRUNC FILE-MODE-PLUS)
            (make-logical-entry
              name
              (if (eq? type TYPE-FOLDER) FAT-ATTR-DIRECTORY 0)
              0 ; ntres (leave at 0)
              0 ; create time tenth
              0 ; create time
              0 ; create date
              0 ; last access date
              (fxand #xFF (>> clus 16)) ; cluster high
              0 ; last write time
              0 ; last write date
              (fxand #xFF clus) ; cluster low
              0
              (> (string-length name) FAT-NAME-LENGTH)
              ))))

      ; Create a file and return a handle to it
      (define (create-fat-file fs path type)
        (let* ((parts (simplify-path path))
               (revd (reverse parts))
               (file-name (car revd))
               (parent-parts (reverse (cdr revd)))
               (root (open-root-dir fs)))
          (follow-path
            root
            parent-parts
            (lambda (parent) ; we opened the parent of where we want to insert a file (or directory)
              (find-first-free-cluster
                fs
                (lambda (cluster link)
                  (let* ((new-file (make-empty-fat-file
                                     fs
                                     parent
                                     cluster
                                     type
                                     file-name))
                         (entries (fat-file->entries new-file))
                         (wrt (lambda (offset)
                                ; Set the cursor to the right place
                                (file-set-cursor-absolute! parent offset)
                                ; The offset is set to zero when we create the empty file
                                (fat-file-entry-offset-set! new-file offset)
                                ; Fold right we start right from left, which is what we want
                                (fold-right
                                  (lambda (e r)
                                    (let ((v (make-vector entry-width 0)))
                                      (write-bytes!
                                        parent
                                        (if (lfn? e)
                                            (unpack-lfn e v 0)
                                            (unpack-entry e v 0))
                                        0
                                        entry-width
                                        ID))
                                    ; We count the number of written entries
                                    (++ r))
                                  0
                                  entries)
                                new-file
                                )))
                    (look-for-n-available-entries!
                      parent
                      (length entries)
                      wrt
                      (lambda (err offset) (if (>= offset 0) (wrt offset) (ID err))))))
                ID))
            (lambda (err)
              ERR-FNF))))

      ; Open a fat file
      (define (open-fat-file fs path mode)
        (let ((parts (simplify-path path))
              (root (open-root-dir fs))
              (mode (parse-modes mode)))
          (follow-path root parts
                       (lambda (f)
                         (cond ((mask mode FILE-MODE-READ) ; r, r+
                                #t ; nothing special
                                )
                               ((mask mode FILE-MODE-APPEND)
                                (file-set-cursor-absolute!
                                  f
                                  (max 0 (- (fat-file-len f) 1))
                                  ))
                               ((mask mode FILE-MODE-TRUNC)
                                (truncate-file f)))
                         (fat-file-mode-set! f mode)
                         f)
                       (lambda (err) ERR-FNF)))) 


      ; --------------------------------------------------
      ; --------------------------------------------------
      ; --------------------------------------------------
      ;                      Tests
      ; --------------------------------------------------
      ; --------------------------------------------------
      ; --------------------------------------------------

      (define (f-test fs)
        (look-for-n-available-entries!
          (open-root-dir fs)
          5
          ID
          (lambda (err pos)
            (display err)
            (display pos))))

      (define (a-tests)
        (let ((fs (car filesystem-list)))
          (create-fat-file fs "home/sam/thisisafilewithalongfilenameevenlonguernow.txt" TYPE-FILE)))

      (define (b-tests)
        (let ((fs (car filesystem-list)))
          (follow-path (open-root-dir fs) (list "home") list-directory ID)))

      (define (c-tests)
        (let ((fs (car filesystem-list)))
          (create-fat-file fs "home/thisisafilewithalongfilename.txt" TYPE-FILE)))

      (define (d-tests)
        (let ((fs (car filesystem-list)))
          (follow-path (open-root-dir fs) (list "home" "sam") list-directory ID)))

      (define (f-tests)
        (let ((fs (car filesystem-list)))
          (list-directory (open-root-dir fs))))

      ))
