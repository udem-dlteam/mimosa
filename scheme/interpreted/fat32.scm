;; Mimosa

;; Université de Montréal
;; Marc Feeley, Samuel Yvon
(define-library (fat32)
  (import
   (utils)
   (errors)
   (disk)
   (gambit)
   (rtc)
   (debug))
  (export
   TYPE-FILE
   TYPE-FOLDER
   entry-name
   fat-file-exists?
   fat32-setup
   file-create!
   file-delete!
   file-open!
   file-read!
   file-write!
   file-write-string!
   filesystem-list
   list-directory
   look-for-n-available-entries!
   make-lfns
   pack-BPB
   pack-entry
   simplify-path
   stat
   stat-from-file
   stat-struct-creation-date
   stat-struct-creation-time
   stat-struct-last-access-date
   stat-struct-last-write-date
   stat-struct-last-write-time
   stat-struct-type
   test-suite
   user-load
   write-file
   )
  (begin
    ;; Somehow, inclusion of macros through library includes do not work
    ;; with the current Gambit version
    (include "mimosa-macros.scm")
    ;; --------------------------------------------------
    ;; --------------------------------------------------
    ;; --------------------------------------------------
    ;;         Definitions, constants and globals
    ;;    These are largely taken from the FAT.h file.
    ;; --------------------------------------------------
    ;; --------------------------------------------------
    ;; --------------------------------------------------
    (define ATTR-BIT-POS 11)
    (define FAT-LINK-MASK #x0FFFFFFF)
    (define FAT-DELETED-MARKER #xE5)
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

    ;; --------------------------------------------------
    ;; --------------------------------------------------
    ;; --------------------------------------------------
    ;;                      MACROS
    ;; --------------------------------------------------
    ;; --------------------------------------------------
    ;; --------------------------------------------------

    (define-macro (folder? ff)
      `(eq? (fat-file-type ,ff) TYPE-FOLDER))

    (define-macro (file? ff)
      `(eq? (fat-file-type ,ff) TYPE-FILE))

    (define-macro (empty-entry? vect)
      (let ((first-byte (gensym)))
        `(let ((,first-byte (vector-ref ,vect 0)))
           (fx= ,first-byte #x00))))

    (define-macro (deleted-entry? vect)
      (let ((first-byte (gensym)))
        `(let ((,first-byte (vector-ref ,vect 0)))
           (fx= ,first-byte FAT-DELETED-MARKER))))

    (define-macro (lfn-entry? vect)
      `(mask (vector-ref ,vect ATTR-BIT-POS) FAT-ATTR-LONG-NAME))

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
        (fat-file-fs ,file)
        (fat-file-curr-clus ,file)
        (fat-file-pos ,file)
        ,mvmt
        (lambda (new-cluster cluster-pos)
          (fat-file-curr-clus-set! ,file new-cluster)
          (fat-file-pos-set! ,file cluster-pos))))

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

    ;; --------------------------------------------------
    ;; --------------------------------------------------
    ;; --------------------------------------------------
    ;;                  STRUCTURES
    ;; --------------------------------------------------
    ;; --------------------------------------------------
    ;; --------------------------------------------------

    ;; Structure for a fat file handler
    (define-structure fat-file
      fs ;; the filesystem that owns the file
      first-clus ;; the first cluster of the file
      curr-clus ;; the current cluster of the file
      pos ;; the current position, in byte, inside the file
      len ;; the length of the file
      entry-cluster ;; cluster where the entry is located
      entry-offset ;; offset within that cluster where the entry is located
      type ;; the type of the file (file or folder)
      mode ;; the mode of the file
      entry ;; the logical entry from the file
      )

    ;; Structure for the boot block parameter that contains relevant
    ;; information on the disk structure
    (define-c-like-structure BPB
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

    ;; Structure for a fat file in a directory
    (define-c-like-structure entry
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

    ;; Structure for a long file name entry
    (define-c-like-structure lfn
      (ord 1)
      (name1 10)
      (attr 1)
      (type 1)
      (checksum 1)
      (name2 12)
      (cluster-lo 2)
      (name3 -4))

    ;; A logical directory entry. The name is a string, and it abstracts
    ;; away the fact that it comes from a long file name and thus combines
    ;; an entry and the multiple associated LFNs
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
      lfn?)

    ;; File system present on a disk
    (define-structure filesystem
      disk ;; the disk it comes from
      bpb  ;; the boot block
      lg2bps ;; lg2 bytes per sector
      lg2spc ;; lg2 sector per cluster
      lg2bpc ;; lg2 bytes per cluster
      bpc ;; bytes per cluster
      first-data-sector ;; the first sector that contains data
      cache-write-mut ;; mutex to control access to the cache
      fat-cache) ;; does not need to be a hash table... a vector would have been good

    ;; Structure that abstracts away the statistic of a file
    (define-structure stat-struct
      type ;; the file type
      creation-date ;; date of creation of the file
      creation-time ;; creation time of the file
      last-access-date ;; last access to the file
      last-write-date ;; last write date
      last-write-time ;; last write time
      )

    ;; Structure that represents a cache link of the fat table
    (define-structure cache-link
      next ;; the next cluster address as a number
      prev ;; the previous cluster address as a number (the one for which the next field is this cache link)
      )

    ;; --------------------------------------------------
    ;; --------------------------------------------------
    ;; --------------------------------------------------
    ;;                  Utils for FAT
    ;; --------------------------------------------------
    ;; --------------------------------------------------
    ;; --------------------------------------------------

    ;; Substring that does not fail if end is past the string limit
    (define (safe-substring s start end)
      (let ((l (string-length s)))
        (if (>= start l)
            ""
            (substring s start (min l end)))))

    ;; FAT32 checksum for the string that represents the filename
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

    ;; Make a fat 16 bit integer for the hours, minute and seconds
    (define (make-fat-time hours minutes seconds)
      ;; According to the FAT specification, the FAT time is set that way:
      ;; Bits 15-11: the hour
      ;; Bits 10-5 : the minutes
      ;; Bits 4-0  : the half seconds
      (fxior
       (<< (fxand #x1F hours) 11)
       (<< (fxand #x3F minutes) 5)
       (fxand #x1F (>> seconds 1))))

    ;; Unpack a fat time entry and call
    ;; the fn function with arguments hours, minutes and second
    (define (unpack-fat-time fat-time fn)
      ;; Bits 15-11: the hour
      ;; Bits 10-5 : the minutes
      ;; Bits 4-0  : the half seconds
      (let ((hours (fxand #x1F (>> fat-time 11)))
            (minutes (fxand #x3F (>> fat-time 5)))
            (seconds (fxand #x1F fat-time)))
        (fn hours minutes seconds)))

    ;; Make a fat 16 bit integer for the day, month and year
    (define (make-fat-date day month year)
      ;; According to the FAT specification, the FAT date is set that way:
      ;; Bits 15-9: Year relative to 1980
      ;; Bits 8-5: Month
      ;; Bits 4-0: Day of month
      (let ((year (min (- year 1980) 1980))) ;; year below 1980 is incorrect
        (fxior
         (<< (fxand year #x7F) 9)
         (<< (fxand month #xF) 5)
         (fxand day #x1F))))

    ;; Unpack a fat date and call the fn function
    ;; with the arguments year month day of month
    (define (unpack-fat-date fat-date fn)
      ;; According to the FAT specification, the FAT date is set that way:
      ;; Bits 15-9: Year relative to 1980
      ;; Bits 8-5: Month
      ;; Bits 4-0: Day of month
      (fn
       (fxand #x1F fat-date)
       (fxand #xF (>> fat-date 5))
       (fx+ 1980 (fxand #x7F (>> fat-date 9)))))

    ;; Extract the long file name string from
    ;; a long file name (assembles the text section)
    (define (extract-lfn-data lfn)
      (let ((text (map (apply-on lfn) (list lfn-name1 lfn-name2 lfn-name3))))
        (fold-right
         string-append
         ""
         (map
          lfn-name->string
          (map (lambda (v) (vector-map integer->char v)) text)))))

    ;; Simplify a path and return it as a list of folder, possibly with a file at the end,
    ;; that need to be traversed to get to the end of the path. Relative movements
    ;; are taken into account and applied to simplify the path
    (define (simplify-path path)
      (let ((split (split-string #\/ path)))
        (fold-right
         (lambda (c r)
           (if (and (> (length r) 0) (string=? ".." (car r)))
               (cdr r)
               (cons c r)))
         (list)
         split)))

    ;; Take a string that represents a file name and convert it into a
    ;; vector that is suited for the name field of a directory entry.
    (define (string->short-name-vect str)
      (let ((split (split-string #\. str)))
        (let ((v (make-vector 11 (char->integer #\space)))
              (name (string-upcase (car split))))
          (vector-copy! v 0 (string->u8vector name) 0 (min 8 (string-length name)))
          (if (> (length split) 1) ;; extension is not necessary
              (let ((ext (string-upcase (cadr split))))
                (vector-copy! v 8 (string->u8vector ext) 0 (min 3 (string-length ext)))))
          v)))

    ;; Remove spaces from a vector
    (define (remove-spaces v)
      (fold-right
       (lambda (e r)
         (if (eq? e #\space) r (cons e r)))
       (list)
       (vector->list v)))

    ;; Take a short name, from a directory entry, and make it into a file name.
    ;; this method will add the dot for the extension of the file
    (define (short-name->string vect)
      (let* ((v (vector-map integer->char vect))
             (name (subvector v 0 8))
             (ext (subvector v 8 11)))
        (let ((short-name (remove-spaces name))
              (short-ext (remove-spaces ext)))
          (if (= 0 (length short-ext))
              (list->string short-name)
              (list->string (append short-name (list #\.) short-ext))))))

    ;; Transform a a long file name vector bit
    ;; in a string
    (define (lfn-name->string vect)
      (list->string (fold-right (lambda (c r)
                                  (if (or (eq? #\null c) (eq? #\xFF c))
                                      r
                                      (cons c r)))
                                (list)
                                (vector->list vect))))

    ;; Convert a cluster number to a LBA
    (define (cluster->lba fs cluster)
      (let ((lg2bps (filesystem-lg2bps fs))
            (lg2spc (filesystem-lg2spc fs))
            (fds (filesystem-first-data-sector fs)))
        (+ fds (<< (- cluster 2) (+ lg2bps lg2spc)))))


    ;; Convert a cluster number and a cluster offset to a
    ;; LBA
    (define (cluster+offset->lba fs cluster offset)
      (let ((lg2bps (filesystem-lg2bps fs))
            (lg2spc (filesystem-lg2spc fs))
            (fds (filesystem-first-data-sector fs)))
        (+ fds (<< (- cluster 2) lg2spc) (// offset (<< 1 lg2bps)))))


    ;; --------------------------------------------------
    ;; --------------------------------------------------
    ;; --------------------------------------------------
    ;;                    Driver init
    ;; --------------------------------------------------
    ;; --------------------------------------------------
    ;; --------------------------------------------------

    ;; Create a cache of the FAT tables of the disk
    (define (make-fat-cache dsk bpb)
      (let* ((fat-sz (BPB-fat-size-32 bpb))
             (rsvd (BPB-reserved-sector-count bpb))
             (entries-per-sector (BPB-entries-per-sector bpb))
             (no-of-entries (* entries-per-sector fat-sz))
             (sectors (map (lambda (s)
                             (with-sector dsk (+ s rsvd) MRO ID)) (iota fat-sz)))
             (cache (make-table)))
        ;; Cache is now filled in
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

    ;; Make a filesystem for a disk
    (define (make-fs dsk)
      (let* ((bpb (with-sector dsk 0 MRO pack-BPB))
             (lg2bps (ilog2 (BPB-bps bpb)))
             (lg2spc (ilog2 (BPB-sec-per-cluster bpb)))
             (lg2bpc (+ lg2bps lg2spc)))
        (make-filesystem
         dsk
         bpb
         lg2bps
         lg2spc
         lg2bpc
         (<< 1 lg2bpc)
         (BPB-first-data-sector bpb)
         (make-mutex) ;; cache write mutex
         (make-fat-cache dsk bpb))))

    ;; Get a root directory file-hande for the filesystem
    (define (root-directory fs)
      (let* ((bpb (filesystem-bpb fs))
             (bps (BPB-bps bpb))
             (root-clus (BPB-root-cluster bpb))
             (sec-per-cluster (BPB-sec-per-cluster bpb)))
        (make-fat-file
         fs               ;; fs
         root-clus        ;; first cluster
         root-clus        ;; curr cluster
         0 ;; abs pos
         0 ;; len
         0 ;; parent first clus
         0 ;; entry pos in parent entry
         TYPE-FOLDER ;; type
         FILE-MODE-READ
         0)))

    ;; Mount a partition from a disk. If the
    ;; disk contains no partition, the default value is
    ;; returned
    (define (mount-partition disk default)
      (if (disk-absent? disk)
          default
          (let* ((fs (make-fs disk))
                 (bpb (filesystem-bpb fs))
                 (fs-type (u8vector->string (BPB-fs-type bpb))))
            (if (string=?
                 (substring fs-type 0 (string-length FAT-32-FS-TYPE))
                 FAT-32-FS-TYPE)
                fs
                default))))

    ;; Mount partitions from a disk
    (define (mount-partitions disk-list)
      (let ((filesystems (fold (lambda (e r)
                                 (let ((fs (mount-partition e #f)))
                                   (if fs (cons fs r) r)))
                               (list)
                               disk-list)))
        (set! filesystem-list filesystems)
        filesystems))

    ;; --------------------------------------------------
    ;; --------------------------------------------------
    ;; --------------------------------------------------
    ;;              Movements of the cursor
    ;; --------------------------------------------------
    ;; --------------------------------------------------
    ;; --------------------------------------------------

    ;; Return the cached cluster (cache-link structure) for the file system
    (define (get-cached-cluster fs cluster)
      (let* ((cache (filesystem-fat-cache fs)))
        (table-ref cache cluster)))

    ;; Fetch the next cluster for a specific cluster, using the cache
    (define (next-cluster fs cluster)
      (let* ((link (get-cached-cluster fs cluster))
             (n (cache-link-next link)))
        (if (fx>= n FAT-32-EOF) EOF n)))

    ;; Reset the cursor of a file, placing the r/w cursor back to the beginning
    ;; of the dinner
    (define (reset-cursor! file)
      (let* ((fs (fat-file-fs file))
             (lg2spc (filesystem-lg2spc fs))
             (lg2bps (filesystem-lg2bps fs))
             (first-cluster (fat-file-first-clus file)))
        (fat-file-curr-clus-set! file first-cluster)
        (fat-file-pos-set! file 0)))

    ;; Set the cursor of a file to an absolute byte position
    (define (set-cursor! file pos)
      (let ((current-pos (fat-file-pos file)))
        (cond ((= current-pos pos)
               #t)
              ((= 0 pos)
               (reset-cursor! file))
              (else
               (let ((delta (- pos current-pos)))
                 (file-move-cursor! file delta))))))

    ;; Navigate the FAT chain, backwards or forward (according to the sign of mvmt)
    ;; It starts from the `relative-to` cluster.
    (define (traverse-fat-chain relative-to mvmt chain)
      (if (= mvmt 0)
          relative-to
          (let ((link (table-ref chain relative-to)))
            (if (> mvmt 0)
                (traverse-fat-chain (cache-link-next link) (-- mvmt) chain)
                (traverse-fat-chain (cache-link-prev link) (++ mvmt) chain)))))

    (define-macro (simulate-direction-move direction fs cluster pos rewind c)
      `(let* ((lg2spc (filesystem-lg2spc ,fs))
              (lg2bps (filesystem-lg2bps ,fs))
              (lg2bpc (filesystem-lg2bpc ,fs))
              (bpc (filesystem-bpc ,fs))
              (section-pos (modulo ,pos bpc))
              (diff (+ section-pos ,rewind))
              (spanned-clusters (>> diff lg2bpc)))
         (if (> (abs spanned-clusters) 0)
             ;; need to go over boundary
             (begin
               (let* ((cluster-leftover (modulo diff (<< 1 lg2bpc)))
                      (new-cluster (traverse-fat-chain
                                    ,cluster
                                    spanned-clusters
                                    (filesystem-fat-cache ,fs))))
                 (c new-cluster cluster-leftover)))
             (c ,cluster (+ section-pos ,rewind)))))

    ;; Simulate a backward move of a file cursor from the a specific position
    ;; The continuation c receives the cluster and offset in the cluster where the
    ;; cursor would be
    (define (simulate-backward-move fs cluster pos rewind c)
      (simulate-direction-move 'BACKWARDS fs cluster pos rewind c))

    ;; Simulate a forward move of a file cursor from the a specific position
    ;; The continuation c receives the cluster and offset in the cluster where the
    ;; cursor would be
    (define (simulate-forward-move fs cluster pos rewind c)
      (simulate-direction-move 'FORWARDS fs cluster pos rewind c))

    ;; Perform a cursor move backwards
    (define (file-move-cursor-backward! file rewind)
      (relative-move-function file rewind -))

    ;; Perform a cursor move forward
    (define (file-move-cursor-forward! file wind)
      (relative-move-function file wind +))

    ;; Simulate a move from a position, with delta being
    ;; a positive or negative number, indicating the direction
    (define (simulate-move fs cluster pos delta c)
      (if (> delta 0)
          (simulate-forward-move fs cluster pos delta c)
          (simulate-backward-move fs cluster pos delta c)))

    ;; Perform a move for a file, with delta being
    ;; a positive or negative number, indicating the direction
    (define (file-move-cursor! file delta)
      (if (> delta 0)
          (file-move-cursor-forward! file delta)
          (file-move-cursor-backward! file delta)))

    ;; Update the next cluster for a cluster, both
    ;; in the cache and on disk
    (define (update-link-next cache clus new-next)
      (let* ((link (table-ref cache clus))
             (previous-next-no (cache-link-next link)))
        ;; Update the new link
        (cache-link-next-set! link new-next)
        (if (and
             (not (>= new-next FAT-32-EOF))
             (> new-next 0))
            (let ((new-next-link (table-ref cache new-next)))
              (cache-link-prev-set! new-next-link clus)))
        ;; If it was set to something, unset it
        (if (and
             (not (>= previous-next-no FAT-32-EOF))
             (> previous-next-no 0))
            (let ((previous-next-link (table-ref cache previous-next-no)))
              (cache-link-prev-set! previous-next-link 0))
            )))

    ;; Set the next cluster, move to it and return the FAT array
    ;; with the file as an argument
    (define (set-next-cluster! fs cluster next-clus)
      (if (eq? next-clus EOF)
          ERR-ARGS
          (let* ((cache (filesystem-fat-cache fs))
                 (disk (filesystem-disk fs))
                 (bpb (filesystem-bpb fs))
                 (lg2bps (filesystem-lg2bps fs))
                 (entries-per-sector (>> (<< 1 lg2bps) 2))
                 (rsvd (BPB-reserved-sector-count bpb))
                 (lba (+ (// cluster entries-per-sector) rsvd))
                 (offset (modulo cluster entries-per-sector)))
            ;; Update in cache
            (update-link-next cache cluster next-clus)
            ;; Update on disk
            (with-sector
             disk
             lba
             MRW
             (lambda (vect) (wint32 vect (<< offset 2) next-clus) next-clus)
             ))))

    ;; --------------------------------------------------
    ;; --------------------------------------------------
    ;; --------------------------------------------------
    ;;                Search / Traversal
    ;; --------------------------------------------------
    ;; --------------------------------------------------
    ;; --------------------------------------------------

    ;; Search the entry with the file name name, and call
    ;; the succ continuation with the logical entry or fail if the
    ;; entry does not seem to exist.
    (define (search-entry file name succ fail)
      (search-entry-aux
       file
       name
       (list)
       succ
       (lazy (fail ERR-FNF))
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

    ;; Read entries as if the file is a FAT directory.
    ;; Takes the argument cont, a lambda that receives the entry and a function
    ;; to read the next entry and fail a function that is called in case of failure
    (define (read-entries file succ fail)
      (let* ((cont (lambda () (read-entries file succ fail)))
             (vect (read-bytes! file entry-width fail)))
        ;; if an error occurs, it's not gonna be a vector
        (if (vector? vect)
            (cond ((deleted-entry? vect)
                   (cont))
                  ((empty-entry? vect)
                   (fail ERR-NO-MORE-ENTRIES))
                  ((lfn-entry? vect)
                   (succ (pack-lfn vect) cont))
                  (else
                   (succ (pack-entry vect) cont)))
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

    ;; Return an absolute position in the 'folder' where
    ;; 'n' consecutive entries are available. The success continuation
    ;; takes the absolute position as a single parameter.
    ;; The fail continuation takes two parameters, the first one being
    ;; the error code and the second one being an absolute position
    ;; that we can start writing to. This can happen if
    ;; there are available entries to write to at the end of a file,
    ;; but the file boundary does not allow more entries to be considered
    ;; part of the file. The boundary can simply be extended (by writing to the file)
    ;; to allow
    ;; This function resets the file cursor to 0 at the start.
    (define (look-for-n-available-entries! folder n succ fail)
      (set-cursor! folder 0)
      (let ((conseq 0))
        (read-entries
         folder
         (lambda (e next)
           (if (entry-deleted? e)
               (if (= (++ conseq) n)
                   ;; Done: find the position
                   (succ (- (fat-file-pos folder) (* n entry-width)))
                   (begin
                     (set! conseq (++ conseq))
                     (next)))
               ;; Not deleted
               (if (entry-empty? e)
                   ;; All the rest is free, we can use it
                   (succ (- (fat-file-pos folder) (* (++ conseq) entry-width)))
                   ;; Reset
                   (begin
                     (set! conseq 0)
                     (next)))))
         (lambda (err)
           (fail
            err
            (cond ((eq? ERR-EOF err) ;; we did not move the cursor
                   (- (fat-file-pos folder)
                      (* conseq entry-width)))
                  ((eq? ERR-NO-MORE-ENTRIES err) ;; the cursor moved before we read 0
                   (- (fat-file-pos folder)
                      (* (++ conseq) entry-width)))
                  (else -1)))))))

    ;; Create a collection of long file name starting at ordinal 'ith'
    ;; for the remaining name part 'name' and the checksum 'chksm'
    (define (make-lfns name ith chksm)
      (let ((l (string-length name)))
        (if (= l 0)
            (list)
            ;; Copy into a LFN and truncate the rest of the string
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

    ;; Create the root entry for a file
    (define (make-root-entry file)
      (let ((le (fat-file-entry file)))
        (make-entry
         (string->short-name-vect (logical-entry-name le))
         (logical-entry-attr le) ;; TODO: upate
         (logical-entry-ntres le) ;; does not change
         (logical-entry-create-time-tenth le) ;; does not change
         (logical-entry-create-time le) ;; does not change
         (logical-entry-create-date le) ;; does not change
         (logical-entry-last-access-date le) ;; TODO: update
         (logical-entry-cluster-hi le) ;; does not change
         (logical-entry-last-write-time le) ;; TODO: update
         (logical-entry-last-write-date le) ;; TODO: update
         (logical-entry-cluster-lo le) ;; does not change
         (fat-file-len file) ;; get the latest information
         )))

    ;; Returns a list in the following order:
    ;; root entry
    ;; LFN 1
    ;; ...
    ;; nth LFN
    (define (fat-file->entries file)
      (let* ((underlying-entry (fat-file-entry file))
             (name (logical-entry-name underlying-entry))
             (name-l (string-length name))
             (requires-lfn? (or;; sometimes an host FS will create lfns
                             ;; to avoid having the name in all caps
                             (logical-entry-lfn? (fat-file-entry file))
                             (> name-l FAT-SHORT-FILE-NAME-MAX-LEN)))
             (hdr (make-root-entry file)))
        (cons hdr (if requires-lfn?
                      (make-lfns name 1 (checksum (entry-name hdr)))
                      (list)))))

    ;; Create a list of subitems in a directory
    (define (list-directory folder)
      (let ((all (read-all-entries folder)))
        (map logical-entry-name (entry-list->logical-entries all))))

    ;; Create a file after we just read the entry of said file
    ;; The cursor is simulated backwards so we get the cluster of the
    ;; parent correctly
    (define (logical-entry->file parent logical)
      (let* ((fs (fat-file-fs parent))
             (fds (filesystem-first-data-sector fs))
             (cluster (build-cluster logical))
             (lg2bps (filesystem-lg2bps fs))
             (lg2spc (filesystem-lg2spc fs)))
        (simulate-move
         fs
         (fat-file-curr-clus parent)
         (fat-file-pos parent)
         (- entry-width)
         (lambda (entry-cluster entry-offset)
           (make-fat-file
            fs
            cluster
            cluster
            0
            (logical-entry-file-size logical)
            entry-cluster
            entry-offset
            (logical-entry-type logical)
            FILE-MODE-READ
            logical)))))

    ;; Follow a path (folder list) and return the last part
    ;; described by the path
    (define (follow-path from to succ fail)
      (let ((l (length to)))
        (if (= l 0)
            (succ from)
            (search-entry
             from ;; we search from there
             (car to) ;; next child
             (lambda (f) ;; on success
               (follow-path
                (logical-entry->file from f)
                (cdr to)
                succ
                fail))
             (lambda (err)
               (if (= l 1)
                   (fail err)
                   (fail ERR-PATH))) ;; on failure
             ))))

    ;; Take a list of entries from a directory, and converts
    ;; it into a list of logical entries. It will merge
    ;; long file names entries into the file. It does not check
    ;; for checksum validity.
    (define (entry-list->logical-entries entry-list)
      ;; The fat spec garantess this structure:
      ;; nth LFN
      ;; nth-1 LFN
      ;; ....
      ;; 1st LFN
      ;; entry
      (fold-right
       (lambda (entry rest)
         (if (lfn? entry)
             ;; Append the name
             (let* ((underlying-entry (car rest))
                    (next-part (extract-lfn-data entry))
                    (name (logical-entry-name underlying-entry)))
               ;; If it was not LFN before, we erase
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
             ;; not LFN, leave it as is
             (cons (entry->logical-entry entry) rest)))
       (list)
       entry-list))

    (define (find-first-free-cluster-aux cache index len succ fail)
      (if (< index len)
          (let* ((link (table-ref cache index))
                 (link-next (cache-link-next link))
                 (link-prev (cache-link-next link)))
            (if (and (fx= link-prev 0) (fx= link-next 0))
                (succ index link)
                (find-first-free-cluster-aux cache (++ index) len succ fail)))
          (fail ERR-EOF)))

    ;; Find the first free cluster in the FS file table.
    ;; the fail continuation is called in case of error,
    ;; the succ continuation is called with the cluster number and the link
    ;; when it found one.
    ;; The link will be marked with an EOF tag to prevent race conditions.
    (define (find-first-free-cluster fs succ fail)
      (let* ((cache (filesystem-fat-cache fs))
             (cache-mut (filesystem-cache-write-mut fs))
             (l (table-length cache)))
        (mutex-lock! cache-mut);; acquire
        (find-first-free-cluster-aux
         cache
         0
         l
         ;; free the mutex when the continuation is called
         (lambda (cluster link)
           (update-link-next cache cluster FAT-32-EOF)
           (update-cluster-link-on-disk fs cluster link)
           (mutex-unlock! cache-mut)
           (succ cluster link))
         (lambda (err)
           (mutex-unlock! cache-mut)
           (fail err)))))

    ;; --------------------------------------------------
    ;; --------------------------------------------------
    ;; --------------------------------------------------
    ;;                Reading / Writing
    ;; --------------------------------------------------
    ;; --------------------------------------------------
    ;; --------------------------------------------------

    (define (read-bytes-aux fs cluster pos max-len buff idx len c fail)
      (if (> len 0)
          (let* ((lg2spc (filesystem-lg2spc fs))
                 (lg2bps (filesystem-lg2bps fs))
                 (bps (s<< 1 lg2bps))
                 (spc (s<< 1 lg2spc))
                 (bpc (filesystem-bpc fs))
                 (disk (filesystem-disk fs))
                 (cluster-pos (modulo pos bpc))
                 ;; How many bytes left in cluster?
                 (left-in-cluster (- bpc cluster-pos))
                 ;; How many bytes left in sector?
                 (left-in-sector (- bps (modulo cluster-pos bps)))
                 (left-in-file (- max-len pos))
                 (sz (min len left-in-cluster left-in-sector left-in-file))
                 (lba (cluster+offset->lba fs cluster cluster-pos))
                 (offset (modulo cluster-pos bps)))
            (with-sector
             disk
             lba
             MRO
             (lambda (vect)
               (for-each
                (lambda (i)
                  (vector-set! buff (+ i idx) (vector-ref vect (+ i offset))))
                (iota sz))))
            (if (= 0 (modulo (+ sz pos) bpc))
                (let ((next-clus (next-cluster fs cluster)))
                  (if (eq? next-clus EOF)
                      (fail ERR-EOF) ;; end
                      (read-bytes-aux fs next-clus (+ sz pos) max-len buff (+ idx sz) (- len sz) c fail)))
                (read-bytes-aux fs cluster (+ sz pos) max-len buff (+ idx sz) (- len sz) c fail)))
          (c buff cluster pos)))

    (define (read-bytes! file count fail)
      (let* ((fs (fat-file-fs file))
             (count (if (= -1 count)
                        (fat-file-len file)
                        count))
             (buff (make-vector count #x0))
             (pos (fat-file-pos file)))
        (read-bytes-aux
         fs
         (fat-file-curr-clus file)
         pos
         (if (folder? file)
             (+ count pos) ;; never run out of space that way
             (fat-file-len file)) ;; prevents reading past the file end
         buff
         0
         (if (not (folder? file))
             (min count (- (fat-file-len file) (fat-file-pos file)))
             count)
         (lambda (result cluster pos)
           (fat-file-curr-clus-set! file cluster)
           (fat-file-pos-set! file pos)
           result)
         fail)))

    (define (update-cluster-link-on-disk fs cluster link)
      (let* ((bpb (filesystem-bpb fs))
             (rsvd (BPB-reserved-sector-count bpb))
             (entries-per-sector (BPB-entries-per-sector bpb))
             (offset (modulo cluster entries-per-sector))
             (lba-offset (// cluster entries-per-sector))
             (d (filesystem-disk fs)))
        (with-sector
         d
         (+ lba-offset rsvd)
         MRW
         (lambda (vect) (wint32 vect (<< offset 2) (cache-link-next link))))))

    (define (unlink-chain-aux fs cache cluster)
      (let* ((bpb (filesystem-bpb fs))
             (rsvd (BPB-reserved-sector-count bpb))
             (entries-per-sector (BPB-entries-per-sector bpb))
             (offset (modulo cluster entries-per-sector))
             (lba-offset (// cluster entries-per-sector))
             (link (table-ref cache cluster))
             (next (cache-link-next link))
             (d (filesystem-disk fs)))
        (with-sector
         d
         (+ lba-offset rsvd)
         MRW
         (lambda (vect)
           (update-link-next (filesystem-fat-cache fs) cluster 0) ;; erase in the cache
           (wint32 vect (<< offset 2) 0) ;; erase on disk
           ;; stack the env to flush to disk the minimal amount of time
           (if (not (mask FAT-32-EOF next))
               (unlink-chain-aux fs cache next))))))

    (define (unlink-chain fs cluster)
      (let ((mut (filesystem-cache-write-mut fs))
            (cache (filesystem-fat-cache fs)))
        (mutex-lock! mut)
        (unlink-chain-aux fs cache cluster)
        (mutex-unlock! mut)))

    ;; Fetch the next cluster
    (define (fetch-or-allocate-next-cluster fs cluster c fail)
      (let ((new-cluster (next-cluster fs cluster)))
        (if (eq? new-cluster EOF)
            (find-first-free-cluster
             fs
             (lambda (new-cluster link)
               (set-next-cluster! fs cluster new-cluster)
               (c new-cluster))
             fail)
            (c new-cluster))))

    (define (write-bytes-aux fs cluster pos vect offset len c fail)
      (if (> len 0)
          (let* ((lg2spc (filesystem-lg2spc fs))
                 (lg2bps (filesystem-lg2bps fs))
                 (bps (s<< 1 lg2bps))
                 (spc (s<< 1 lg2spc))
                 (bpc (filesystem-bpc fs))
                 (disk (filesystem-disk fs))
                 (cluster-pos (modulo pos bpc))
                 ;; How many bytes left in cluster?
                 (left-in-cluster (- bpc cluster-pos))
                 ;; How many bytes left in sector?
                 (left-in-sector (- bps (modulo cluster-pos bps)))
                 (sz (min len left-in-cluster left-in-sector))
                 (lba (cluster+offset->lba fs cluster cluster-pos))
                 (dest-offset (modulo cluster-pos bps)))
            (with-sector
             disk
             lba
             MRW
             ;; write to vector
             (lambda (v) (vector-copy! v dest-offset vect offset (+ offset sz))))
            (if (= 0 (modulo (+ sz pos) bpc))
                (fetch-or-allocate-next-cluster
                 fs
                 cluster
                 (lambda (new-cluster)
                   (write-bytes-aux fs new-cluster (+ sz pos) vect (+ offset sz) (- len sz) c fail))
                 fail
                 )
                (write-bytes-aux fs cluster (+ sz pos) vect (+ offset sz) (- len sz) c fail)))
          (c vect cluster pos)))

    ;; Write 'len' bytes from the vector 'vect' starting at offset 'offset'
    ;; the continuation fail is called if the data could not be written.
    ;; data is written at the current cursor position of the file 'file'
    ;; If necessary, the sector boundary will be moved and extended
    (define (write-bytes! file vect offset len fail)
      (let ((fs (fat-file-fs file))
            (cluster (fat-file-curr-clus file))
            (pos (fat-file-pos file)))
        (write-bytes-aux
         fs
         cluster
         pos
         vect
         offset
         len
         (lambda (vect cluster pos)
           (fat-file-curr-clus-set! file cluster)
           (fat-file-pos-set! file pos)
           vect)
         fail)))

    ;; Read a string from a file
    (define (read-string! file len fail)
      (vector->string
       (vector-map integer->char (read-bytes! file len fail))))

    ;; Read byte data (as a string) from a file
    (define (file-read! file len fail)
      (let ((today (rtc-current-date make-fat-date))
            (entry (fat-file-entry file))
            (m (fat-file-mode file)))
        (logical-entry-last-access-date-set! entry today)
        (if (mask m FILE-MODE-BINARY)
            (read-bytes! file len fail)
            (read-string! file len fail))))

    ;; Update the transient information of a file entry
    ;; on the disk
    (define (update-file-entry file)
      (let* ((len (fat-file-len file))
             (entry (fat-file-entry file))
             (last-access-date (logical-entry-last-access-date entry))
             (last-write-time (logical-entry-last-write-time entry))
             (last-write-date (logical-entry-last-write-date entry))
             (fs (fat-file-fs file))
             (entry-cluster (fat-file-entry-cluster file))
             (entry-offset (fat-file-entry-offset file))
             (lba (cluster+offset->lba fs entry-cluster entry-offset))
             (offset (modulo entry-offset (<< 1 (filesystem-lg2bps fs)))))
        (with-sector
         (filesystem-disk fs)
         lba
         MRW
         (lambda (vect) ;; update all but name
           ;; Only update the transient fields to avoid changing file information that
           ;; might be hard to update (mainly the name since the algorithm for name generation is painful)
           (wint16 vect (+ offset entry-width -14) last-access-date)
           (wint16 vect (+ offset entry-width -10) last-write-time)
           (wint16 vect (+ offset entry-width -8) last-write-date)
           (wint32 vect (+ offset entry-width -4) len)))))

    ;; Write a string to a file,
    (define (file-write-string! file str fail)
      (file-write!
       file
       (string->u8vector str)
       0
       (string-length str)
       fail))

    ;; Write the vect to the file, at offset 'offset' for len bytes
    (define (file-write! file vect offset len fail)
      (let ((result (write-bytes! file vect offset len fail))
            (now (rtc-current-time make-fat-time))
            (today (rtc-current-date make-fat-date)))
        (let ((entry (fat-file-entry file))
              (pos (fat-file-pos file))
              (max-len (fat-file-len file)))
          (logical-entry-last-write-time-set! entry now)
          (logical-entry-last-write-date-set! entry today)
          (if (not (folder? file))
              (begin
                (fat-file-len-set!
                 file
                 (if (>= pos max-len)
                     (++ pos)
                     max-len))
                (update-file-entry file))))
        result))

    ;; Truncate a file
    ;; This involves two things: erasing the cluster chain to free the blocks,
    ;; and setting the file length to zero.
    (define (truncate-file file)
      (let* ((now (rtc-current-time make-fat-time))
             (today (rtc-current-date make-fat-date))
             (fs (fat-file-fs file))
             (chain (filesystem-fat-cache fs))
             (cluster (fat-file-first-clus file))
             (link (table-ref chain cluster))
             (next (cache-link-next link))
             (entry (fat-file-entry file)))
        (if (not (mask FAT-32-EOF next)) ;; We want to keep the current block
            (unlink-chain fs chain next))
        (logical-entry-last-write-time-set! entry now)
        (logical-entry-last-write-date-set! entry today)
        (fat-file-len-set! file 0)
        (update-file-entry file)))

    ;; Parse the file opening mode
    ;; r, r+ : read, read-write from start respectively
    ;; w, w+ : write, read-write from start (truncation)
    ;; a, a+ : write, read-write, from end (append)
    ;; c is the continuation that will
    (define (parse-modes mode)
      ;; We want to take the smallest mode
      ;; in case of conflict, but leave the + there
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

    ;; Take a list of entries 'entry-chain' in the order
    ;; ROOT
    ;; LFN 1
    ;; LFN 2
    ;; ...
    ;; LFN N
    ;; and a file descriptor and write them to the disk
    (define (flush-entry-list-to-disk fs cluster pos entry-chain c)
      (let* ((entry-chain-l (length entry-chain))
             (vector-len (* entry-width entry-chain-l))
             (v (make-vector vector-len 0)))
        ;; unpack into vector
        (fold-right
         (lambda (e r)
           (let ((entry (list-ref entry-chain e)))
             (if (lfn? entry)
                 (unpack-lfn entry v (* entry-width (- entry-chain-l 1 e)))
                 (unpack-entry entry v (* entry-width (- entry-chain-l 1 e)))))
           r)
         v
         (iota entry-chain-l))
        (write-bytes-aux fs cluster pos v 0 vector-len c ID)))

    ;; Create an empty file structure that can be saved on file later
    (define (make-empty-fat-file fs parent clus type name)
      (let* ((bpb (filesystem-bpb fs))
             (bps (BPB-bps bpb))
             (root-clus (BPB-root-cluster bpb))
             (now (rtc-current-time make-fat-time))
             (today (rtc-current-date make-fat-date))
             (sec-per-cluster (BPB-sec-per-cluster bpb)))
        (make-fat-file
         fs
         clus
         clus
         0
         0
         0
         0
         type
         (fxior FILE-MODE-TRUNC FILE-MODE-PLUS)
         (make-logical-entry
          name
          (if (eq? type TYPE-FOLDER) FAT-ATTR-DIRECTORY 0)
          0 ;; ntres (leave at 0)
          0 ;; create time tenth
          now ;; create time
          today ;; create date
          today ;; last access date
          (fxand #xFFFF (>> clus 16)) ;; cluster high
          now ;; last write time
          now ;; last write date
          (fxand #xFFFF clus) ;; cluster low
          0
          (> (string-length name) FAT-NAME-LENGTH)
          ))))

    ;; Create a file and return a handle to it
    (define (file-create! fs path type)
      (let* ((parts (simplify-path path))
             (revd (reverse parts))
             (file-name (car revd))
             (parent-parts (reverse (cdr revd)))
             (root (root-directory fs)))
        (follow-path
         root
         parent-parts
         (lambda (parent) ;; we opened the parent of where we want to insert a file (or directory)
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
                     (bpc (filesystem-bpc fs))
                     (n (length entries))
                     (wrt (lambda (offset)
                            (set-cursor! parent offset)
                            (flush-entry-list-to-disk
                             fs
                             (fat-file-curr-clus parent)
                             (fat-file-pos parent)
                             entries
                             (lambda (r c p)
                               ;; It's possible that writing the file will allocate a cluster that
                               ;; is not reserved for the file yet. A forward move before would therefore
                               ;; potentially end up in an unallocated zone
                               (simulate-move
                                fs
                                c
                                p
                                (- entry-width)
                                (lambda (entry-cluster entry-offset)
                                  ;; Set the correct entry coodinates
                                  (fat-file-entry-cluster-set! new-file entry-cluster)
                                  (fat-file-entry-offset-set! new-file entry-offset)))))
                            new-file)))
                (look-for-n-available-entries!
                 parent
                 n
                 wrt
                 (lambda (err offset)
                   (if (>= offset 0)
                       (wrt offset)
                       (ID err))))))
            ID))
         (lambda (err) ERR-FNF))))

    ;; Mark an entry as deleted
    (define (mark-entry-deleted entry)
      (if (lfn? entry)
          (lfn-ord-set! entry FAT-DELETED-MARKER)
          (entry-name-set!
           entry
           (vector-set (entry-name entry) 0 FAT-DELETED-MARKER)
           ))
      entry)

    ;; Delete a file represented from a filesystem and a path
    (define (file-delete! fs path)
      (let* ((parts (simplify-path path))
             (chain (filesystem-fat-cache fs))
             (root (root-directory fs)))
        (follow-path
         root
         parts
         (lambda (file)
           (let ((start-cluster (fat-file-first-clus file))
                 (new-lfn-chain (map mark-entry-deleted (fat-file->entries file))))
             (unlink-chain fs start-cluster)
             (simulate-move
              fs
              (fat-file-entry-cluster file)
              (fat-file-entry-offset file)
              (* (- entry-width) (-- (length new-lfn-chain)))
              (lambda (c p)
                (flush-entry-list-to-disk
                 fs
                 c
                 p
                 new-lfn-chain
                 (lazy #t))))))
         (lambda (err) ERR-FNF))))

    ;; Open a fat file with a file system, a path and a mode string
    (define (file-open! fs path mode)
      (let ((parts (simplify-path path))
            (root (root-directory fs))
            (mode (parse-modes mode)))
        (follow-path
         root
         parts
         (lambda (f)
           (cond ((mask mode FILE-MODE-READ) ;; r, r+
                  #t) ;; nothing special
                 ((mask mode FILE-MODE-APPEND)
                  (set-cursor! f (max 0 (-- (fat-file-len f)))))
                 ((mask mode FILE-MODE-TRUNC)
                  (truncate-file f)))
           (fat-file-mode-set! f mode)
           f)
         (lambda (err) ERR-FNF))))

    ;; Create a stat structure from a filesystem + file
    (define (stat fs path)
      (let ((file (file-open! fs path "r")))
        (if-not file ERR-FNF stat-from-file)))

    ;; Create a stat structure for a file handler
    (define (stat-from-file file)
      (let* ((ent (fat-file-entry file))
             (create-time (logical-entry-create-time ent))
             (create-date (logical-entry-create-date ent))
             (la-date (logical-entry-last-access-date ent))
             (lw-time (logical-entry-last-write-time ent))
             (lw-date (logical-entry-last-write-date ent)))
        (make-stat-struct
         (fat-file-type file)
         (unpack-fat-date create-date day-month-year->epoch-seconds)
         (unpack-fat-time create-time hour-minute-seconds->seconds)
         (unpack-fat-date la-date day-month-year->epoch-seconds)
         (unpack-fat-date lw-date day-month-year->epoch-seconds)
         (unpack-fat-time lw-time hour-minute-seconds->seconds))))

    ;; This routines loads a file from
    ;; disk and places it in executable context
    (define (user-load fs path)
      (let ((s (file-read! (file-open! fs path "r") -1 ID)))
        (eval (read (open-input-string s)))
        (string-append "eval '(" s ")")))

    (define (test-suite)
      (let* ((fs (car filesystem-list))
             (f (file-create! fs "home/sam/anewfile" TYPE-FILE)))
        (if (not (fat-file? f))
            (debug-write "Test 1 failed"))
        (file-write-string!
         f
         "abcdefghijklmnopqrstuvwxyz" ID)

        (set! f (file-open! fs "home/sam/ANEWFILE" "r"))
        (if (not (fat-file? f))
            (debug-write "Test 2 failed"))

        (if (not (string=? "abcdefghijklmnopqrstuvwxyz" (file-read! f 26 ID)))
            (debug-write "Test 3 failed"))

        (set-cursor! f 0)
        (let ((s (file-read! f 26 ID)))
          (debug-write (string-append "(" s ")"))
          (if (not (string=? "abcdefghijklmnopqrstuvwxyz" s))
              (begin
                (debug-write "Test 4 failed")
                (debug-write (string-append "(" s ")")))))

        (set-cursor! f 5)
        (let ((s (file-read! f 1 ID)))
          (debug-write (string-append "(" s ")"))
          (if (not (string=? "f" s))
              (begin
                (debug-write "Test 5 failed")
                (debug-write (string-append "(" s ")")))))

        (set-cursor! f 4)
        (let ((s (file-read! f 1 ID)))
          (debug-write (string-append "(" s ")"))
          (if (not (string=? "e" s))
              (begin
                (debug-write "Test 6 failed")
                (debug-write (string-append "(" s ")")))))

        (set! f (file-open! fs "home/sam/ANEWFILE" "w+"))
        (let ((s (file-read! f -1 ID)))
          (debug-write (string-append "(" s ")"))
          (if (not (string=? "" s))
              (begin
                (debug-write "Test 7 failed")
                (debug-write (string-append s)))))
        ))

    (define (fat32-setup)
      (mount-partitions disk-list)
      #t)

    ))
