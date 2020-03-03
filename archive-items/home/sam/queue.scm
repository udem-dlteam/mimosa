; The mimosa project
; A scheme fixed size queue

(namespace ("_queue#"
            make
            get-size
            get-lo
            get-hi
            empty?
            push
            pop
            ))

;;;============================================================================

(define (__get-backing-vector q)
  (vector-ref q 3))

(define (__set-hi! hi q)
  (begin
    (vector-set! q 1 hi)
    hi))

(define (__set-lo! lo q)
  (begin
    (vector-set! q 0 lo)
    lo))

(define (__set-at! idx val q)
  (let ((b (__get-backing-vector q)))
    (vector-set! b idx val)
    val))

(define (__get-at idx q)
  (let ((b (__get-backing-vector q)))
    (vector-ref b idx)))

(define (make size)
  ; A queue contains a lo position, a hi position, a size and the vector itself
  (vector 0 0 (+ size 1) (make-vector (+ size 1) (list))))

(define (get-size q)
  (vector-ref q 2))

(define (get-lo q)
  (vector-ref q 0))

(define (get-hi q)
  (vector-ref q 1))

(define (empty? q)
  (let ((lo (get-lo q))
        (hi (get-hi q)))
    (= lo hi)))

; push an Element into the Queue
(define (push element queue)
  (let* ((e element)
         (q queue)
         (hi (get-hi q))
         (lo (get-lo q))
         (sz (get-size q))
         (n-hi (modulo (+ 1 hi) sz)))
    (if (= n-hi lo)
        #f 
        (begin
          (__set-at! hi element q)
          (__set-hi! n-hi q)
          element))))

(define (pop queue)
  (let* ((q queue)
         (hi (get-hi q))
         (lo (get-lo q))
         (sz (get-size q)))
    (if (= lo hi)
        #f
        (let ((e (__get-at lo q)))
          (__set-at! lo (list) q)
          (__set-lo! (modulo (+ 1 lo) sz) q)
          e))))

(define (peek queue)
  (let* ((q queue)
         (hi (get-hi q))
         (lo (get-lo q))
         (sz (get-size q)))
    (if (= lo hi)
        #f
        (let ((e (__get-at lo q)))
          e))))
