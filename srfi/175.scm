(define (ensure-int x)
  (if (char? x) (char->integer x) x))

(define (base-offset-limit x base offset limit)
  (let ((cc (ensure-int x)))
    (and (>= cc base) (< cc (+ base limit))
         (+ offset (- cc base)))))

(define (char->int->char map-int char)
  (let ((int (map-int (char->integer char))))
    (and int (integer->char int))))

(define (int->char->int map-char int)
  (let ((char (map-char (integer->char int))))
    (and char (char->integer char))))

;;

(define (ascii-codepoint? x)
  (and (integer? x) (exact? x) (<= 0 x #x7f)))

(define (ascii-char? x)
  (and (char? x) (< (char->integer x) #x80)))

(define (ascii-bytevector? x)
  (and (bytevector? x)
       (let check ((i (- (bytevector-length x) 1)))
         (or (< i 0) (and (< (bytevector-u8-ref x i) #x80)
                          (check (- i 1)))))))

(define (ascii-string? x)
  (and (string? x)
       (let ((in (open-input-string x)))
         (let check ()
           (let ((char (read-char in)))
             (or (eof-object? char)
                 (and (< (char->integer char) #x80) (check))))))))

(define (ascii-control? x)
  (let ((cc (ensure-int x)))
    (or (<= 0 cc #x1f) (= cc #x7f))))

(define (ascii-display? x)
  (let ((cc (ensure-int x)))
    (<= #x20 cc #x7e)))

(define (ascii-whitespace? x)
  (let ((cc (ensure-int x)))
    (cond ((< cc #x09) #f)
          ((< cc #x0e) #t)
          (else (= cc #x20)))))

(define (ascii-space-or-tab? x)
  (let ((cc (ensure-int x)))
    (case cc ((#x09 #x20) #t) (else #f))))

(define (ascii-punctuation? x)
  (let ((cc (ensure-int x)))
    (or (<= #x21 cc #x2f)
        (<= #x3a cc #x40)
        (<= #x5b cc #x60)
        (<= #x7b cc #x7e))))

(define (ascii-upper-case? x)
  (let ((cc (ensure-int x)))
    (<= #x41 cc #x5a)))

(define (ascii-lower-case? x)
  (let ((cc (ensure-int x)))
    (<= #x61 cc #x7a)))

(define (ascii-alphabetic? x)
  (let ((cc (ensure-int x)))
    (or (<= #x41 cc #x5a)
        (<= #x61 cc #x7a))))

(define (ascii-alphanumeric? x)
  (let ((cc (ensure-int x)))
    (or (<= #x30 cc #x39)
        (<= #x41 cc #x5a)
        (<= #x61 cc #x7a))))

(define (ascii-numeric? x radix)
  (not (not (ascii-digit-value x radix))))

;;

(define (ascii-digit-value x limit)
  (base-offset-limit x #x30 0 (min limit 10)))

(define (ascii-upper-case-value x offset limit)
  (base-offset-limit x #x41 offset (min limit 26)))

(define (ascii-lower-case-value x offset limit)
  (base-offset-limit x #x61 offset (min limit 26)))

(define (ascii-nth-digit n)
  (and (<= 0 n 9) (integer->char (+ #x30 n))))

(define (ascii-nth-upper-case n)
  (integer->char (+ #x41 (modulo n 26))))

(define (ascii-nth-lower-case n)
  (integer->char (+ #x61 (modulo n 26))))

(define (ascii-upcase x)
  (if (char? x)
      (integer->char (ascii-upcase (char->integer x)))
      (or (ascii-lower-case-value x #x41 26) x)))

(define (ascii-downcase x)
  (if (char? x)
      (integer->char (ascii-downcase (char->integer x)))
      (or (ascii-upper-case-value x #x61 26) x)))

(define (ascii-control->display x)
  (if (char? x)
      (char->int->char ascii-control->display x)
      (or (and (<= 0 x #x1f) (+ x #x40))
          (and (= x #x7f) #x3f))))

(define (ascii-display->control x)
  (if (char? x)
      (char->int->char ascii-display->control x)
      (or (and (<= #x40 x #x5f) (- x #x40))
          (and (= x #x3f) #x7f))))

(define (ascii-open-bracket char)
  (case char
    ((#\( #\[ #\{ #\<) char)
    (else (and (integer? char) (int->char->int ascii-open-bracket char)))))

(define (ascii-close-bracket char)
  (case char
    ((#\) #\] #\} #\>) char)
    (else (and (integer? char) (int->char->int ascii-close-bracket char)))))

(define (ascii-mirror-bracket char)
  (case char
    ((#\() #\))
    ((#\)) #\()
    ((#\[) #\])
    ((#\]) #\[)
    ((#\{) #\})
    ((#\}) #\{)
    ((#\<) #\>)
    ((#\>) #\<)
    (else (and (integer? char) (int->char->int ascii-mirror-bracket char)))))
