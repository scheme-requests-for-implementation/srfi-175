(define (ensure-int x)
  (if (char? x) (char->integer x) x))

(define (ascii-base-offset-limit x base offset limit)
  (let ((cc (ensure-int x)))
    (and (>= cc base) (< cc (+ base limit))
         (+ offset (- cc base)))))

;;

(define (ascii-char? x)
  (and (char? x) (< (char->integer x) #x80)))

(define (ascii-control? x)
  (let ((cc (ensure-int x)))
    (or (< cc #x20) (= cc #x7f))))

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
  (ascii-base-offset-limit x #x30 0 (min limit 10)))

(define (ascii-upper-case-value x offset limit)
  (ascii-base-offset-limit x #x41 offset (min limit 26)))

(define (ascii-lower-case-value x offset limit)
  (ascii-base-offset-limit x #x61 offset (min limit 26)))

(define (ascii-nth-digit n)
  (integer->char (+ #x30 (modulo n 10))))

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

;;

(define ascii-digits "0123456789")
(define ascii-lower-case "abcdefghijklmnopqrstuvwxyz")
(define ascii-upper-case "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
(define ascii-punctuation "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~")
