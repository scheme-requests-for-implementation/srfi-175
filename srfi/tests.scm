(import (scheme base) (scheme file) (scheme read) (scheme write) (srfi 175))

(define-syntax want
  (syntax-rules ()
    ((_ right-answer (proc args ...))
     (unless (equal? right-answer (proc args ...))
       (display "Failed: wanted ")
       (write right-answer)
       (display " but got ")
       (write (proc args ...))
       (display " from ")
       (display '(proc args ...))
       (newline)))))

(want #f (ascii-byte? -1))
(want #t (ascii-byte? 0))
(want #t (ascii-byte? #x7f))
(want #f (ascii-byte? #x80))

(want #t (ascii-char? (integer->char 0)))
(want #t (ascii-char? (integer->char #x7f)))
(want #f (ascii-char? (integer->char #x80)))

(want #t (ascii-string? ""))
(want #t (ascii-string? "a"))
(want #t (ascii-string? "a b c"))
(want #f (ascii-string? "å b o"))
(want #t (ascii-string? (make-string 1 (integer->char #x7f))))
(want #f (ascii-string? (make-string 1 (integer->char #x80))))

(want #t (ascii-bytevector? (string->utf8 "")))
(want #t (ascii-bytevector? (string->utf8 "a")))
(want #t (ascii-bytevector? (string->utf8 "a b c")))
(want #f (ascii-bytevector? (string->utf8 "å b o")))
(want #t (ascii-bytevector?
          (string->utf8 (make-string 1 (integer->char #x7f)))))
(want #f (ascii-bytevector?
          (string->utf8 (make-string 1 (integer->char #x80)))))

(want #t (ascii-display? #\space))
(want #f (ascii-display? #\tab))
(want #f (ascii-display? #\newline))
(want #f (ascii-display? (integer->char #x0d)))

(want #t (ascii-space-or-tab? #\space))
(want #t (ascii-space-or-tab? #\tab))
(want #f (ascii-space-or-tab? #\newline))
(want #f (ascii-display? (integer->char #x0d)))

(let ((lowers "abcdefghijklmnopqrstuvwxyz")
      (uppers "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))
  (let loop ((i 0))
    (when (< i 26)
      (let ((lower (string-ref lowers i))
            (upper (string-ref uppers i)))
        (want upper (ascii-upcase upper))
        (want upper (ascii-upcase lower))
        (want lower (ascii-downcase upper))
        (want lower (ascii-downcase lower))
        (loop (+ i 1))))))

(let loop ((cc 0))
  (when (< cc #x80)
    (unless (ascii-alphabetic? cc)
      (want cc (ascii-upcase cc))
      (want cc (ascii-downcase cc)))
    (loop (+ cc 1))))

(let loop ((cc 0))
  (when (< cc #x80)
    (want #f (ascii-char? cc))
    (want #t (ascii-char? (integer->char cc)))
    (cond ((ascii-alphabetic? cc)
           (want #t (ascii-upper-case? (ascii-upcase cc)))
           (want #t (ascii-lower-case? (ascii-downcase cc)))
           (want #f (ascii-lower-case? (ascii-upcase cc)))
           (want #f (ascii-upper-case? (ascii-downcase cc)))
           (want #t (ascii-alphanumeric? cc))
           (want #t (ascii-display? cc))
           (want #f (ascii-punctuation? cc))
           (want #f (ascii-control? cc))
           (want #f (ascii-numeric? cc 10))
           (want #f (ascii-whitespace? cc))
           (want #f (ascii-space-or-tab? cc)))
          ((ascii-control? cc)
           (want #f (ascii-display? cc))
           (want #f (ascii-punctuation? cc))
           (want cc
                 (ascii-display->control
                  (ascii-control->display cc)))
           (want (integer->char cc)
                 (ascii-display->control
                  (ascii-control->display (integer->char cc)))))
          ((ascii-open-bracket cc)
           (want #f (ascii-close-bracket cc))
           (want cc (ascii-mirror-bracket (ascii-mirror-bracket cc)))
           (want cc (ascii-open-bracket
                     (ascii-mirror-bracket
                      (ascii-close-bracket
                       (ascii-mirror-bracket cc))))))
          ((ascii-close-bracket cc)
           (want #f (ascii-open-bracket cc))
           (want cc (ascii-mirror-bracket (ascii-mirror-bracket cc)))
           (want cc (ascii-close-bracket
                     (ascii-mirror-bracket
                      (ascii-open-bracket
                       (ascii-mirror-bracket cc)))))))
    (loop (+ cc 1))))

(want #f (ascii-char? -1))
(want #f (ascii-char? #x80))
(want #f (ascii-char? (integer->char #x80)))

(want #f (ascii-control? -1))
(want #t (ascii-control? #x00))
(want #t (ascii-control? #x1f))
(want #f (ascii-control? #x20))
(want #f (ascii-control? #x7e))
(want #t (ascii-control? #x7f))
(want #f (ascii-control? #x80))

(want 0 (ascii-digit-value #\0 10))
(want 0 (ascii-digit-value #\0 1))
(want #f (ascii-digit-value #\0 0))
(want #f (ascii-digit-value #\0 -1))
(want 7 (ascii-digit-value #\7 8))
(want #f (ascii-digit-value #\7 7))
(want #f (ascii-digit-value #\: 10))

(want 0 (ascii-upper-case-value #\A 0 26))
(want 25 (ascii-upper-case-value #\Z 0 26))
(want #f (ascii-upper-case-value #\Z 0 25))

(want 0 (ascii-lower-case-value #\a 0 26))
(want 25 (ascii-lower-case-value #\z 0 26))
(want #f (ascii-lower-case-value #\z 0 25))

(want 0 (ascii-lower-case-value #\a 0 1))
(want #f (ascii-lower-case-value #\a 0 0))
(want #f (ascii-lower-case-value #\a 0 -1))
(want 9001 (ascii-lower-case-value #\b 9000 2))

(want #f (ascii-nth-digit -1))
(want #\0 (ascii-nth-digit 0))
(want #\9 (ascii-nth-digit 9))
(want #f (ascii-nth-digit 10))

(want #\Z (ascii-nth-upper-case -1))
(want #\A (ascii-nth-upper-case 0))
(want #\Z (ascii-nth-upper-case 25))
(want #\A (ascii-nth-upper-case 26))

(want #\z (ascii-nth-lower-case -1))
(want #\a (ascii-nth-lower-case 0))
(want #\z (ascii-nth-lower-case 25))
(want #\a (ascii-nth-lower-case 26))

(define (count-matching predicates value)
  (let loop ((ps predicates) (n 0))
    (if (null? ps) n (loop (cdr ps) (if ((car ps) value) (+ n 1) n)))))

(define (union? whole . parts)
  (let check ((cc 0))
    (or (= cc #x80)
        (if (and (whole cc) (not (= 1 (count-matching parts cc))))
            #f (check (+ cc 1))))))

(define (subset? small-set . bigger-sets)
  (let check ((cc 0))
    (or (= cc #x80)
        (if (and (small-set cc) (= 0 (count-matching bigger-sets cc)))
            #f (check (+ cc 1))))))

(define (disjoint? . predicates)
  (let check ((cc 0))
    (or (= cc #x80) (and (<= (count-matching predicates cc) 1)
                         (check (+ cc 1))))))

(define (decimal-numeric? x) (ascii-numeric? x 10))

(want #t (union? ascii-alphanumeric? ascii-alphabetic? decimal-numeric?))
(want #t (union? ascii-alphabetic? ascii-upper-case? ascii-lower-case?))

(want #t (subset? ascii-space-or-tab?  ascii-whitespace?))
(want #t (subset? ascii-punctuation?   ascii-display?))
(want #t (subset? ascii-upper-case?    ascii-alphabetic? ascii-display?))
(want #t (subset? ascii-lower-case?    ascii-alphabetic? ascii-display?))
(want #t (subset? ascii-alphabetic?    ascii-alphanumeric? ascii-display?))
(want #t (subset? decimal-numeric?     ascii-alphanumeric? ascii-display?))
(want #t (subset? ascii-alphanumeric?  ascii-display?))

(want #t (disjoint? ascii-control? ascii-display?))
(want #t (disjoint? ascii-whitespace?
                    ascii-punctuation?
                    ascii-upper-case?
                    ascii-lower-case?
                    decimal-numeric?))
(want #t (disjoint? ascii-control?
                    ascii-punctuation?
                    ascii-upper-case?
                    ascii-lower-case?
                    decimal-numeric?))
