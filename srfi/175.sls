#!r6rs
;; Automatically generated
(library (srfi :175)
         (export ascii-char?
                 ascii-control?
                 ascii-display?
                 ascii-whitespace?
                 ascii-space-or-tab?
                 ascii-punctuation?
                 ascii-upper-case?
                 ascii-lower-case?
                 ascii-alphabetic?
                 ascii-alphanumeric?
                 ascii-numeric?
                 ascii-digit-value
                 ascii-upper-case-value
                 ascii-lower-case-value
                 ascii-nth-digit
                 ascii-nth-upper-case
                 ascii-nth-lower-case
                 ascii-upcase
                 ascii-downcase
                 ascii-control->display
                 ascii-display->control
                 ascii-digits
                 ascii-lower-case
                 ascii-upper-case
                 ascii-punctuation)
         (import (rnrs))
         (define (ensure-int x) (if (char? x) (char->integer x) x))
         (define (base-offset-limit x base offset limit)
           (let ((cc (ensure-int x)))
             (and (fx>=? cc base)
                  (fx<? cc (fx+ base limit))
                  (fx+ offset (fx- cc base)))))
         (define (ascii-char? x) (and (char? x) (fx<? (char->integer x) 128)))
         (define (ascii-control? x)
           (let ((cc (ensure-int x))) (or (fx<=? 0 cc 31) (fx=? cc 127))))
         (define (ascii-display? x)
           (let ((cc (ensure-int x))) (fx<=? 32 cc 126)))
         (define (ascii-whitespace? x)
           (let ((cc (ensure-int x)))
             (cond ((fx<? cc 9) #f) ((fx<? cc 14) #t) (else (fx=? cc 32)))))
         (define (ascii-space-or-tab? x)
           (let ((cc (ensure-int x))) (case cc ((9 32) #t) (else #f))))
         (define (ascii-punctuation? x)
           (let ((cc (ensure-int x)))
             (or (fx<=? 33 cc 47)
                 (fx<=? 58 cc 64)
                 (fx<=? 91 cc 96)
                 (fx<=? 123 cc 126))))
         (define (ascii-upper-case? x)
           (let ((cc (ensure-int x))) (fx<=? 65 cc 90)))
         (define (ascii-lower-case? x)
           (let ((cc (ensure-int x))) (fx<=? 97 cc 122)))
         (define (ascii-alphabetic? x)
           (let ((cc (ensure-int x))) (or (fx<=? 65 cc 90) (fx<=? 97 cc 122))))
         (define (ascii-alphanumeric? x)
           (let ((cc (ensure-int x)))
             (or (fx<=? 48 cc 57) (fx<=? 65 cc 90) (fx<=? 97 cc 122))))
         (define (ascii-numeric? x radix)
           (not (not (ascii-digit-value x radix))))
         (define (ascii-digit-value x limit)
           (base-offset-limit x 48 0 (min limit 10)))
         (define (ascii-upper-case-value x offset limit)
           (base-offset-limit x 65 offset (min limit 26)))
         (define (ascii-lower-case-value x offset limit)
           (base-offset-limit x 97 offset (min limit 26)))
         (define (ascii-nth-digit n)
           (and (fx<=? 0 n 9) (integer->char (fx+ 48 n))))
         (define (ascii-nth-upper-case n)
           (integer->char (fx+ 65 (fxmod n 26))))
         (define (ascii-nth-lower-case n)
           (integer->char (fx+ 97 (fxmod n 26))))
         (define (ascii-upcase x)
           (if (char? x)
               (integer->char (ascii-upcase (char->integer x)))
               (or (ascii-lower-case-value x 65 26) x)))
         (define (ascii-downcase x)
           (if (char? x)
               (integer->char (ascii-downcase (char->integer x)))
               (or (ascii-upper-case-value x 97 26) x)))
         (define (ascii-control->display x)
           (if (char? x)
               (let ((x (ascii-control->display (char->integer x))))
                 (and x (integer->char x)))
               (or (and (fx<=? 0 x 31) (fx+ x 64)) (and (fx=? x 127) 63))))
         (define (ascii-display->control x)
           (if (char? x)
               (let ((x (ascii-display->control (char->integer x))))
                 (and x (integer->char x)))
               (or (and (fx<=? 64 x 95) (fx- x 64)) (and (fx=? x 63) 127))))
         (define ascii-digits "0123456789")
         (define ascii-lower-case "abcdefghijklmnopqrstuvwxyz")
         (define ascii-upper-case "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
         (define ascii-punctuation "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~"))
