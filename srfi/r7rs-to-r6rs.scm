(import (scheme base) (scheme file) (scheme read) (scheme write) (srfi 1))

(cond-expand (chibi (import (chibi show) (chibi show pretty)))
             (else))

(define substitutions
  '((= . fx=?) (< . fx<?) (<= . fx<=?) (> . fx>?) (>= . fx>=?)
    (+ . fx+) (- . fx-) (modulo . fxmod)
    (open-binary-input-file . open-file-input-port) (read-u8 . get-u8)))

(define (substitute form)
  (if (pair? form)
      (cons (substitute (car form))
            (substitute (cdr form)))
      (let ((sub (assoc form substitutions)))
        (if sub (cdr sub) form))))

(define (pretty-print out form)
  (cond-expand
    (chibi (show out (pretty form)))
    (else (write form out) (newline out))))

(define (read-all port)
  (let loop ((forms '()))
    (let ((form (read port)))
      (if (eof-object? form) forms (loop (append forms (list form)))))))

(define (r7rs->r6rs filename)
  (substitute
   (remove (lambda (form) (eqv? 'import (car form)))
           (call-with-input-file filename read-all))))

(define (write-r6rs-file filename . forms)
  (call-with-output-file filename
    (lambda (out)
      (display "#!r6rs\n;; Automatically generated\n" out)
      (for-each (lambda (form) (pretty-print out form)) forms))))

(define (translate-source-file r7rs-filename r6rs-filename)
  (apply write-r6rs-file r6rs-filename
         (cons '(import (rnrs) (ascii))
               (r7rs->r6rs r7rs-filename))))

(define (main)
  (let* ((lib (call-with-input-file "ascii.sld" read))
         (libname (cadr lib))
         (exports (assoc 'export (cddr lib))))
    (write-r6rs-file "ascii.sls"
                     `(library ,libname
                        ,exports
                        (import (rnrs))
                        ,@(r7rs->r6rs "ascii.scm")))
    (translate-source-file "examples.scm" "examples.sps")
    (translate-source-file "tests.scm" "tests.sps")))

(main)
