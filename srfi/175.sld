(define-library (srfi 175)
  (import (scheme base))
  (export ascii-codepoint?
          ascii-bytevector?

          ascii-char?
          ascii-string?

          ascii-control?
          ascii-non-control?
          ascii-whitespace?
          ascii-space-or-tab?
          ascii-other-graphic?
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
          ascii-control->graphic
          ascii-graphic->control
          ascii-mirror-bracket

          ascii-ci=?
          ascii-ci<?
          ascii-ci>?
          ascii-ci<=?
          ascii-ci>=?

          ascii-string-ci=?
          ascii-string-ci<?
          ascii-string-ci>?
          ascii-string-ci<=?
          ascii-string-ci>=?)
  (include "175.scm"))
