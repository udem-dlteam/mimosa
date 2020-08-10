;;;============================================================================

;;; File: "_syntax-boot.scm"

;;; Copyright (c) 2000-2015 by Marc Feeley, All Rights Reserved.

;;;============================================================================

;; This file implements an unhygienic version of the (syntax-case ...)
;; and (syntax ...) forms that are used for bootstrapping.

;;;----------------------------------------------------------------------------

;; needed by expansion of syntax-case and syntax forms
(include "_syntax-pattern.scm")
(include "_syntax-template.scm")
(include "_syntax-common.scm")

;;;----------------------------------------------------------------------------

(##define-syntax syntax-case
  (lambda (src)
    (##include "_syntax-case-xform-boot.scm")
    (syn#syntax-case-form-transformer src)))

(##define-syntax syntax
  (lambda (src)
    (##include "_syntax-xform-boot.scm")
    (syn#syntax-form-transformer src '())))

;;;============================================================================
