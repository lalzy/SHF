;;;; package.lisp

(defpackage #:shf-error-handling
  (:use #:cl)
  (:nicknames  #:shf-error)
  (:export #:error-message #:try-retry  #:release-error))
