;;;; package.lisp

(defpackage #:shf-error-handling
  (:use #:cl #:ftw) ;#:iterate)
  (:nicknames  #:shf-error)
  (:export #:error-message #:try-retry  #:release-error))
