(in-package #:rad-user)

(define-module #:parenworks
  (:use #:cl #:radiance)
  (:export #:*port*
           #:*resend-api-key*
           #:*contact-email*
           #:compile-stylesheets
           #:compile-scripts
           #:load-resend-api-key
           #:send-contact-form-email))

(in-package #:parenworks)

(defvar *port* 3000
  "The port on which the ParenWorks website runs.")

(defun compile-stylesheets ()
  "Compile LASS stylesheets to CSS."
  (let ((lass-file (asdf:system-relative-pathname :parenworks "static/parenworks.lass")))
    (when (probe-file lass-file)
      (lass:generate lass-file)
      (format t "~&Compiled LASS: ~A~%" lass-file))))

(defun compile-scripts ()
  "Compile Parenscript to JavaScript."
  (let ((paren-file (asdf:system-relative-pathname :parenworks "static/parenworks.paren"))
        (js-file (asdf:system-relative-pathname :parenworks "static/parenworks.js")))
    (when (probe-file paren-file)
      (with-open-file (out js-file :direction :output :if-exists :supersede)
        (write-string (ps:ps-compile-file paren-file) out))
      (format t "~&Compiled Parenscript: ~A~%" paren-file))))

(compile-stylesheets)
(compile-scripts)
;; load-resend-api-key is called from email.lisp after it's defined
