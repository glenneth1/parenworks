;;;; ParenWorks Website Startup Script
;;;;
;;;; Load this file to start the ParenWorks website.
;;;; Usage: (load "start.lisp")
;;;;
;;;; To use a different port, set *parenworks-port* before loading:
;;;;   (defvar *parenworks-port* 3000)
;;;;   (load "start.lisp")

(in-package #:cl-user)

;;; Default port (can be overridden before loading this file)
(defvar *parenworks-port* 3001
  "The port on which the ParenWorks website runs. Default is 3001 for dev, 3000 for production.")

;;; Ensure Quicklisp is available
(unless (find-package :quicklisp)
  (error "Quicklisp is required. Please install Quicklisp first."))

;;; Install Shirakumo dist if not present
(handler-case
    (ql-dist:find-dist "shirakumo")
  (error ()
    (format t "~&Installing Shirakumo dist...~%")
    (ql-dist:install-dist "http://dist.shirakumo.org/shirakumo.txt")))

;;; Load Radiance
(format t "~&Loading Radiance...~%")
(ql:quickload :radiance :silent t)

;;; Register and load ParenWorks
(format t "~&Loading ParenWorks...~%")
(pushnew (make-pathname :directory (pathname-directory *load-truename*))
         asdf:*central-registry*
         :test #'equal)

;;; Handle first-time environment setup during load
(handler-bind ((radiance:environment-not-set
                 (lambda (c)
                   (declare (ignore c))
                   (invoke-restart 'continue))))
  (ql:quickload :parenworks :silent t))

;;; Stylesheets are compiled automatically when the module loads

;;; Handle environment setup during startup as well
(handler-bind ((radiance:environment-not-set
                 (lambda (c)
                   (declare (ignore c))
                   (invoke-restart 'continue))))
  
  ;;; Start the server
  (format t "~&~%")
  (format t "~&========================================~%")
  (format t "~&  (ParenWorks) Systems Website~%")
  (format t "~&========================================~%")
  (format t "~&~%")
  (let ((port (if (boundp 'parenworks:*port*) parenworks:*port* *parenworks-port*)))
    (format t "~&Starting Radiance server on port ~A...~%" port)
    
    ;; Start Radiance with the parenworks environment (uses port 3000)
    ;; Config is in ~/.config/radiance/parenworks/
    (radiance:startup "parenworks")
    
    (format t "~&~%")
    (format t "~&Website is now running at:~%")
    (format t "~&  http://localhost:~A/~%" port)
    (format t "~&~%")
    (format t "~&Pages:~%")
    (format t "~&  Home:     http://localhost:~A/~%" port)
    (format t "~&  About:    http://localhost:~A/about~%" port)
    (format t "~&  Services: http://localhost:~A/services~%" port)
    (format t "~&  Contact:  http://localhost:~A/contact~%" port))
  (format t "~&~%")
  (format t "~&To stop: (radiance:shutdown)~%")
  (format t "~&========================================~%"))
