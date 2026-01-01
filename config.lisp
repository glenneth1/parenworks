;;;; ParenWorks Configuration
;;;; 
;;;; This file contains configuration for the ParenWorks website.
;;;; Load this after loading Radiance but before starting up.

(in-package #:rad-user)

;;; Port Configuration
;;; Default is 3000 to avoid conflict with other services on 8080
(defvar *parenworks-port* 3000
  "The port on which the ParenWorks website runs.")

;;; Server Configuration
;;; 
;;; To use a server other than Hunchentoot, uncomment one of the following
;;; before calling (radiance:startup)

;; Use Woo server (high-performance, requires libev)
;; (setf (mconfig :radiance-core :interfaces :server) "r-woo")

;; Use Clack (flexible, supports multiple backends)
;; (setf (mconfig :radiance-core :interfaces :server) "r-clack")

;;; Debug Mode
;;; Set to T during development to get interactive debugger on errors
;; (setf radiance:*debugger* T)

;;; Logging
;;; Uncomment to enable verbose logging
;; (ql:quickload :verbose)
;; (setf (v:repl-level) :debug)
