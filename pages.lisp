(in-package #:parenworks)

;;; Home Page
(define-page home "parenworks/" (:clip "home.ctml")
  (r-clip:process T))

;;; About Page
(define-page about "parenworks/about" (:clip "about.ctml")
  (r-clip:process T))

;;; Services Page
(define-page services "parenworks/services" (:clip "services.ctml")
  (r-clip:process T))

;;; Contact Page
(define-page contact "parenworks/contact" (:clip "contact.ctml")
  (r-clip:process T))

;;; Route to make the site accessible at parenworks.systems
;;; Radiance parses parenworks.systems as domains ("systems" "parenworks")
;;; We need to map this to the parenworks module subdomain
(define-route parenworks-domain :mapping (uri) ()
  (when (or (null (domains uri))
            (equal '("systems" "parenworks") (domains uri))
            (equal '("parenworks") (domains uri)))
    (setf (domains uri) '("parenworks")))
  uri)

(define-route parenworks-domain :reversal (uri) ()
  (when (equal '("parenworks") (domains uri))
    (setf (domains uri) NIL))
  uri)
