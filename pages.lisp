(in-package #:parenworks)

;;; Home Page - accessible at root and /parenworks/
(define-page home "/" (:clip "home.ctml")
  (r-clip:process T))

(define-page home-alt "parenworks/" (:clip "home.ctml")
  (r-clip:process T))

;;; About Page
(define-page about "/about" (:clip "about.ctml")
  (r-clip:process T))

(define-page about-alt "parenworks/about" (:clip "about.ctml")
  (r-clip:process T))

;;; Services Page
(define-page services "/services" (:clip "services.ctml")
  (r-clip:process T))

(define-page services-alt "parenworks/services" (:clip "services.ctml")
  (r-clip:process T))

;;; Contact Page
(define-page contact "/contact" (:clip "contact.ctml")
  (r-clip:process T))

(define-page contact-alt "parenworks/contact" (:clip "contact.ctml")
  (r-clip:process T))

;;; Contact Form API Endpoint
(define-api parenworks/contact/submit (name email message) ()
  "Handle contact form submission."
  (cond
    ((or (null name) (string= name ""))
     (error 'api-error :message "Name is required"))
    ((or (null email) (string= email ""))
     (error 'api-error :message "Email is required"))
    ((or (null message) (string= message ""))
     (error 'api-error :message "Message is required"))
    (t
     (send-contact-form-email name email message)
     (setf (content-type *response*) "application/json")
     "{\"status\":\"success\",\"message\":\"Thank you for your message! I'll get back to you soon.\"}")))


;;; Route to make the site accessible at parenworks.systems
;;; When parenworks.systems is in the :domains config, Radiance recognizes it
;;; and domains will be NIL. We need to map this to the parenworks module subdomain.
(define-route parenworks-domain :mapping (uri) ()
  ;; Always route to parenworks module when domains is empty or already parenworks
  (when (or (null (domains uri))
            (equal '("systems" "parenworks") (domains uri)))
    (setf (domains uri) '("parenworks")))
  uri)

(define-route parenworks-domain :reversal (uri) ()
  (when (equal '("parenworks") (domains uri))
    (setf (domains uri) NIL))
  uri)
