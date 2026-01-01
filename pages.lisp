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
     "{\"status\":\"success\",\"message\":\"Thank you for your message! I'll get back to you soon.\"}"))))

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
