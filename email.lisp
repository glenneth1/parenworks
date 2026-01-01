(in-package #:parenworks)

;;; Email sending via Resend API

(defvar *resend-api-key* nil
  "Resend API key, loaded from secrets file on server.")

(defvar *contact-email* "hello@parenworks.systems"
  "Email address to receive contact form submissions.")

(defvar *from-email* "contact@send.parenworks.systems"
  "Email address to send from (must be verified in Resend).")

(defun load-resend-api-key ()
  "Load the Resend API key from the secrets file."
  (let ((key-file "/opt/parenworks/secrets/resend-api-key"))
    (when (probe-file key-file)
      (with-open-file (stream key-file :direction :input)
        (setf *resend-api-key* (string-trim '(#\Space #\Newline #\Return) 
                                            (read-line stream nil ""))))
      (format t "~&Loaded Resend API key~%"))))

(defun escape-json-string (str)
  "Escape special characters for JSON string."
  (with-output-to-string (out)
    (loop for char across str do
      (case char
        (#\" (write-string "\\\"" out))
        (#\\ (write-string "\\\\" out))
        (#\Newline (write-string "\\n" out))
        (#\Return (write-string "\\r" out))
        (#\Tab (write-string "\\t" out))
        (otherwise (write-char char out))))))

(defun send-email (to subject body &key (from *from-email*))
  "Send an email via Resend API."
  (unless *resend-api-key*
    (error "Resend API key not configured"))
  (let* ((escaped-body (escape-json-string body))
         (escaped-subject (escape-json-string subject))
         (payload (format nil "{\"from\":\"~A\",\"to\":[\"~A\"],\"subject\":\"~A\",\"text\":\"~A\"}"
                          from to escaped-subject escaped-body))
         (result (drakma:http-request "https://api.resend.com/emails"
                                      :method :post
                                      :content-type "application/json"
                                      :additional-headers 
                                      `(("Authorization" . ,(format nil "Bearer ~A" *resend-api-key*)))
                                      :content payload))
         (result-string (if (stringp result) result (babel:octets-to-string result))))
    (format t "~&Resend API response: ~A~%" result-string)
    (if (search "\"id\":" result-string)
        (progn
          (format t "~&Email sent successfully to ~A~%" to)
          t)
        (progn
          (format t "~&Email send failed: ~A~%" result-string)
          nil))))

(defun send-contact-form-email (name email message)
  "Send a contact form submission email."
  (let ((subject (format nil "ParenWorks Contact: ~A" name))
        (body (format nil "New contact form submission:~%~%Name: ~A~%Email: ~A~%~%Message:~%~A"
                      name email message)))
    (send-email *contact-email* subject body)))

;; Load API key when this file is loaded
(load-resend-api-key)
