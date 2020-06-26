(require 'apigee-man-api)




(defun apigee-project-zip-api-proxy ()
  (let ((zip-dir (locate-dominating-file
                  default-directory
                  "apiproxy"))
        ;; TODO uniqify zip-file-name?
        (zip-file-name (format "%sapiproxy.zip"
                               (temporary-file-directory))))

    (unless zip-dir
      (user-error "Not inside an apiproxy directory"))

    (when (file-exists-p zip-file-name)
      (delete-file zip-file-name)
      (message "Deleting %s" zip-file-name))

    ;; Zip directory contents.
    (let ((default-directory zip-dir))
      (call-process "zip" nil nil nil "-r" zip-file-name "apiproxy")
      (message default-directory))
    zip-file-name))


(defun apigee-project-api-proxy-name ()
  "identity-service-internal-dev-apm-1010-reduce-default-refresh-token-expiry")

(defun apigee-project-import-api-proxy (arg)
  "Validate the API proxy.

If called with prefix ARG. then actually import it."
  (interactive "P")

  (let* ((zip-file-name (apigee-project-zip-api-proxy))
         (action (if arg "import" "validate"))
         (response-data (apigee-man-api-import-api
                         apigee-project-organization
                         (apigee-project-api-proxy-name)
                         zip-file-name
                         :action action)))
    (pp response-data)))


(defcustom apigee-project-organization nil
  "The organization associated with this project."
  :group 'apigee
  :type 'string)

(define-minor-mode apigee-project-mode
  "Toggle Apigee Project mode."
  :init-value nil
  :lighter " Apigee"
  :keymap  (let ((map (make-sparse-keymap)))
             (define-key map (kbd "\C-x \C-a i") 'apigee-project-import-api-proxy)
             map)
  :group 'apigee)


(provide 'apigee-project)
;;; apigee-project.el ends here
