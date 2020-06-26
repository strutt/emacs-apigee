(require 'apigee-man-api)

(defun apigee-project--build ()
  "Assemble the zipped apiproxy ready for upload."
  (let ((src-dir (locate-dominating-file
                  default-directory
                  "apiproxy"))
        (dest-dir (concat (temporary-file-directory) "apiproxy"))
        (templates apigee-project-template-values)
        (zip-file-name (format "%sapiproxy.zip"
                               (temporary-file-directory))))

    (unless src-dir
      (user-error "Not inside an apiproxy directory"))

    (when (file-exists-p zip-file-name)
      (delete-file zip-file-name)
      (message "Deleting %s" zip-file-name))

    (when (file-exists-p dest-dir)
      (delete-directory dest-dir t)
      (message "Deleting %s" dest-dir))

    (dired-copy-file-recursive (concat src-dir "apiproxy")
                               dest-dir
                               nil nil nil 'always)

    (let* ((file-names (directory-files-recursively dest-dir ".+" ))
           (recentf-exclude file-names) ;; Prevent flooding!
           )
      (save-excursion
        (mapc (lambda (file-name)
                (with-temp-buffer
                  (insert-file-contents file-name)
                  (mapc (lambda (template)
                          (goto-char (point-min))
                          (replace-string (car template) (cdr template)))
                        templates)
                  (write-file file-name)))
              file-names)))


    ;; Zip directory contents.
    (let ((default-directory (temporary-file-directory)))
      (call-process "zip" nil nil nil "-r" zip-file-name "apiproxy"))

    ;; Return zip file name
    zip-file-name))


(defun apigee-project-api-proxy-name ()
  "identity-service-internal-dev-apm-1010-reduce-default-refresh-token-expiry")

(defun apigee-project-import-api-proxy (arg)
  "Validate the API proxy.

yIf called with prefix ARG. then actually import it."
  (interactive "P")

  (let* ((zip-file-name (apigee-project--build))
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

(defcustom apigee-project-template-values nil
  "List of template values used to build the project."
  :group 'apigee
  :type 'alist
  )

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
