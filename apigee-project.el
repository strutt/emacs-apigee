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

    ;; Copy the source tree to tempdir.
    (dired-copy-file-recursive (concat src-dir "apiproxy")
                               dest-dir
                               nil nil nil 'always)

    ;; "Compile" the project, i.e. replace all instances of template
    ;; keys with values.
    (let* ((file-names (directory-files-recursively dest-dir ".+" ))
           (recentf-exclude file-names))
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

If called with prefix ARG. then actually import it.  Returns the
revision of the proxy just imported if called with ARG, otherwise
returns nil."
  (interactive "P")
  (let* ((zip-file-name (apigee-project--build))
         (action (if arg "import" "validate"))
         (response-data (apigee-man-api-import-api
                         apigee-project-organization
                         apigee-project-api
                         zip-file-name
                         :action action))
         (revision (when arg
                     (alist-get 'revision response-data))))
    (delete-file zip-file-name)
    (delete-directory
     (concat (file-name-directory zip-file-name)
             (file-name-base zip-file-name))
     t)
    
    revision))

(defun apigee-project-deploy-api-proxy (revision)
  "Deploy your API at REVISION.

If `apigee-project-environment' is defined then use
that. Otherwise prompt user to choose from environments in
`apigee-project-organization'."
  (interactive)
  (unless (integerp revision)
    (user-error (format "Invalid revision %s" revision)))

  (let* ((env apigee-project-environment)
         (envs (apigee-man-api-list-environment-names apigee-project-organization)))
    (unless (seq-contains envs env 'string-equal)
      (setq env (completing-read (format "Select environment to deploy %s to: " apigee-project-api)  envs nil t)))

    (let ((response-data (apigee-man-api-deploy-api-revision
                          apigee-project-organization
                          env
                          apigee-project-api
                          revision)))
      (pp response-data))))

(defcustom apigee-project-api nil
  "The name of the API proxy."
  :group 'apigee
  :type 'string)

(defcustom apigee-project-environment nil
  "The environment associated with this project."
  :group 'apigee
  :type 'string)

(defcustom apigee-project-organization nil
  "The organization associated with this project."
  :group 'apigee
  :type 'string)

(defcustom apigee-project-template-values nil
  "List of template values used to build the project."
  :group 'apigee
  :type 'alist)

(define-minor-mode apigee-project-mode
  "Toggle Apigee Project mode."
  :init-value nil
  :lighter " Apigee"
  :keymap  (let ((map (make-sparse-keymap)))
             (define-key map (kbd "\C-x \C-a i") 'apigee-project-import-api-proxy)
             map)
  :group 'apigee)



;;;; apigee-project-deployment stuffs






(define-derived-mode apigee-project-deployment-mode
  tablist-mode
  "APD"
  "Apigee Project Deployment"
  )

(defun apigee-project-deployment ()
  "See deployment status of project."
  (interactive)
  
  (let* ((org apigee-project-organization)
         (env apigee-project-environment)
         (api apigee-project-api)
         (revisions (apigee-man-api-get-api-revisions org api))
         (envs (sort (apigee-man-api-list-environment-names org)
                     'string-lessp)))
    (pop-to-buffer "*Deployment*")
    (apigee-project-deployment-mode)
    (setq tabulated-list-format
          (let ((tlf '(("Rev" 3 t))))
            (apply 'vector
                   (append
                    tlf
                    (mapcar (lambda (env)
                              (list env
                                    (max (length "deployed")
                                         (length env))
                                    t))
                            envs)))))
    (setq tabulated-list-padding 1)
    (setq tabulated-list-sort-key (cons "Rev" nil))
    (pp tabulated-list-format)
    (tabulated-list-init-header)
    (setq tabulated-list-entries
          (mapcar (lambda (rev)
                    (let ((row (list rev (make-vector (length tabulated-list-format) ""))))
                      (aset (nth 1 row) 0 rev)
                      row))
                  revisions)))
  (pp tabulated-list-entries)
  (tabulated-list-print t))





















(provide 'apigee-project)
;;; apigee-project.el ends here
