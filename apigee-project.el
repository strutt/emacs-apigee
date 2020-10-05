;;; -*- lexical-binding: t; -*-

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
    (copy-directory (concat src-dir "apiproxy/")
                    dest-dir
                    t nil t)

    ;; "Compile" the project, i.e. replace all instances of template
    ;; keys with values.
    (let* ((file-names (directory-files-recursively dest-dir ".+" ))
           (recentf-exclude file-names))
      (save-excursion
        (mapc (lambda (file-name)
                (with-temp-buffer
                  (insert-file-contents-literally file-name)
                  (apigee-jinja2-render)
                  (write-file file-name)))
              file-names)))

    ;; Zip directory contents.
    (let ((default-directory (temporary-file-directory)))
      (call-process "zip" nil nil nil "-r" zip-file-name "apiproxy"))

    ;; Return zip file name
    zip-file-name))


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
         (uploaded-revision (when arg
                              (string-to-number (alist-get 'revision response-data)))))
    (delete-file zip-file-name)
    (delete-directory
     (concat (file-name-directory zip-file-name)
             (file-name-base zip-file-name))
     t)
    (when uploaded-revision
      (message "Uploaded new revision: %d" uploaded-revision)
      (when (y-or-n-p (format "Deploy new revision %d? " uploaded-revision
                              apigee-project--environment))
        (let* ((env (apigee-project-select-environment
                     (format "Deploy new revision %d to: " uploaded-revision)))
               (deployments (apigee-man-api-get-api-deployments-in-environment
                             apigee-project-organization
                             env
                             apigee-project-api)))

          ;; Undeploy all deployed revision in `env'
          (seq-do
           (lambda (deployment)
             (let ((revision (string-to-number (alist-get 'name deployment))))
               (apigee-man-api-undeploy-api-revision
                apigee-project-organization
                env
                apigee-project-api
                revision)))
           (alist-get 'revision deployments))

          ;; Deploy new revision to `env'
          (apigee-man-api-deploy-api-revision
           apigee-project-organization
           env
           apigee-project-api
           uploaded-revision))))))

(defun apigee-project-deploy-api-proxy (revision)
  "Deploy your API at REVISION.

If `apigee-project-environment' is defined then use
that. Otherwise prompt user to choose from environments in
`apigee-project-organization'."
  (interactive)
  (unless (integerp revision)
    (user-error (format "Invalid revision %s" revision)))

  (let* ((env apigee-project--environment)
         (envs (apigee-man-api-list-environment-names apigee-project-organization)))
    (unless (seq-contains envs env 'string-equal)
      (setq env (completing-read (format "Select environment to deploy %s to: " apigee-project-api)  envs nil t)))

    (let ((response-data (apigee-man-api-deploy-api-revision
                          apigee-project-organization
                          env
                          apigee-project-api
                          revision)))
      (pp response-data))))

(defcustom apigee-project-organization nil
  "The organization associated with this project.")

(defvar apigee-project--environment nil
  "Default environment to deploy to.")

(defun apigee-project-select-environment (prompt &optional organization)
  "PROMPT user to select an environment in ORGANIZATION."
  (unless organization
    (setq organization apigee-project-organization))
  
  (let ((env (completing-read
              prompt
              (apigee-project-environments organization)
              nil t apigee-project--environment)))

    (setq apigee-project--environment env)))

(defvar apigee-project-api nil
  "The name of the API proxy under development.")

(defvar apigee-project--environments nil
  "Alist of environments available per apigee organization.
See `apigee-project-environments'.")

(defun apigee-project-environments (&optional organization)
  "List of environments that exist in ORGANIZATION.

This is the recommended way to access the list of environments
available in the organization.

The environments are cached per-organization after the first API
call.  To remove the cached entry edit
`apigee-project--environments' to remove the alist key or set the
whole alist to nil."
  (unless organization
    (setq organization apigee-project-organization))

  (or (alist-get organization apigee-project--environments)
      (let* ((envs (sort (apigee-man-api-list-environment-names organization)
                         'string-lessp))
             (envs (seq-into envs 'list)))
        (setq apigee-project--environments
              (cons `(,organization . ,envs) apigee-project--environments))
        envs)))


(define-minor-mode apigee-project-mode
  "Toggle Apigee Project mode."
  :init-value nil
  :lighter " Apigee"
  :keymap  (let ((map (make-sparse-keymap)))
             (define-key map (kbd "\C-x \C-a \C-u") 'apigee-project-import-api-proxy)
             (define-key map (kbd "\C-x \C-a \C-d") 'apigee-project-deployment)
             map)
  :group 'apigee)



;;;; apigee-project-deployment stuff

(defvar apigee-project-deployment-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "a") 'apigee-project-deployment-deploy-marked)
    (define-key map (kbd "r") 'apigee-project-deployment-undeploy-marked)
    map))

(define-derived-mode apigee-project-deployment-mode
  tablist-mode
  "apd"
  "Apigee Project Deployment"
  (setq-local tablist-operations-function 'apigee-project-deployment--operate))


(defun apigee-project-deployment ()
  "See deployment status of project."
  (interactive)

  (unless (and apigee-project-organization
               apigee-project-api)
    (error "Variables apigee-project-organization and apigee-project-api must be set"))
  
  (let* ((org apigee-project-organization)
         (api apigee-project-api)
         (revisions (apigee-man-api-get-api-revisions org api))
         (envs (apigee-project-environments org)))

    (pop-to-buffer "*Deployment*")
    (apigee-project-deployment-mode)

    ;; In case these are per-project dir-local variables.
    (setq-local apigee-project-organization org)
    (setq-local apigee-project-api api)
    (setq-local apigee-project--environments (seq-map 'identity envs))

    (setq tabulated-list-format
          (let ((tlf '(("Rev" 3 (lambda (a b) (< (car a) (car b)))))))
            (apply 'vector
                   (append
                    tlf
                    (mapcar (lambda (env)
                              (list env
                                    (max (length "Deployed")
                                         (length env))
                                    t))
                            envs)))))
    (setq tabulated-list-padding 1)
    (setq tabulated-list-sort-key (cons "Rev" nil))
    (tabulated-list-init-header)
    (setq tabulated-list-entries
          (mapcar (lambda (rev)
                    (let ((row (list (string-to-number rev)
                                     (make-vector (length tabulated-list-format) ""))))
                      (aset (nth 1 row) 0 rev)
                      row))
                  revisions))
    (tabulated-list-print t)

    (mapc ;; TODO - asyncify
     (lambda (rev)
       (let* ((deployments (alist-get 'environments (apigee-man-api-get-api-revision-deployments org api rev))))
         (with-current-buffer "*Deployment*"
           
           )
         )
       )
     (mapcar 'car tabulated-list-entries)
     ))
  (goto-char (point-min)))


(add-to-list 'tabulated-list-revert-hook 'apigee-project-deployment--maybe-refresh)

(defvar-local apigee-project-deployment--refresh nil)

(defun apigee-project-deployment--maybe-refresh ()
  "Refresh if required."
  (when apigee-project-deployment--refresh
    (apigee-project-deployment)
    (setq apigee-project-deployment--refresh nil)))

(defun apigee-project-deployment-undeploy-marked ()
  "Undeploy all marked revisions, ARG."
  (interactive)
  (tablist-do-operation current-prefix-arg
                        'apigee-project-deployment--undeploy-marked
                        "Undeploy revision"
                        nil t))

(defun apigee-project-deployment-deploy-marked ()
  "Deploy all marked revisions, ARG."
  (interactive)
  (tablist-do-operation current-prefix-arg
                        'apigee-project-deployment--deploy-marked
                        "Deploy revision"
                        nil t))

(defun apigee-project-deployment--get-entry-by-id (id)
  "Get entry in `tabulated-list-entries' by ID."
  (let ((entry (seq-find (lambda (entry_)
                           (equal (car entry_) id))
                         tabulated-list-entries)))
    (unless entry
      (error "No entry with id %s" id))
    entry))

(defun apigee-project-deployment--undeploy-marked (id &optional entry)
  "Undeploy ENTRY marked by ID."
  (interactive)
  (unless entry
    (setq entry (apigee-project-deployment--get-entry-by-id id)))
  (mapc (lambda (env)
          (let ((state (aref (nth 1 entry)
                             (tabulated-list--column-number env))))
            (when (string-equal "Deployed" state)
              (apigee-man-api-undeploy-api-revision apigee-project-organization
                                                    env
                                                    apigee-project-api
                                                    id)
              (setq apigee-project-deployment--refresh t))))
        apigee-project--environments))

(defun apigee-project-deployment--deploy-marked (id &optional entry)
  "Deploy ENTRY marked by ID."
  (interactive)
  (unless entry
    (setq entry (apigee-project-deployment--get-entry-by-id id)))
  
  (let* ((envs (completing-read-multiple "Select environments: "
                                         apigee-project--environments
                                         nil
                                         ;; nil
                                         t
                                         nil
                                         t
                                         )))
    (mapc (lambda (env)
            (apigee-man-api-deploy-api-revision apigee-project-organization
                                                env
                                                apigee-project-api
                                                id)
            (setq apigee-project-deployment--refresh t))
          envs)))

(defun apigee-project-deployment--operate (operation &rest args)
  "Do OPERATION on selected entries encoded in ARGS."
  (cond ((eq operation 'supported-operations)
         '(delete) )
        ((eq operation 'delete)
         (apigee-project-deployment--delete-marked (car args)))))

(defun apigee-project-deployment--delete-marked (ids)
  "Delete entries with IDS."
  (mapc (lambda (revision)
          (apigee-man-api-delete-api-revision apigee-project-organization
                                              apigee-project-api
                                              revision)
          (setq-local apigee-project-deployment--refresh t))
        ids))

(provide 'apigee-project)
;;; apigee-project.el ends here
