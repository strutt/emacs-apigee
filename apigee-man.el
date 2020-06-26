;;; -*- lexical-binding: t; -*-

(require 'apigee-man-api)
(require 'apigee-man-kvms)

(defcustom apigee-man-organizations nil
  "List of organizations you with to interact with.")


(defun apigee-man--list-key-value-maps ()
  (interactive)
  ;; aref indices must match `tabulated-list-format'
  (let* ((api-name (aref (tabulated-list-get-entry) 2))
         (api-kvms (apigee-man-api-list-kvms :api api-name))
         (environment (aref (tabulated-list-get-entry) 1))
         (env-kvms (apigee-man-api-list-kvms :environment environment))
         (org-kvms (apigee-man-api-list-kvms :organization t)))
    (apigee-man-kvms api-kvms env-kvms org-kvms api-name environment)))

(defun apigee-man--api-metadata-timestamp-to-string (timestamp)
  "Remove the random factor of 1000 from TIMESTAMP and format nicely for GMT string."
  (format-time-string "%Y-%m-%dT%H:%M:%S" (seconds-to-time (/ timestamp 1000)) t))


(defun apigee-man--api-to-table-row (env-apis)
  "Convert ENV-APIS to tabulated list rows."
  (let* ((environment (alist-get 'name env-apis))
         (api-names (mapcar (lambda (proxy)
                              (alist-get 'name proxy))
                            (alist-get 'aPIProxy env-apis)))
         (apis (mapcar 'apigee-man-api-get-api api-names))
         (api-last-modified-ats (mapcar (lambda (api)
                                          (apigee-man--api-metadata-timestamp-to-string
                                           (alist-get 'lastModifiedAt (alist-get 'metaData api))))
                                        apis))
         
         )
    (mapcar* (lambda (api-name api-last-modified-at)
               (list
                api-name ;; unique-id
                (vector ;; The order of this vector must match `tabulated-format-list'
                 
                 api-last-modified-at ;; Last Modified
                 environment ;; Environment
                 api-name ;; API name
                 )))
             api-names
             api-last-modified-ats
             )))


(defun apigee-man ()
  "Entrypoint apigee management."
  (interactive)
  (let* ((org (car apigee-man-organizations))
         (environments (apigee-man-api-list-environment-names org))
         (apis (mapcar
                (lambda (env)
                  apigee-man-api-get-apis-deployed-to-environment org env)
                org
                environments))
         (rows-list (mapcar
                     'apigee-man--api-to-table-row
                     apis))
         (rows (apply 'append rows-list))
         )
    (pop-to-buffer (format "*apigee management*"))
    (apigee-man-mode)
    (setq tabulated-list-entries rows)
    (tabulated-list-print t))
  (goto-char (point-min))
  )


(defvar apigee-man-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [?m] 'apigee-man--list-key-value-maps)
    map
    )
  )


(define-derived-mode
  apigee-man-mode
  tabulated-list-mode
  "apigee"
  "Manage Apigee from the one true editor."

  (setq tabulated-list-format [("Last Modified (GMT)" 20 t)
                               ("Environment" 20 t)
                               ("API" 100 t)
                               ])
  (setq tabulated-list-padding 1)
  (setq tabulated-list-sort-key (cons "Last Modified (GMT)" t))
  (tabulated-list-init-header))
