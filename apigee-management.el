;; TODO

(require 'apigee-management-api)
(require 'apigee-management-kvms)

(defun apigee-management--list-key-value-maps ()
  (interactive)
    ;; aref indices must match `tabulated-list-format'
  (let* ((api-name (aref (tabulated-list-get-entry) 2))
         (api-kvms (mapcar (lambda (kvm-name)
                             (apigee-management-api--get-kvm kvm-name :api api-name))
                           (apigee-management-api--list-kvms :api api-name)))
         (environment (aref (tabulated-list-get-entry) 1))
         (env-kvms (mapcar (lambda (kvm-name)
                             (apigee-management-api--get-kvm kvm-name :environment environment))
                           (apigee-management-api--list-kvms :environment environment)))
         (org-kvms (mapcar (lambda (kvm-name)
                             (apigee-management-api--get-kvm kvm-name :organization nil))
                           (apigee-management-api--list-kvms :organization nil))))

    (apigee-management-kvms api-kvms env-kvms org-kvms api-name environment)))

(defun apigee-management--api-metadata-timestamp-to-string (timestamp)
  "Remove the random factor of 1000 from TIMESTAMP and format nicely for GMT string."
  (format-time-string "%Y-%m-%dT%H:%M:%S" (seconds-to-time (/ timestamp 1000)) t))


(defun apigee-management--api-to-table-row (env-apis)
  "Convert ENV-APIS to tabulated list rows."
  (let* ((environment (alist-get 'name env-apis))
         (api-names (mapcar (lambda (proxy)
                              (alist-get 'name proxy))
                            (alist-get 'aPIProxy env-apis)))
         (apis (mapcar 'apigee-management-api--get-api api-names))
         (api-last-modified-ats (mapcar (lambda (api)
                                          (apigee-management--api-metadata-timestamp-to-string
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


(defun apigee-management ()
  "Entrypoint apigee management."
  (interactive)
  (let* ((environments (apigee-management-api--get-environment-names))
         (apis (mapcar
                'apigee-management-api--get-apis-deployed-to-environment
                environments))
         (rows-list (mapcar
                     'apigee-management--api-to-table-row
                     apis))
         (rows (apply 'append rows-list))
         )
    (pop-to-buffer (format "*apigee management: %s*" apigee-management-api-organization))
    (apigee-management-mode)
    (setq tabulated-list-entries rows)
    (tabulated-list-print t))
  (goto-char (point-min))
  )


(defvar apigee-management-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "k" 'apigee-management--list-key-value-maps)
    map
    )
  )





(define-derived-mode
  apigee-management-mode
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
