;; TODO

(require 'apigee-admin-api)

(defvar apigee-management-mode-map
  (let ((map (make-sparse-keymap)))
    
    map
    )
  )


(define-derived-mode
  apigee-management-mode
  tabulated-list-mode
  "apigee"
  "Manage Apigee from the one true editor."

  (setq tabulated-list-format [("Environment" 18 t)
                               ("API" 12 nil)
                               ;; ("Col3"  10 t)
                               ;; ("Col4" 0 nil)
                               ])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key (cons "Environment" nil))
  (tabulated-list-init-header))
  


(defun apigee-management--api-to-table-row (env-apis)
  "Convert ENV-APIS to tabulated list rows."
  (let ((environment (alist-get 'name env-apis))
        (api-names (mapcar (lambda (api)
                             (alist-get 'name api))
                           (alist-get 'aPIProxy env-apis))))
    (mapcar (lambda (api-name)
              (list
               api-name ;; unique-id
               (vector
                environment ;; Environment
                api-name ;; API name
                )))
            api-names)))


(defun apigee-management ()
  "Entrypoint apigee management."
  (interactive)
  (let* ((environments (apigee-admin-api--get-environment-names))
         (apis (mapcar
                'apigee-admin-api--get-apis-deployed-to-environment
                environments))
         (rows-list (mapcar
                     'apigee-management--api-to-table-row
                     apis))
         (rows (apply 'append rows-list))
         )
    (pop-to-buffer (format "*apigee management: %s*" apigee-admin-api-organization))
    (apigee-management-mode)
    (setq tabulated-list-entries rows)
    (tabulated-list-print t)))
