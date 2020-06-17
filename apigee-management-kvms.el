(require 'apigee-management-api)
(require 'apigee-management-kvm)

(defvar-local apigee-management-kvms--api nil)
(defvar-local apigee-management-kvms--environment nil)

(defvar apigee-management-kvms-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [?k] 'apigee-management-kvms--expand)
    map
    )
  )

(defun apigee-management-kvms--expand ()
  "Show the KVM at point."
  (interactive)
  ;; aref indices must match `tabulated-list-format'
  (let* ((entry (aref (tabulated-list-get-entry) 3))
         (kvm-name (aref (tabulated-list-get-entry) 2)))
    
    (apigee-management-kvm kvm-name (json-read-from-string entry))))


(define-derived-mode
  apigee-management-kvms-mode
  tabulated-list-mode
  "apigee kvms"
  "Manage Apigee KVMs from the one true editor."

  (setq tabulated-list-format [("Scope" 20 t)
                               ("Encrypted" 9 t)
                               ("Name" 30 t)
                               ("Entries" 100 t)
                               ])
  (setq tabulated-list-padding 1)
  ;; (setq tabulated-list-sort-key (cons "Scope" t))
  (tabulated-list-init-header))

(defun apigee-management-kvms (api-kvms env-kvms org-kvms api-name env-name)
  "Show the API-KVMS, ENV-KVMS, and ORG-KVMS for API-NAME/ENV-NAME."
  (pop-to-buffer (format "KeyValueMaps: %s %s %s" api-name env-name apigee-management-api-organization))
  (apigee-management-kvms-mode)
  (setq apigee-management-kvms--api api-name)
  (setq apigee-management-kvms--environment env-name)
  (setq tabulated-list-entries
        (append
         (mapcar (lambda (kvm)
                   (let ((kvm-name (alist-get 'name kvm)))
                     (list kvm-name
                           (vector api-name
                                   (json-encode (alist-get 'encrypted kvm))
                                   kvm-name
                                   (json-encode (alist-get 'entry kvm))))))
                 api-kvms)

         (mapcar (lambda (kvm)
                   (let ((kvm-name (alist-get 'name kvm)))
                     (list kvm-name
                           (vector env-name
                                   (json-encode (alist-get 'encrypted kvm))
                                   kvm-name
                                   (json-encode (alist-get 'entry kvm))))))
                 env-kvms)

         (mapcar (lambda (kvm)
                   (let ((kvm-name (alist-get 'name kvm)))
                     (list kvm-name
                           (vector apigee-management-api-organization
                                   (json-encode (alist-get 'encrypted kvm))
                                   kvm-name
                                   (json-encode (alist-get 'entry kvm))))))
                 org-kvms)
         
         ))
  (tabulated-list-print t))

(provide 'apigee-management-kvms)
