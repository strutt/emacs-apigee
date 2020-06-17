(require 'apigee-admin-api)
(require 'apigee-admin-kvm)

(defvar-local apigee-management-kvms--api nil)
(defvar-local apigee-management-kvms--environment nil)

(defvar apigee-management-kvms-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [?k] 'apigee-management-kvms--list-keys)
    map
    )
  )

;; (defun apigee-management-kvms--list-keys ()
;;   "List the keys of KVM at point."
;;   (interactive)
;;   ;; aref indices must match `tabulated-list-format'
;;   (let* ((scope-name (aref (tabulated-list-get-entry) 0))
;;          (kvm-name (aref (tabulated-list-get-entry) 1))
;;          (scope (if (string-equal scope-name apigee-management-kvms--api)
;;                     :api
;;                   (if (string-equal scope-name apigee-management-kvms--environment)
;;                       :environment
;;                     :organization ))))
    
;;     (apigee-management-kvm kvm-name scope )))


(define-derived-mode
  apigee-management-kvms-mode
  tabulated-list-mode
  "apigee kvms"
  "Manage Apigee KVMs from the one true editor."

  (setq tabulated-list-format [("Scope" 20 t)
                               ("Encrypted" 5 t)
                               ("Name" 100 t)
                               ("Entries" 100 t)
                               ])
  (setq tabulated-list-padding 1)
  ;; (setq tabulated-list-sort-key (cons "Scope" t))
  (tabulated-list-init-header))

(defun apigee-management-kvms (api-kvms env-kvms org-kvms api-name env-name)
  "Show the API-KVMS, ENV-KVMS, and ORG-KVMS for API-NAME/ENV-NAME."
  (pp api-kvms)
  (pp env-kvms)
  (pp org-kvms)
  (pop-to-buffer (format "KeyValueMaps: %s %s %s" api-name env-name apigee-admin-api-organization))
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
                           (vector apigee-admin-api-organization
                                   (json-encode (alist-get 'encrypted kvm))
                                   kvm-name
                                   (json-encode (alist-get 'entry kvm))))))
                 org-kvms)
         
         ))
  (tabulated-list-print t))

(provide 'apigee-management-kvms)
