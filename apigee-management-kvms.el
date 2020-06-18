;;; -*- lexical-binding: t; -*-

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
  (let* ((entry (aref (tabulated-list-get-entry) 4))
         (kvm-name (aref (tabulated-list-get-entry) 3)))
    
    (apigee-management-kvm kvm-name (json-read-from-string entry))))


(define-derived-mode
  apigee-management-kvms-mode
  tabulated-list-mode
  "apigee kvms"
  "Manage Apigee KVMs from the one true editor."

  (setq tabulated-list-format [("Scope Type" 11 t)
                               ("Scope Name" 20 t)
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
         (mapcar (lambda (kvm-name)
                   (list api-name (vector "API" api-name "" kvm-name "")))
                 api-kvms)
         (mapcar (lambda (kvm-name)
                   (list api-name (vector "Environment" env-name "" kvm-name "")))
                 env-kvms)
         (mapcar (lambda (kvm-name)
                   (list api-name (vector "Organization" apigee-management-api "" kvm-name "")))
                 org-kvms)))
  (tabulated-list-print t)

  ;; Dispatch async requests to get KVM details.
  (mapc
   (lambda (tabulated-list-entry)
     (let* ((v (nth 1 tabulated-list-entry))
            (scope-type (aref v 0))
            (scope-name (aref v 1))
            (kvm-name (aref v 3))
            (key (cond ((string-equal scope-type "API") :api)
                       ((string-equal scope-type "Environment") :environment)
                       ((string-equal scope-type "Organization") :organization)))
            (buf-name (buffer-name)))
       (apigee-management-api--get-kvm kvm-name
                                       (lambda (kvm)
                                         (with-current-buffer buf-name
                                           (aset v 2 (json-encode (alist-get 'encrypted kvm)))
                                           (aset v 4 (json-encode (alist-get 'entry kvm)))
                                           (tabulated-list-print t)))
                                       key scope-name)))
   tabulated-list-entries)

  
  
  )

(provide 'apigee-management-kvms)
