;;; -*- lexical-binding: t; -*-

(require 'apigee-man-api)
(require 'apigee-man-kvm)

(defvar-local apigee-man-kvms--api nil)
(defvar-local apigee-man-kvms--environment nil)

(defvar apigee-man-kvms-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [?m] 'apigee-man-kvms--map-at-point)
    map))

(defun apigee-man-kvms--map-at-point ()
  "Show the KVM at point."
  (interactive)
  ;; aref indices must match `tabulated-list-format'
  (let* ((row (tabulated-list-get-entry))
         (entry (aref row 4))
         (kvm-name (aref row 3))
         (scope-type
          (apigee-man-kvms--scope-type-header-to-keyword-symbol
           (aref row 0)))
         (scope (aref row 1)))
    
    (apigee-man-kvm kvm-name
                           (json-read-from-string entry)
                           scope-type scope)))


(defun apigee-man-kvms--scope-type-header-to-keyword-symbol (scope-type)
  "Convert the header string SCOPE-TYPE from the to the corresponding keyword-symbol."
  (cond ((string-equal scope-type "API") :api)
        ((string-equal scope-type "Environment") :environment)
        ((string-equal scope-type "Organization") :organization)
        (t (error (format "Unknown scope-type: %s" scope-type)))))



(define-derived-mode
  apigee-man-kvms-mode
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

(defun apigee-man-kvms (api-kvms env-kvms org-kvms api-name env-name)
  "Show the API-KVMS, ENV-KVMS, and ORG-KVMS for API-NAME/ENV-NAME."
  (pop-to-buffer (format "KeyValueMaps: %s %s %s" api-name env-name apigee-man-api-organization))
  (apigee-man-kvms-mode)
  (setq apigee-man-kvms--api api-name)
  (setq apigee-man-kvms--environment env-name)
  (setq tabulated-list-entries
        (append
         (mapcar (lambda (kvm-name)
                   (list api-name (vector "API" api-name "" kvm-name "")))
                 api-kvms)
         (mapcar (lambda (kvm-name)
                   (list api-name (vector "Environment" env-name "" kvm-name "")))
                 env-kvms)
         (mapcar (lambda (kvm-name)
                   (list api-name (vector "Organization" apigee-man-api-organization "" kvm-name "")))
                 org-kvms)))
  (tabulated-list-print t)

  ;; Dispatch async requests to get KVM details.
  (mapc
   (lambda (tabulated-list-entry)
     (let* ((v (nth 1 tabulated-list-entry))
            (scope-type (aref v 0))
            (scope-name (aref v 1))
            (kvm-name (aref v 3))
            (key (apigee-man-kvms--scope-type-header-to-keyword-symbol scope-type))
            (buf-name (buffer-name)))
       (apigee-man-api-get-kvm
        kvm-name
        :callback (lambda (kvm)
                    (with-current-buffer buf-name
                      (aset v 2 (json-encode (alist-get 'encrypted kvm)))
                      (aset v 4 (json-encode (alist-get 'entry kvm)))
                      (tabulated-list-print t)))
        key scope-name)))
   tabulated-list-entries)



  )

(provide 'apigee-man-kvms)
