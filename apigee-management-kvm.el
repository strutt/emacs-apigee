;;; -*- lexical-binding: t; -*-

;;; Code:
(require 'apigee-management-api)

(defvar-local apigee-management-kvm--name nil
  "Name of the KeyValueMap.")

(defvar-local apigee-management-kvm--scope-type nil
  "Should be :api, :environment or :organisation.")

(defvar-local apigee-management-kvm--scope nil
  "Name of containing scope e.g. env-name, api-name. Can be nil
  in the case of organisation, following the conventions of
  `apigee-management-api'. ")



(defun apigee-management-kvm--edit-key-value-pair-at-point ()
  "Edit the Key Value pair at point."
  (interactive)
  (let* ((row (tabulated-list-get-entry))
         (current-key (aref row 0))
         (current-val (aref row 1))
         (new-key (read-string "Key: " current-key 'minibuffer-history current-key))
         (new-val (read-string "Value: " current-val 'minibuffer-history current-val)))

    (apigee-management-api-update-kvm-entry
     apigee-management-kvm--name
     current-key
     (cons new-key new-val)
     apigee-management-kvm--scope-type ;; keyword -> :api, :environment, :organisation
     apigee-management-kvm--scope ;; keyword-arg
     )))


(defvar apigee-management-kvm-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [?e] 'apigee-management-kvm--edit-key-value-pair-at-point)
    map ))

(define-derived-mode
  apigee-management-kvm-mode
  tabulated-list-mode
  "apigee kvm"
  "Manage a single Apigee KVM from the one true editor."

  (setq tabulated-list-format [("Key" 30 t)
                               ("Value" 30 t)
                               ])
  (setq tabulated-list-padding 1)
  (setq tabulated-list-sort-key (cons "Key" nil))
  (tabulated-list-init-header))

(defun apigee-management-kvm (kvm-name entry scope-type scope)
  "Show entries in KeyValueMap KVM-NAME with entries given by ENTRY.

SCOPE-TYPE and SCOPE are forwarded to the appropriate
`apigee-management-api' functions."
  (pop-to-buffer (format "KeyValueMap: %s" kvm-name))

  (apigee-management-kvm-mode)

  (setq apigee-management-kvm--name kvm-name)
  (setq apigee-management-kvm--scope-type scope-type)
  (setq apigee-management-kvm--scope scope)

  (setq tabulated-list-entries
        (mapcar
         (lambda (kvp)
           (let ((key (alist-get 'name kvp)))
             (list key (vector
                        key
                        (alist-get 'value kvp)))))
         entry))
  (tabulated-list-print t))


(provide 'apigee-management-kvm)
;;; apigee-management-kvm.el ends here
