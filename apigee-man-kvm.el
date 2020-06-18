;;; -*- lexical-binding: t; -*-

;;; Code:
(require 'apigee-man-api)

(defvar-local apigee-man-kvm--name nil
  "Name of the KeyValueMap.")

(defvar-local apigee-man-kvm--scope-type nil
  "Should be :api, :environment or :organisation.")

(defvar-local apigee-man-kvm--scope nil
  "Name of containing scope e.g. env-name, api-name. Can be nil
  in the case of organisation, following the conventions of
  `apigee-man-api'. ")



(defun apigee-man-kvm--edit-key-value-pair-at-point ()
  "Edit the Key Value pair at point."
  (interactive)
  (let* ((row (tabulated-list-get-entry))
         (current-key (aref row 0))
         (current-val (aref row 1))
         (new-key (read-string "Key: " current-key 'minibuffer-history current-key))
         (new-val (read-string "Value: " current-val 'minibuffer-history current-val)))

    (apigee-man-api-update-kvm-entry
     apigee-man-kvm--name
     current-key
     (cons new-key new-val)
     apigee-man-kvm--scope-type ;; keyword -> :api, :environment, :organisation
     apigee-man-kvm--scope ;; keyword-arg
     )))


(defvar apigee-man-kvm-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [?e] 'apigee-man-kvm--edit-key-value-pair-at-point)
    map ))

(define-derived-mode
  apigee-man-kvm-mode
  tabulated-list-mode
  "apigee kvm"
  "Manage a single Apigee KVM from the one true editor."

  (setq tabulated-list-format [("Key" 30 t)
                               ("Value" 30 t)
                               ])
  (setq tabulated-list-padding 1)
  (setq tabulated-list-sort-key (cons "Key" nil))
  (tabulated-list-init-header))

(defun apigee-man-kvm (kvm-name entry scope-type scope)
  "Show entries in KeyValueMap KVM-NAME with entries given by ENTRY.

SCOPE-TYPE and SCOPE are forwarded to the appropriate
`apigee-man-api' functions."
  (pop-to-buffer (format "KeyValueMap: %s" kvm-name))

  (apigee-man-kvm-mode)

  (setq apigee-man-kvm--name kvm-name)
  (setq apigee-man-kvm--scope-type scope-type)
  (setq apigee-man-kvm--scope scope)

  (setq tabulated-list-entries
        (mapcar
         (lambda (kvp)
           (let ((key (alist-get 'name kvp)))
             (list key (vector
                        key
                        (alist-get 'value kvp)))))
         entry))
  (tabulated-list-print t))


(provide 'apigee-man-kvm)
;;; apigee-man-kvm.el ends here
