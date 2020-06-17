(require 'apigee-management-api)

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


(defun apigee-management-kvm (kvm-name entry)
  "Show entries in KeyValueMap KVM-NAME with entries given by ENTRIES-JSON."
  (pop-to-buffer (format "KeyValueMap: %s" kvm-name))
  (apigee-management-kvm-mode)
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
