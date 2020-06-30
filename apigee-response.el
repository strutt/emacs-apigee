;;; -*- lexical-binding: t; -*-

;;; Code:
(require 'json)

(defun apigee-response ()
  "Read the response data in the current buffer."
  (save-excursion
    (goto-char (point-min)) ;; unnecessary?
    (if (not (looking-at-p "HTTP/1.1 20"))
        ;; 200-type responses are all good.
        (progn
          (pop-to-buffer (current-buffer))
          (error "Invalid request in %s" (current-buffer)))
      (let ((content-type nil))
        (save-excursion
          (save-match-data
            (re-search-forward "Content-Type: \\(.+\\)" nil t)
            (setq content-type (match-string-no-properties 1))))
        (search-forward "\n\n");; Find the blank line
        (cond ((string-equal content-type "application/json")
               (json-read))
              ((string-equal content-type "application/xml")
               (xml-parse-region (point) (point-max)))
              (t
               (buffer-substring-no-properties (point) (point-max))))))))

(provide 'apigee-response)
;;; apigee-response.el ends here
