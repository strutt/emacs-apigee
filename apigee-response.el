;;; -*- lexical-binding: t; -*-

;;; Code:
(require 'json)

(defun apigee-response ()
  "Read the response data in the current buffer."
  (save-excursion
    (goto-char (point-min)) ;; unnecessary?
    (if (not (looking-at-p "HTTP/1.1 200 OK"))
        ;; TODO put http response code/desc into error
        (error "Invalid request in %s" (current-buffer))
      (search-forward "\n\n") ;; Find the blank line
      (json-read))))

(provide 'apigee-response)
;;; apigee-response.el ends here
