;;; apigee-xml.el --- syntax highlighting            -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Ben Strutt

;; Author: Ben Strutt <ben@benstrutt.net>
;; Keywords: 

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(require 'nxml-mode)

(define-minor-mode apigee-xml-mode
  "Edit Apigee XML files."
  :lighter "aXML"
  :init-value nil
  :global nil
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "\C-c '") 'apigee-xml-edit-json)
            map)
  :group 'apigee
  
  (when apigee-xml-mode
    (progn 
      (unless (advice-member-p 'apigee-xml--fontify-matcher-advice 'nxml-fontify-matcher)
        (advice-add 'nxml-fontify-matcher :around #'apigee-xml--fontify-matcher-advice))
      (font-lock-flush))))

(defun apigee-xml--fontify-matcher-advice (orig-fun &rest args)
  "Advice for nxml-fontify-matcher.

ORIG-FUN should be nxml-fontify-matcher.
ARGS are the for nxml-fontify-matcher."
  (let* ((font-lock-beg (point))
         (res (apply orig-fun args))
         (font-lock-end (point)))
    (when apigee-xml-mode
      (save-match-data
        (let ((just-nxml-fontified (buffer-substring-no-properties
                                    font-lock-beg font-lock-end)))
          (when (and (string-match
                      "<Payload .*?contentType=\"\\(.+?\\)\".*>\\(\\(.\\|\n\\)*\\)</Payload *>"
                      just-nxml-fontified)
                     (string-equal (match-string 1 just-nxml-fontified) "application/json"))
            (let* ((json-index 2)
                   (unfontified-string (match-string-no-properties json-index just-nxml-fontified))
                   (beg (+ font-lock-beg (nth (* 2 json-index) (match-data)))) ;; buffer position of beginning of json-string
                   (buf (get-buffer-create "*apigee-xml-json-fontification*"))
                   (fontified-string (with-current-buffer buf
                                       (unless (eq major-mode 'json-mode)
                                         (json-mode))
                                       (erase-buffer)
                                       (insert unfontified-string)
                                       (insert " ");; ensure property change at end.
                                       (with-no-warnings
                                         (font-lock-ensure (point-min) (point-max)))
                                       (buffer-string)))
                   (str-pos 0))
              (while str-pos
                (let* ((props (text-properties-at str-pos fontified-string))
                       (next-pos (next-property-change str-pos fontified-string))
                       (next-pos-safe (if next-pos next-pos (length fontified-string))))
                  (set-text-properties (+ beg str-pos)
                                       (+ beg next-pos-safe)
                                       (text-properties-at str-pos fontified-string))
                  (setq str-pos next-pos))))))))
    res))


(defun apigee-xml--payload-at-point ()
  "Return the payload string at point."
  (save-match-data
    (save-excursion
      (nxml-backward-up-element)
      (when (looking-at-p "<Payload")
        (let ((point-start (point)))
          (nxml-forward-balanced-item)
          (let* ((point-end (point))
                 (elements (xml-parse-region point-start point-end))
                 (element (car elements))
                 (name (car element))
                 (tags (nth 1 element))
                 (body (nth 2 element)))
            (list body (alist-get 'contentType tags) point-start point-end)))))))

(provide 'apigee-xml)
;;; apigee-xml.el ends here
