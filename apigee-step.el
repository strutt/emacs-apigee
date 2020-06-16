;;; apigee-step.el --- A mode to navigate apigee projects  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Ben Strutt

;; Author: Ben Strutt <ben.strutt1@nhs.net>
;; Keywords: apigee, nxml

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

;; Put this in .dir-locals at the root of your project and you're good to go.
;;     ((nxml-mode . ((eval . (add-to-list 'xref-backend-functions 'apigee-step-xref-backend)))))



;;; Code:

(require 'project)
(require 'xref)
(require 'xmltok) ;; in nxml-mode


(defun apigee-step-xref--named-element-at-point ()
  "Go up the element tree until we find something with a name attribute, returning that name."
  (save-excursion
    (save-match-data
      (let ((continue t)
            (last-point nil))
        (while (eq continue t)
          (nxml-backward-up-element)
          
          (save-excursion
            (xmltok-forward)
            (dolist (attr xmltok-attributes nil)
              (when (string-equal "name" (xmltok-attribute-local-name attr))
                (setq continue (xmltok-attribute-value attr)))
              ))

          ;; This is a bit hacky but does the job of preventing an
          ;; infinite loop.
          (when (and (eq continue t)
                     (equal (point) last-point))
            (setq continue nil))
          (setq last-point (point)))
        
        continue))))


(defun apigee-step-xref--step-at-point ()
  "File the file associated with <Step> at point."
  (interactive)
  (save-excursion
    (save-match-data
      (condition-case nil
          (let ((last-point nil))
            (while (not (looking-at-p "\n?[ \t]*<Step>"))
              (nxml-backward-up-element)
              
              ;; This is a bit hacky but does the job of preventing an
              ;; infinite loop.
              (if (equal (point) last-point)
                  (error "Not inside Step element")
                (setq last-point (point))))
            
            (nxml-down-element)

            ;; TODO newlines?
            (while (not (looking-at "\n?[ \t]*<Name>\\(.+?\\)</Name>"))
              (nxml-forward-element))

            (match-string-no-properties 1))
        (error nil)))))


(defun apigee-step-xref--find-regexp (regexp)
  "Find REGEXP in all XML files in project."
  (let ((xrefs (xref-matches-in-files
                regexp
                (seq-filter
                 (lambda (filename)
                   (message (file-name-extension filename))
                   (string-equal (file-name-extension filename) "xml"))
                 (project-files (project-current)))
                )))
    (unless xrefs
      (user-error "No matches for: %s" regexp))
    xrefs)
  )



;;;###autoload
(defun apigee-step-xref-backend ()
  "Apigee-Step-Ref backend for Xref."
  (when (eq major-mode 'nxml-mode)
    'apigee-step-ref)
  )

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql apigee-step-ref)))
  (or (apigee-step-xref--step-at-point)
      (apigee-step-xref--named-element-at-point)))

(cl-defmethod xref-backend-definitions ((_backend (eql apigee-step-ref)) symbol)
  (apigee-step-xref--find-regexp (regexp-quote (concat "name=\"" symbol "\""))))

(cl-defmethod xref-backend-references ((_backend (eql apigee-step-ref)) symbol)
  (apigee-step-xref--find-regexp (regexp-quote (concat "<Name>" symbol "</Name>"))))

(cl-defmethod xref-backend-apropos ((_backend (eql apigee-step-ref)) symbol)
  (apigee-step-xref--find-regexp (regexp-quote symbol)))

(cl-defmethod xref-backend-identifier-completion-table ((_backend (eql apigee-step-ref)))
  "Return a list of terms for completions taken from the symbols in the current buffer.
Selects all name=\"something\" attribute tags and
<Name>something<Name> inside a <Step> element.
"
  (let (words)
    (save-excursion
      (save-restriction
        (widen)

        (goto-char (point-min))
        (while (re-search-forward "<Name>\\(.+?\\)</Name>" nil t)
          (save-excursion
            (nxml-backward-up-element)
            (when (looking-at-p "<Step>")
              (add-to-list 'words (match-string-no-properties 1)))))
        
        (goto-char (point-min))
        (while (re-search-forward "name=\"\\(.+?\\)\"" nil t)
          (add-to-list 'words (match-string-no-properties 1)))
        (seq-uniq words)))))


(provide 'apigee-step)
;;; apigee-step.el ends here
