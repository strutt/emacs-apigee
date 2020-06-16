;;; apigee-admin.el --- Use the apigee admin API     -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Ben Strutt

;; Author: Ben Strutt <ben@benstrutt.net>
;; Keywords: apigee

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

(require 'request)

(defvar apigee-admin--access-token nil "Current access token.")
(defvar apigee-admin--refresh-token nil "Current refresh token.")

(defun apigee-admin--get-token ()
  "Get `apigee-admin--access-token' and `apigee-amin--refresh-token'.

Searches your auth-sources for username/password for
login.apigee.com, prompts for them if that search fails.  Prompts
for mfa code.  See
https://docs.apigee.com/api-platform/system-administration/management-api-tokens"
  (let* ((auth-info (car (auth-source-search :max 1
                                             :host "login.apigee.com")))
         (user (plist-get auth-info :user))
         (secret (plist-get auth-info :secret))
         (mfa-code nil))
    (unless user (setq user (read-string "Enter username: ")))
    (if secret
        (setq secret (funcall secret))
      (setq user (read-string "Enter password: ")))
    (setq mfa-code (read-string (format "Enter MFA code for %s: " user)))
    
    (let ((url (format "https://login.apigee.com/oauth/token?mfa_token=%s" mfa-code))
          (url-request-data (format "username=%s&password=%s&grant_type=password"
                                    user
                                    secret))
          (url-request-method "POST")
          (url-request-extra-headers '(("content-type" . "application/x-www-form-urlencoded;charset=utf-8")
                                       ("accept" . "application/json;charset=utf-8")
                                       ("authorization" . "Basic ZWRnZWNsaTplZGdlY2xpc2VjcmV0"))))
      (with-current-buffer
          (url-retrieve-synchronously url)
        (goto-char (point-min)) ;; unnecessary?
        (if (not (looking-at-p "HTTP/1.1 200 OK"))
            (error "Invalid request")
          (while (not (eq (char-after) ?\{ ))
            (forward-char))
          (let ((data (json-read)))
            (setq apigee-admin--access-token (alist-get 'access_token data))
            (setq apigee-admin--refresh-token (alist-get 'refresh_token data))))))))


(defun apigee-admin--get-utils ()
  "Download the apigee utils"
  (shell-command "curl https://login.apigee.com/resources/scripts/sso-cli/ssocli-bundle.zip -o \"ssocli-bundle.zip\"")
  (shell-command "unzip ssocli-bundle.zip")
  )



(provide 'apigee-admin)
;;; apigee-admin.el ends here


