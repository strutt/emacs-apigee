 ;;; -*- lexical-binding: t; -*-

;;; Code:
(require 'apigee-response)

(defvar apigee-auth--access-token nil "Current access token.")
(defvar apigee-auth--refresh-token nil "Current refresh token.")
(defvar apigee-auth-mfa-code-func nil
  "Function to call to get MFA code.

If nil then you will be prompted for your MFA code in the minibuffer.")

(defconst apigee-auth--token-extra-headers
  '(("content-type" . "application/x-www-form-urlencoded;charset=utf-8")
    ("accept" . "application/json;charset=utf-8")
    ("authorization" . "Basic ZWRnZWNsaTplZGdlY2xpc2VjcmV0"))
  "Extra headers for `url-retrieve'.")

(defun apigee-auth--set-tokens ()
  "Set `apigee-auth--access-token' and `apigee-amin--refresh-token'."
  (let ((data (apigee-response)))
    (setq apigee-auth--access-token (alist-get 'access_token data))
    (setq apigee-auth--refresh-token (alist-get 'refresh_token data))))

(defun apigee-auth--new-access-token ()
  "Send your username and password to get tokens.

This functions sets `apigee-auth--access-token' and
`apigee-amin--refresh-token'.

Searches your `auth-sources' for username/password for
login.apigee.com, prompts for them if that search fails.  Prompts for
mfa code.  See
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
    (if apigee-auth-mfa-code-func
        (setq mfa-code (funcall apigee-auth-mfa-code-func))
      (setq mfa-code (read-string (format "Enter MFA code for %s: " user))))

    (let ((url (format "https://login.apigee.com/oauth/token?mfa_token=%s" mfa-code))
          (url-request-data (format "username=%s&password=%s&grant_type=password"
                                    user
                                    secret))
          (url-request-method "POST")
          (url-request-extra-headers apigee-auth--token-extra-headers))
      (with-current-buffer
          (url-retrieve-synchronously url)
        (apigee-auth--set-tokens)))))

(defun apigee-auth-refresh-token ()
  "Refresh your access token."

  (unless apigee-auth--refresh-token
    (user-error "Cannot refresh access token without refresh token"))

  (let ((url "https://login.apigee.com/oauth/token")
        (url-request-data (format "grant_type=refresh_token&refresh_token=%s"
                                  apigee-auth--refresh-token))
        (url-request-method "POST")
        (url-request-extra-headers apigee-auth--token-extra-headers))
    (with-current-buffer
        (url-retrieve-synchronously url)
      (apigee-auth--set-tokens))))

(defun apigee-auth-header ()
  "Create Authorization header for `apigee-api' call."
  (unless apigee-auth--access-token
    (apigee-auth--new-access-token))
  (cons "authorization" (format "Bearer %s" apigee-auth--access-token)))

(provide 'apigee-auth)
;;; apigee-auth.el ends here
