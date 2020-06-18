;;; apigee-management-api.el --- Use the apigee admin API     -*- lexical-binding: t; -*-

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

(defvar apigee-management-api-organization nil "Your organization, used in API calls.")
(defvar apigee-management-api--access-token nil "Current access token.")
(defvar apigee-management-api--refresh-token nil "Current refresh token.")

(defconst apigee-management-api--request-extra-headers
  '(("content-type" . "application/x-www-form-urlencoded;charset=utf-8")
    ("accept" . "application/json;charset=utf-8")
    ("authorization" . "Basic ZWRnZWNsaTplZGdlY2xpc2VjcmV0"))
  "Extra headers for `url-retrieve'.")

(defun apigee-management-api--response-data ()
  "Read the response data in the current buffer."
  (save-excursion
    (goto-char (point-min)) ;; unnecessary?
    (if (not (looking-at-p "HTTP/1.1 200 OK"))
        (error "Invalid request in %s" (current-buffer))
      (search-forward "\n\n") ;; Find the blank line
      (json-read))))

(defun apigee-management-api--set-tokens ()
  "Set `apigee-management-api--access-token' and `apigee-amin--refresh-token'."
  (let ((data (apigee-management-api--response-data)))
    (setq apigee-management-api--access-token (alist-get 'access_token data))
    (setq apigee-management-api--refresh-token (alist-get 'refresh_token data))))

(defun apigee-management-api--refresh-access-token ()
  "Refresh your access token."

  (unless apigee-management-api--refresh-token
    (user-error "Cannot refresh access token without refresh token")
    )

  (let ((url "https://login.apigee.com/oauth/token")
        (url-request-data (format "grant_type=refresh_token&refresh_token=%s"
                                  apigee-management-api--refresh-token))
        (url-request-method "POST")
        (url-request-extra-headers apigee-management-api--request-extra-headers))
    (with-current-buffer
        (url-retrieve-synchronously url)
      (apigee-management-api--set-tokens))))

(defun apigee-management-api--get-new-access-token ()
  "Send your username and password to get tokens.

This functions sets `apigee-management-api--access-token' and
`apigee-amin--refresh-token'.

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
          (url-request-extra-headers apigee-management-api--request-extra-headers))
      (with-current-buffer
          (url-retrieve-synchronously url)
        (apigee-management-api--set-tokens)))))


(defun apigee-management-api--access-token ()
  "Call the OAuth2 API to get an access token if we don't have one."
  (unless apigee-management-api--access-token
    (apigee-management-api--get-new-access-token))
  apigee-management-api--access-token)

(defun apigee-management-api--get (endpoint &optional callback)
  "Read the data from ENDPOINT.

If CALLBACK is provided then endpoint is retrieved asynchronously
calling CALLBACK with the emacs-lispified JSON data returned."
  (unless apigee-management-api-organization
    (user-error "Please set apigee-management-api-organization"))

  (let* ((url-request-extra-headers `(("authorization" . ,(format "Bearer %s" (apigee-management-api--access-token)))))
         (url (format "https://api.enterprise.apigee.com/v1/organizations/%s/%s"
                      apigee-management-api-organization
                      endpoint))
         (url-func (if callback 'url-retrieve 'url-retrieve-synchronously))
         (url-callback (lambda (status &rest cbargs)
                         (let ((response-data (apigee-management-api--response-data)))
                           (when (string-match-p "*http" (buffer-name))
                             (kill-buffer (current-buffer)))
                           (when callback
                             (apply callback (list response-data)))
                           response-data)))
         (url-func-args (if callback `(,url ,url-callback) `(,url))))
    
    (with-current-buffer
        ;; Try to refresh token and try again if we have an error
        ;; thrown, this could be improved.
        ;; TODO make this work in both sync + async cases!
        (condition-case nil
            (apply url-func url-func-args)
          (error nil
                 (apigee-management-api--refresh-access-token)
                 (setq url-request-extra-headers `(("authorization" . ,(format "Bearer %s" (apigee-management-api--access-token)))))
                 (apply url-func url-func-args)
                 (url-retrieve-synchronously url)))
      (when (eq url-func 'url-retrieve-synchronously)
        (apply url-callback nil nil)))))

;; (defun apigee-management-api--apiproducts ()
;;   "List API products."
;;   (apigee-management-api--get "apiproducts"))




;; Environment API calls
;; https://apidocs.apigee.com/api/environments

(defun apigee-management-api--get-environment-names ()
  "Get environment names for `apigee-management-api-organisation'."
  (apigee-management-api--get "environments"))

(defun apigee-management-api--get-environment-details (environment)
  "Get ENVIRONMENT details for `apigee-management-api-organisation'."
  (apigee-management-api--get (format "environments/%s" )))

(defun apigee-management-api--get-apis-deployed-to-environment (environment)
  "Get APIs deployed to ENVIRONMENT for `apigee-management-api-organisation'."
  (apigee-management-api--get (format "environments/%s/deployments" environment)))


;; APIs
(cl-defun apigee-management-api--get-api (api)
  "Get API details for `apigee-management-api-organisation'."
  (apigee-management-api--get (format "apis/%s" api)))



;; KVMs
(cl-defun apigee-management-api--list-kvms (&optional
                                            &key
                                            api
                                            environment
                                            organization
                                            &allow_other_keys)
  "Get KeyValueMap for scope.

The scope is indicated by choice of key: API, ENVIRONMENT,
ORGANIZATION.  If passed multiple keys we prefer the lowest
scope: API, then ENVIRONMENT, then ORGANIZATION.  The
organization value is ignored as `apigee-management-api-organization'
is used."
  (cond (api
         (apigee-management-api--get (format "apis/%s/keyvaluemaps" api)))
        (environment
         (apigee-management-api--get (format "environments/%s/keyvaluemaps" environment)))
        (organization
         (apigee-management-api--get (format "keyvaluemaps" apigee-management-api-organization)))))

(cl-defun apigee-management-api--get-kvm (kvm
                                          &optional
                                          callback
                                          &key
                                          api
                                          environment
                                          organization
                                          &allow_other_keys)
  "Get KeyValueMap KVM.

The scope is indicated by choice of key: API, ENVIRONMENT,
ORGANIZATION.  If passed multiple keys we prefer the lowest
scope: API, then ENVIRONMENT, then ORGANIZATION. organization value is ignored as `apigee-management-api-organization'
is used."
  (cond (api
         (apigee-management-api--get (format "apis/%s/keyvaluemaps/%s" api kvm) callback))
        (environment
         (apigee-management-api--get (format "environments/%s/keyvaluemaps/%s" environment kvm) callback))
        (organization
         (apigee-management-api--get (format "keyvaluemaps/%s" kvm) callback))))


;; Keystores/truststores
(defun apigee-management-api--list-keystores (api)
  "Get KeyValueMap deployed for API `apigee-management-api-organisation'."
  (apigee-management-api--get (format "apis/%s/" api)))



(provide 'apigee-management-api)
;;; apigee-management-api.el ends here


