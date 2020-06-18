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
(require 'cl-lib)
(require 'auth-source)
(require 'json)

(require 'apigee-response)
(require 'apigee-auth)

(defcustom apigee-management-api-organization nil "Your organization, used in API calls.")
(defcustom apigee-management-api-send-data-confirm t
  "Confirm before sending data.  Set to nil to disable.")


(cl-defun apigee-management-api--request (endpoint
                                          &key
                                          callback
                                          data
                                          (method "GET"))
  "Make a request to ENDPOINT.

The backbone of `apigee-management-api'.

METHOD should be a string representing an HTTP verb,
e.g. \"GET\", \"POST\", \"PUT\".  Default is \"GET\".

DATA will be `json-encode'd and put in the request body for
\"POST\" and \"PUT\" requests.

If CALLBACK is provided then endpoint is retrieved asynchronously
calling CALLBACK with the emacs-lispified JSON data returned."
  (unless apigee-management-api-organization
    (user-error "Please set apigee-management-api-organization"))

  (let* ((url-request-method method)
         (url-request-data (unless (string-equal method "GET")
                             (json-encode data)))
         (url-request-extra-headers `(,(apigee-auth-header)
                                      ("content-type" . "application/json")))
         (url (format "https://api.enterprise.apigee.com/v1/organizations/%s/%s"
                      apigee-management-api-organization
                      endpoint))
         (url-func (if callback 'url-retrieve 'url-retrieve-synchronously))
         (url-callback (lambda (status &rest cbargs)
                         (let ((response-data (apigee-response)))
                           (when (string-match-p "*http" (buffer-name))
                             (kill-buffer (current-buffer)))
                           (when callback
                             (apply callback (list response-data)))
                           response-data)))
         (url-func-args (if callback `(,url ,url-callback) `(,url))))

    (when (or (not url-request-data) ;; No need to confirm if there's no body
              ;; only ask y-or-n-p if apigee-management-api-send-data-confirm is non-nil.
              (not apigee-management-api-send-data-confirm)
              (y-or-n-p (format "%s %s to %s? " method url-request-data url)))
      (with-current-buffer
          ;; Try to refresh token and try again if we have an error
          ;; thrown, this could be improved.
          ;; TODO ensure this will work in async cases!
          (condition-case nil
              (apply url-func url-func-args)
            (error nil
                   (apigee-auth-refresh-token)
                   (setq url-request-extra-headers `(,(apigee-auth-header)
                                                     ("content-type" . "application/json")))
                   (apply url-func url-func-args)
                   (url-retrieve-synchronously url)))
        (when (eq url-func 'url-retrieve-synchronously)
          (apply url-callback nil nil))))))



;; Environment API calls
;; https://apidocs.apigee.com/api/environments

(defun apigee-management-api-get-environment-names ()
  "Get environment names for `apigee-management-api-organisation'."
  (apigee-management-api--request "environments"))

(defun apigee-management-api-get-environment-details (environment)
  "Get ENVIRONMENT details for `apigee-management-api-organisation'."
  (apigee-management-api--request (format "environments/%s" environment)))

(defun apigee-management-api-get-apis-deployed-to-environment (environment)
  "Get APIs deployed to ENVIRONMENT for `apigee-management-api-organisation'."
  (apigee-management-api--request (format "environments/%s/deployments" environment)))


;; APIs
(cl-defun apigee-management-api-get-api (api)
  "Get API details for `apigee-management-api-organisation'."
  (apigee-management-api--request (format "apis/%s" api)))



;; KVMs
(cl-defun apigee-management-api-list-kvms (&key
                                           api
                                           environment
                                           organization
                                           &allow-other-keys)
  "Get all KeyValueMaps in specified scope.

The scope is indicated by choice of key: API, ENVIRONMENT,
ORGANIZATION.  If passed multiple keys we prefer the lowest
scope: API, then ENVIRONMENT, then ORGANIZATION.  The
organization value is ignored as `apigee-management-api-organization'
is used."
  (unless (or api environment organization)
    (user-error "Must specify one of API, ENVIRONMENT, ORGANIZATION"))
  (let ((endpoint (cond (api
                         (format "apis/%s/keyvaluemaps" api))
                        (environment
                         (format "environments/%s/keyvaluemaps" environment))
                        (organization "keyvaluemaps"))))
    (apigee-management-api--request endpoint)))


(cl-defun apigee-management-api-get-kvm (kvm
                                         &key
                                         api
                                         environment
                                         organization
                                         callback
                                         &allow-other-keys)
  "Get KeyValueMap KVM in specified scope.

The scope is indicated by choice of key: API, ENVIRONMENT,
ORGANIZATION.  If passed multiple keys we prefer the lowest
scope: API, then ENVIRONMENT, then ORGANIZATION. organization value is ignored as `apigee-management-api-organization'
is used.
CALLBACK is passed to `apigee-management-api--request'."
  (unless (or api environment organization)
    (user-error "Must specify one of API, ENVIRONMENT, ORGANIZATION"))

  (let ((endpoint (cond (api (format "apis/%s/keyvaluemaps/%s" api kvm))
                        (environment (format "environments/%s/keyvaluemaps/%s" environment kvm))
                        (organization (format "keyvaluemaps/%s" kvm)))))
    (apigee-management-api--request endpoint :callback callback)))



(cl-defun apigee-management-api-update-kvm-entry (kvm
                                                  entry
                                                  kvp
                                                  &key
                                                  api
                                                  environment
                                                  organization
                                                  callback
                                                  &allow-other-keys)
  "Update KeyValueMap (KVM) entry (ENTRY) to a new key value pair (KVP).

KVP should be a cons cell of the form (KEY . VALUE).  The scope
is indicated by choice of key: API, ENVIRONMENT, ORGANIZATION.
If passed multiple keys we prefer the lowest scope: API, then
ENVIRONMENT, then ORGANIZATION.  Note: the ORGANISATION value is
ignored as `apigee-management-api-organization' is used.

CALLBACK is passed to `apigee-management-api--request'."

  (unless (or api environment organization)
    (user-error "Must specify one of API, ENVIRONMENT, ORGANIZATION"))

  (unless (and (stringp (car kvp))
               (stringp (cdr kvp)))
    (user-error "Both (KEY . VALUE) in KVP must be strings"))

  (let ((formatted-kvp `((name . ,(car kvp))
                         (value . ,(cdr kvp))))
        (endpoint (cond (api (format "apis/%s/keyvaluemaps/%s/entries/%s" api kvm entry))
                        (environment (format "environments/%s/keyvaluemaps/%s/entries/%s" environment kvm entry))
                        (organization (format "keyvaluemaps/%s/entries/%s" kvm entry)))))
    (apigee-management-api--request endpoint
                                    :method "POST"
                                    :data formatted-kvp
                                    :callback callback)))

(provide 'apigee-management-api)
;;; apigee-management-api.el ends here
