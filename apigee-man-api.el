;;; apigee-man-api.el --- Use the apigee admin API     -*- lexical-binding: t; -*-

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

(defconst apigee-man-api-root-url "https://api.enterprise.apigee.com/v1"
  "The Apigee management API URL.")

(defcustom apigee-man-api-send-data-confirm t
  "Confirm before sending data.  Set to nil to disable.")

(cl-defun apigee-man-api--request (endpoint
                                   &key
                                   callback
                                   data
                                   (content-type "application/json")
                                   (method "GET")
                                   (confirm nil))
  "Make a request to `apigee-man-api-root-url' ENDPOINT.

The backbone of `apigee-man-api'.

METHOD should be a string representing an HTTP verb,
e.g. \"GET\", \"POST\", \"PUT\".  Default is \"GET\".

DATA will be `json-encode'd and put in the request body for
\"POST\" and \"PUT\" requests.

If CALLBACK is provided then endpoint is retrieved asynchronously
calling CALLBACK with the `json-read' data returned."
  (let* ((url-request-method method)
         (url-request-data data)
         (url-request-extra-headers `(,(apigee-auth-header)
                                      ("content-type" . ,content-type)))
         (url (format "%s/%s" apigee-man-api-root-url endpoint))
         (url-func (if callback 'url-retrieve 'url-retrieve-synchronously))
         (url-callback (lambda (status &rest cbargs)
                         (let ((response-data (apigee-response)))
                           (when (string-match-p "*http" (buffer-name))
                             (kill-buffer (current-buffer)))
                           (when callback
                             (apply callback (list response-data)))
                           response-data)))
         (url-func-args (if callback `(,url ,url-callback) `(,url))))

    (when (or (not confirm)
              ;; only ask y-or-n-p if apigee-man-api-send-data-confirm is non-nil.
              (not apigee-man-api-send-data-confirm)
              (y-or-n-p (if (string-p confirm) confirm
                          (format "%s %s to %s? " method url-request-data url))))
      (with-current-buffer
          ;; Try to refresh token and try again if we have an error
          ;; thrown, this could be improved.
          ;; TODO ensure this will work in async cases!
          (condition-case nil
              (apply url-func url-func-args)
            (error nil
                   (apigee-auth-refresh-token)
                   (setq url-request-extra-headers `(,(apigee-auth-header)
                                                     ("content-type" . ,content-type)))
                   (apply url-func url-func-args)
                   (url-retrieve-synchronously url)))
        (when (eq url-func 'url-retrieve-synchronously)
          (apply url-callback nil nil))))))



;; Environment API calls
;; https://apidocs.apigee.com/api/environments

(defun apigee-man-api-list-environment-names (organization)
  "Get environment names in ORGANIZATION."
  (apigee-man-api--request (format "organizations/%s/environments" organization)))

(defun apigee-man-api-get-environment-details (organization environment)
  "Get ENVIRONMENT details for ORGANIZATION."
  (apigee-man-api--request (format "organizations/%s/environments/%s" environment)))

(defun apigee-man-api-get-apis-deployed-to-environment (organization environment)
  "Get APIs deployed to ENVIRONMENT for ORGANIZATION."
  (apigee-man-api--request (format "organizations/%s/environments/%s/deployments" organization environment)))

;; APIs
(cl-defun apigee-man-api-get-api (organization api)
  "Get details of API from ORGANIZATION."
  (apigee-man-api--request (format "organizations/%s/apis/%s" organization api)))

(cl-defun apigee-man-api-get-api-revisions (organization api)
  "Get details of API REVSION from ORGANIZATION."
  (apigee-man-api--request (format "organizations/%s/apis/%s/revisions"
                                   organization api)))

(cl-defun apigee-man-api-get-api-revision (organization api revision)
  "Get details of API REVSION from ORGANIZATION."
  (apigee-man-api--request (format "organizations/%s/apis/%s/revisions/%d"
                                   organization api revision)))



(defun apigee-man-api-deploy-api-revision (organization
                                           environment
                                           api
                                           revision)
  "Deploy API at REVISION to ORGANIZATION in ENVIRONMENT."
  (apigee-man-api--request (format "organizations/%s/environments/%s/apis/%s/revision/%d"
                                   organization
                                   environment
                                   api
                                   revision)
                           :method "POST"
                           :confirm (format "Deploy %s rev %d to %s?" api revision environment)
                           )
  )



(cl-defun apigee-man-api-import-api (organization api zip-file-name
                                                  &key (action "validate"))
  "Import the api proxy in ZIP-FILE-NAME, called API to ORGANIZATION.

ACTION should be either \"import\" or \"validate\", defaults to \"validate\"."
  (unless (member action '("validate" "import"))
    (user-error (format "Unknown action %s" action)))

  (unless (file-exists-p zip-file-name)
    (user-error (format "No such file %s" zip-file-name)))

  (with-temp-buffer
    (insert-file-contents-literally zip-file-name)
    
    (let* ((multi-part-boundary ;; Random string
            (let ((s "abcdefghijkl"))
              (dotimes (i (length s))
                (aset s i (aref "abcdefghijklmnopqrstuvwxyz012345789" (random 36))))
              s))
           (data (buffer-substring-no-properties (point-min) (point-max)))
           (data (concat
                  "--" multi-part-boundary "\r\n"
                  "Content-Disposition: form-data; name=file; filename=apiproxy.zip\r\n"
                  "\r\n"
                  data "\r\n"
                  "\r\n"
                  "--" multi-part-boundary "\r\n" ))
           (data (encode-coding-string data 'binary)))
      (apigee-man-api--request
       (format "organizations/%s/apis?name=%s&action=%s" organization api action)
       :method "POST"
       :content-type (format "multipart/form-data; boundary=%s" multi-part-boundary)
       :confirm (string-equal action "import")
       :data data))))

;; KVMs
(cl-defun apigee-man-api-list-kvms (organization
                                    &key
                                    api
                                    environment
                                    &allow-other-keys)
  "Get all KeyValueMaps in specified scope.

The scope is indicated by combinations API, ENVIRONMENT and
ORGANIZATION.  If passed keys we prefer the lowest scope: API,
then ENVIRONMENT, then ORGANIZATION if nothing else is provided."
  (unless (or api environment organization)
    (user-error "Must specify one of API, ENVIRONMENT, ORGANIZATION"))
  (let ((endpoint (cond (api
                         (format "organizations/%s/apis/%s/keyvaluemaps" api))
                        (environment
                         (format "organizations/%s/environments/%s/keyvaluemaps" environment))
                        (organization "organizations/%s/keyvaluemaps"))))
    (apigee-man-api--request endpoint)))


(cl-defun apigee-man-api-get-kvm (organization
                                  kvm
                                  &key
                                  api
                                  environment
                                  callback
                                  &allow-other-keys)
  "Get KeyValueMap KVM in specified scope."
  (unless (or api environment organization)
    (user-error "Must specify one of API, ENVIRONMENT, ORGANIZATION"))

  (let ((endpoint (cond (api (format "organizations/%s/apis/%s/keyvaluemaps/%s" api kvm))
                        (environment (format "organizations/%s/environments/%s/keyvaluemaps/%s" environment kvm))
                        (organization (format "organizations/%s/keyvaluemaps/%s" kvm)))))
    (apigee-man-api--request endpoint :callback callback)))



(cl-defun apigee-man-api-update-kvm-entry (organization
                                           kvm
                                           entry
                                           kvp
                                           &key
                                           api
                                           environment
                                           callback
                                           &allow-other-keys)
  "Update KeyValueMap (KVM) entry (ENTRY) to a new key value pair (KVP).

KVP should be a cons cell of the form (KEY . VALUE).

CALLBACK is passed to `apigee-man-api--request'."

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
    (apigee-man-api--request endpoint
                             :method "POST"
                             :confirm t
                             :data (json-encode formatted-kvp)
                             :callback callback)))

(provide 'apigee-man-api)
;;; apigee-man-api.el ends here
