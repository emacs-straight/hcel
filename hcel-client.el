;;; hcel-client.el --- talks to a haskell-code-server. -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Free Software Foundation, Inc.
;; 
;; This file is part of hcel.
;; 
;; hcel is free software: you can redistribute it and/or modify it under
;; the terms of the GNU Affero General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;; 
;; hcel is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Affero General
;; Public License for more details.
;; 
;; You should have received a copy of the GNU Affero General Public
;; License along with hcel.  If not, see <https://www.gnu.org/licenses/>.

(require 'hcel-utils)
(require 'json)
(defcustom hcel-host "localhost:8080"
  "hcel server host"
  :group 'hcel :type '(string))
(defcustom hcel-indexed-dir "/.haskell-code-explorer"
  "hcel indexed dir"
  :group 'hcel :type '(string))

(defvar hcel-client-buffer-name "*hcel-client*")

(defun hcel-api-packages ()
  (let ((packages
         (hcel-url-fetch-json (concat hcel-host "/api/packages"))))
    (mapcan
     (lambda (package)
       (mapcar
        (lambda (version) (list (cons 'name (alist-get 'name package))
                                (cons 'version version)))
        (alist-get 'versions package)))
     packages)))

(defun hcel-api-package-info (package-id)
  (hcel-url-fetch-json (concat
                        hcel-host "/files/" (hcel-format-package-id package-id "-")
                        hcel-indexed-dir "/packageInfo.json")))

(defun hcel-list-modules (package-id)
  (mapcar
   (lambda (tuple)
     (prin1-to-string (car tuple) t))
   (alist-get 'modules (hcel-api-package-info package-id))))

(defun hcel-api-definition-site
    (package-id component-id module-name entity name)
  (hcel-url-fetch-json
   (concat hcel-host "/api/definitionSite/"
           (hcel-format-package-id package-id "-")
           "/" component-id "/" module-name "/" entity "/" name)))

(defun hcel-definition-site-location-info (approx-location-info)
  "Call definitionSite with info from an approximate location."
  (when (string= (hcel-location-tag approx-location-info) "ExactLocation")
    (error "An ExactLocation supplied."))
  (when-let* ((package-id (alist-get 'packageId approx-location-info))
              (component-id (alist-get 'componentId approx-location-info))
              (module-name (alist-get 'moduleName approx-location-info))
              (entity (alist-get 'entity approx-location-info))
              (name (alist-get 'name approx-location-info)))
    (hcel-api-definition-site package-id component-id module-name entity name)))

(defun hcel-approx-to-exact-location (approx-location-info)
  "Fetch exact location given approximate location.

Example of approximate location:

      \"locationInfo\": {
        \"componentId\": \"exe-haskell-code-server\",
        \"entity\": \"Typ\",
        \"haddockAnchorId\": \"PackageInfo\",
        \"moduleName\": \"HaskellCodeExplorer.Types\",
        \"name\": \"PackageInfo\",
        \"packageId\": {
          \"name\": \"haskell-code-explorer\",
          \"version\": \"0.1.0.0\"
        },
        \"tag\": \"ApproximateLocation\"
      }"
  (alist-get 'location
             (hcel-definition-site-location-info approx-location-info)))

(defun hcel-api-module-info (package-id module-path)
  (hcel-url-fetch-json
   (concat
    hcel-host "/files/" (hcel-format-package-id package-id "-")
    hcel-indexed-dir
    "/" (replace-regexp-in-string "/" "%252F" module-path) ".json.gz")
   t))

(defun hcel-api-expressions
    (package-id module-path line-beg col-beg line-end col-end)
  (hcel-url-fetch-json
   (concat
    hcel-host "/api/expressions/" (hcel-format-package-id package-id "-")
    "/" (replace-regexp-in-string "/" "%2F" module-path)
    "/" (number-to-string (1+ line-beg))
    "/" (number-to-string (1+ col-beg))
    "/" (number-to-string (1+ line-end))
    "/" (number-to-string (1+ col-end)))))

(defun hcel-api-hoogle-docs (package-id module-name entity name)
  (hcel-url-fetch-json
   (concat hcel-host "/api/hoogleDocs/"
           (hcel-format-package-id package-id "-") "/"
           module-name "/" entity "/" name)))

(defun hcel-format-pagination-query (page per-page)
  (when (or page per-page)
    (concat "?"
            (string-join 
             (list
              (when page (concat "page=" page))
              (when per-page (concat "per_page=" per-page)))
             (when (and page per-page) "&")))))

(defun hcel-api-references (package-id name &optional page per-page)
  (hcel-url-fetch-json
   (concat hcel-host "/api/references/"
           (hcel-format-package-id package-id "-") "/"
           name
           (hcel-format-pagination-query page per-page))))

(defun hcel-api-identifiers (scope query package-id &optional page per-page
                                   with-header)
  (hcel-url-fetch-json
   (concat hcel-host
           (if (eq scope 'global)
               "/api/globalIdentifiers/"
             (concat "/api/identifiers/"
                     (hcel-format-package-id package-id "-")
                     "/"))
           query
           (hcel-format-pagination-query page per-page))
   nil with-header))

(defun hcel-api-global-references (name)
  (hcel-url-fetch-json (concat hcel-host "/api/globalReferences/" name)))

(defun hcel-parse-http-header (text)
  (let ((status) (fields))
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      (re-search-forward "^HTTP.*\\([0-9]\\{3\\}\\).*$")
      (setq status (match-string 1))
      (while (re-search-forward "^\\(.*?\\): \\(.*\\)$" nil t)
        (push (cons (intern (match-string 1)) (match-string 2)) fields)))
    (list (cons 'status status) (cons 'fields fields))))

(defun hcel-url-fetch-json (url &optional decompression with-header)
  (with-current-buffer (get-buffer-create hcel-client-buffer-name)
    (goto-char (point-max))
    (insert "[" (current-time-string) "] Request: " url "\n"))
  (with-current-buffer (url-retrieve-synchronously url t)
    (let ((header) (status) (fields))
      (hcel-delete-http-header)
      (goto-char (point-min))
      (setq header (hcel-parse-http-header (car kill-ring))
            status (alist-get 'status header)
            fields (alist-get 'fields header))
      (with-current-buffer hcel-client-buffer-name
        (insert "[" (current-time-string) "] Response: " status "\n"))
      (when decompression
        (call-process-region (point) (point-max) "gunzip" t t t)
        (goto-char (point-min)))
      (call-interactively 'delete-trailing-whitespace)
      (if (string= status "200")
          (unless (= (point) (point-max))
            (if with-header
                (list
                 (cons 'header fields)
                 (cons 'json (json-read)))
              (json-read)))
        (error "HTTP error: %s" (buffer-substring (point) (point-max)))))))

(defun hcel-delete-http-header ()
  (save-excursion
    (goto-char (point-min))
    (kill-region (point) (progn (re-search-forward "\r?\n\r?\n")
                                (point)))))

(provide 'hcel-client)
