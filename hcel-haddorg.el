;;; hcel-haddorg.el --- jumping between hcel and org generated from haddorg. -*- lexical-binding: t; -*-
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

(require 'hcel-source)
(require 'hcel-client)
(require 'hcel-utils)
(require 'json)
(require 'org)

(defcustom hcel-haddorg-dir "~/Projects/sedoc/haddock/org-output"
  "Directory of haddorg org files.")

(defcustom hcel-haddorg-lax-version t
  "If non-nil, match highest version if no exact match found.

Say we have ghc-8.6.5.org and ghc-9.2.2.org.  If the definition
is in ghc-8.10.1, hcel will attempt to look up in ghc-9.2.2.org.")

(defun hcel-haddorg-to-hcel-definition ()
  (interactive)
  (save-excursion
    (org-back-to-heading t)
    (let* ((id (org-entry-get (point) "CUSTOM_ID"))
           (splitted (split-string id "/"))
           (module-name (car splitted))
           (entity (if (equal "v" (cadr splitted)) "Val" "Typ"))
           (name (caddr splitted))
           (package) (package-id))
      (goto-char (point-min))
      (setq package (org-entry-get (point) "ITEM"))
      (setq package-id
            (hcel-parse-package-id
             (progn
               (string-match "^\\(.*?\\)\\(:.*\\)?$" package)
               (match-string 1 package))
             "-"))
      (hcel-load-module-location-info
       (alist-get 'location
                  (hcel-api-definition-site
                   package-id "lib" module-name entity name))))))

(defun hcel-identifier-at-point-to-haddorg ()
  (interactive)
  (when-let* ((identifier (hcel-text-property-near-point 'identifier))
              (id (alist-get (intern identifier) hcel-identifiers))
              (exported (alist-get 'isExported id))
              (external-id (alist-get 'externalId id)))
    (if (and (eq exported json-false)
             ;; FIXME: Hacky.  ExactLocation implies identifier is declared in
             ;; the current module.
             (equal (alist-get 'tag (alist-get 'locationInfo id)) "ExactLocation"))
        (message "%s is not exported." (hcel-occ-symbol-at-point))
      (let* ((splitted (split-string external-id "|"))
             (package-id (car splitted))
             (module-name (cadr splitted))
             (entity (cond ((equal (caddr splitted) "Typ") "t")
                           ((equal (caddr splitted) "Val") "v")
                           (t nil)))
             (name (cadddr splitted))
             (file-name (hcel-haddorg-fuzzy-version-match package-id))
             (custom-id (concat module-name "/" entity "/" name)))
        (if file-name
            (org-link-open-from-string
             (format "[[file:%s::#%s]]" file-name custom-id))
          (message "Cannot find org file for %s" package-id))))))

(defun hcel-haddorg-fuzzy-version-match (package-id)
  (let ((exact-match
         (expand-file-name (format "%s/%s.org" hcel-haddorg-dir package-id))))
    (cond ((file-exists-p exact-match) exact-match)
          (hcel-haddorg-lax-version
           (when-let ((files
                       (sort (directory-files
                              hcel-haddorg-dir t
                              (format "^%s\\(-[0-9.]+\\)?\\.org$"
                                      (car (split-string package-id "-"))))
                             (lambda (x y)
                               (string> (file-name-base x)
                                        (file-name-base y))))))
             (message
              "Cannot find org file for %s, opening instead that of the highest available version %s."
              package-id (file-name-base (car files)))
             (car files)))
          (t nil))))

(provide 'hcel-haddorg)
;;; hcel-haddorg.el ends here.
