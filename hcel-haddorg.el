;;; hcel-haddorg.el --- jumping between hcel and org generated from haddorg. -*- lexical-binding: t; -*-
;; Copyright (C) 2022 Yuchen Pei.
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
(require 'org)

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

(provide 'hcel-haddorg)
;;; hcel-haddorg.el ends here.
