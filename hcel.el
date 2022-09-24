;;; hcel.el --- Haskell codebase explorer -*- lexical-binding: t; -*-

;; Author: Yuchen Pei <id@ypei.org>
;; Maintainer: Yuchen Pei <id@ypei.org>
;; Created: 2022
;; Version: 0
;; Keywords: haskell
;; Package-Requires: ((emacs "28"))
;; Package-Type: multi
;; Homepage: https://g.ypei.me/hc.el.git

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

(require 'hcel-minor)
(require 'hcel-outline)
(require 'hcel-results)
(require 'hcel-source)
(require 'hcel-utils)

(defun hcel-package (package-id)
  "Select a package, followed by selecting a module to display the source."
  (interactive
   (list
    (completing-read "Select package: "
                     (mapcar #'hcel-format-package-id
                             (hcel-api-packages)))))
  (call-interactively
   (hcel-module-selector (hcel-parse-package-id package-id))))

(defun hcel-module ()
  "Select a module to display source."
  (interactive)
  (call-interactively
   (hcel-module-selector hcel-package-id)))

(defun hcel-module-selector (package-id)
  (lambda (module-path)
    (interactive
     (list
      (completing-read "Select module: "
                       (hcel-list-modules package-id))))
    (switch-to-buffer
     (hcel-load-module-source package-id module-path))))

(provide 'hcel)
;;; hcel.el ends here.
