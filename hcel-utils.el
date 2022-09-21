;;; hcel-utils.el --- Commonly used utilities -*- lexical-binding: t; -*-

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

;; data conversions
(defun hcel-parse-package-id (package-id &optional divider)
  (unless (stringp divider) (setq divider " "))
  (string-match (format "^\\(.*\\)%s\\(.*\\)$" divider) package-id)
  (list (cons 'name (match-string 1 package-id))
        (cons 'version (match-string 2 package-id))))

(defun hcel-location-tag (location-info)
  "Gets the tag of LOCATION-INFO."
  (alist-get 'tag location-info))

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

(defun hcel-id-src-span-to-location-info (package-id module-path id-src-span)
  "Converts an idSrcSpan to an exact location.

Example of an idSrcSpan:

        \"idSrcSpan\": {
          \"endColumn\": 43,
          \"line\": 228,
          \"modulePath\": \"src/HaskellCodeExplorer/Types.hs\",
          \"startColumn\": 26
        }
"
  (list
   (cons 'packageId package-id)
   (cons 'modulePath module-path)
   (cons 'startLine (alist-get 'line id-src-span))
   (cons 'startColumn (alist-get 'startColumn id-src-span))
   (cons 'endLine (alist-get 'line id-src-span))
   (cons 'endColumn (alist-get 'endColumn id-src-span))
   (cons 'tag "ExactLocation")))

(defun hcel-expression (expr)
  (when-let* ((span (alist-get 'srcSpan expr))
              (line-beg (alist-get 'line (alist-get 'start span)))
              (col-beg (alist-get 'column (alist-get 'start span)))
              (line-end (alist-get 'line (alist-get 'end span)))
              (col-end (alist-get 'column (alist-get 'end span))))
    (hcel-buffer-substring-line-column line-beg (1- col-beg) line-end (1- col-end))))

;; buffers and strings manipulation
(defun hcel-goto-line-column (line column)
  (goto-char (point-min))
  (forward-line (1- line))
  (move-to-column column))

(defun hcel-unquote-html (html)
  (replace-regexp-in-string
   "&amp;" "&"
   (replace-regexp-in-string
    "&#39;" "'"
    (replace-regexp-in-string
     "&quot;" "\""
     (replace-regexp-in-string
      "&gt;" ">"
      (replace-regexp-in-string
       "&lt;" "<" html))))))

(defun hcel-buffer-substring-line-column (line-beg col-beg line-end col-end)
  (save-excursion
    (buffer-substring
     (progn (hcel-goto-line-column line-beg col-beg) (point))
     (progn (hcel-goto-line-column line-end col-end) (point)))))

(defun hcel-fontify-with-haskell-mode ()
  "Fontify using haskell-mode"
  (require 'haskell)
  (let ((text (buffer-string)))
    (with-temp-buffer
      (haskell-mode)
      (insert text)
      (if (fboundp 'font-lock-ensure)
          (font-lock-ensure)
        (with-no-warnings (font-lock-fontify-buffer)))
      (setq text (buffer-substring (point-min) (point-max))))
    (erase-buffer)
    (insert text)))

(defun hcel-remove-html-markup (html)
  (replace-regexp-in-string
   "<.*?>" "" 
   (replace-regexp-in-string "</p>" "\n\n" html)))

(defun hcel-fill-string (text)
  (with-temp-buffer
    (insert text)
    (fill-region (point-min) (point-max))
    (buffer-substring (point-min) (point-max))))

;; string formatting
(defun hcel-format-package-id (package &optional divider)
  (unless (stringp divider) (setq divider " "))
  (concat (alist-get 'name package) divider (alist-get 'version package)))

(defun hcel-render-components (components &optional name)
  (when (or components name)
    (concat (when name (replace-regexp-in-string "\n" " " name))
            (when components
              (concat (when name " :: ")
                      (replace-regexp-in-string
                       "\n" " " (mapconcat
                                 (lambda (component)
                                   (propertize
                                    (or (alist-get 'name component)
                                        (alist-get 'contents component))
                                    'internal-id (alist-get 'internalId component)))
                                 components
                                 "")))))))

(defun hcel-render-id-type (id-type)
  (concat
   (hcel-render-components (alist-get 'components id-type))
   (when-let ((expanded (hcel-render-components
                         (alist-get 'componentsExpanded id-type))))
     (concat "\nExpands to: " expanded))))

(defun hcel-expression-and-type (expr)
  "Returns a pair of expression text and its type text."
  (when-let ((expression (hcel-expression expr))
             (type (hcel-render-id-type
                    (alist-get 'exprType (alist-get 'info expr)))))
    (cons expression type)))

(defun hcel-render-html (html)
  (when html
    (with-temp-buffer
      (insert html)
      (shr-render-region (point-min) (point-max))
      (buffer-string))))

(defun hcel-text-property-near-point (prop)
  "Find property prop at point, or just before point."
  (or (get-text-property (point) prop)
      (get-text-property (max 1 (1- (point))) prop)))

(provide 'hcel-utils)
;;; hcel-utils.el ends here.
