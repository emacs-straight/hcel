;;; hcel-minor.el --- hcel-minor mode for definitions, references and eldoc. -*- lexical-binding: t; -*-

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
(require 'hcel-results)
(require 'hcel-outline)

(defvar hcel-minor-major-modes
  '(hcel-outline-mode hcel-ids-mode)
  "Major modes where hcel-minor mode can live in.")

(defvar hcel-minor-mode-map
  (let ((kmap (make-sparse-keymap)))
    (define-key kmap (kbd "M-?") #'hcel-minor-find-references-at-point)
    kmap))

(define-minor-mode hcel-minor-mode
  "A minor mode for exploring haskell codebases."
  :lighter " hcel-minor"
  (add-hook 'xref-backend-functions
            #'hcel-minor--xref-backend nil t)
  (cond
   ((null hcel-minor-mode)
    (remove-hook 'eldoc-documentation-functions #'hcel-minor-eldoc-id-type t)
    (remove-hook 'eldoc-documentation-functions #'hcel-minor-eldoc-docs t))
   ((not (or (apply 'derived-mode-p hcel-minor-major-modes)
             (eq (current-buffer) eldoc--doc-buffer)))
    (setq hcel-minor-mode nil)
    (error "Not in one of the supported modes (%s) or the eldoc buffer."
           (mapconcat #'prin1-to-string hcel-minor-major-modes
                      ", ")))
   (t
    (add-hook
     'eldoc-documentation-functions #'hcel-minor-eldoc-docs nil t)
    (add-hook
     'eldoc-documentation-functions #'hcel-minor-eldoc-id-type nil t)
    (setq-local eldoc-documentation-strategy 'eldoc-documentation-compose))))

(defun hcel-minor-find-references-at-point ()
  (interactive)
  (cond ((or (derived-mode-p 'hcel-outline-mode)
             (eq (current-buffer) eldoc--doc-buffer))
         (hcel-find-references-internal
          (hcel-text-property-near-point 'package-id)
          (hcel-text-property-near-point 'module-path)
          (hcel-text-property-near-point 'internal-id)))
        ((derived-mode-p 'hcel-ids-mode)
         (hcel-find-references-internal
          (alist-get 'packageId (hcel-text-property-near-point 'location-info))
          (alist-get 'modulePath (hcel-text-property-near-point 'location-info))
          (hcel-text-property-near-point 'internal-id)))
        (t (error "%S not supported and not in eldoc doc buffer." major-mode))))

(defun hcel-minor-eldoc-id-type (cb)
  (when-let* ((internal-id (hcel-text-property-near-point 'internal-id))
              (symbol (save-excursion
                        (buffer-substring-no-properties
                         (progn
                           (text-property-search-backward
                            'internal-id internal-id 'equal)
                           (point))
                         (progn
                           (text-property-search-forward
                            'internal-id internal-id 'equal)
                           (point)))))
              (docstring
               (cond ((derived-mode-p 'hcel-outline-mode)
                      (hcel-render-type-internal
                       (hcel-text-property-near-point 'package-id)
                       (hcel-text-property-near-point 'module-path)
                       internal-id))
                     ((derived-mode-p 'hcel-ids-mode)
                      (hcel-render-type-internal
                       (alist-get 'packageId (hcel-text-property-near-point 'location-info))
                       (alist-get 'modulePath (hcel-text-property-near-point 'location-info))
                       internal-id))
                     (t nil))))
    (funcall cb docstring
             :thing symbol
             :face 'font-lock-variable-name-face)
    (with-current-buffer eldoc--doc-buffer
      (hcel-minor-mode 1))))

(defun hcel-minor-eldoc-docs (cb)
  (when-let* ((docstring
               (cond ((derived-mode-p 'hcel-outline-mode)
                      (hcel-id-docs-internal
                       (hcel-text-property-near-point 'package-id)
                       (hcel-text-property-near-point 'module-path)
                       (hcel-text-property-near-point 'internal-id)))
                     ((derived-mode-p 'hcel-ids-mode)
                      (hcel-id-docs-internal
                       (alist-get 'packageId (hcel-text-property-near-point 'location-info))
                       (alist-get 'modulePath (hcel-text-property-near-point 'location-info))
                       (hcel-text-property-near-point 'internal-id)))
                     (t nil))))
    (setq this-command nil)
    (funcall cb docstring)
    (with-current-buffer eldoc--doc-buffer
      (hcel-minor-mode))))

(add-hook 'hcel-ids-mode-hook (lambda () (hcel-minor-mode 1)))
(add-hook 'hcel-outline-mode-hook (lambda () (hcel-minor-mode 1)))

(add-hook 'hcel-eldoc-hook (lambda ()
                             (with-current-buffer eldoc--doc-buffer
                               (hcel-minor-mode 1))))

(defun hcel-minor--xref-backend () 'hcel-minor-xref)
(cl-defmethod xref-backend-definitions ((_backend (eql hcel-minor-xref)) _identifiers)
  (hcel-minor-find-definition-at-point))
(defun hcel-minor-find-definition-at-point ()
  (interactive)
  (cond ((or (derived-mode-p 'hcel-outline-mode)
             (eq (current-buffer) eldoc--doc-buffer))
         (hcel-find-definition-internal
          (hcel-text-property-near-point 'package-id)
          (hcel-text-property-near-point 'module-path)
          (hcel-text-property-near-point 'internal-id)))
        ((derived-mode-p 'hcel-ids-mode)
         (hcel-find-definition-internal
          (alist-get 'packageId (hcel-text-property-near-point 'location-info))
          (alist-get 'modulePath (hcel-text-property-near-point 'location-info))
          (hcel-text-property-near-point 'internal-id)))
        (t (error "%S not supported and not in eldoc doc buffer." major-mode))))

(provide 'hcel-minor)
