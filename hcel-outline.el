;;; hcel-outline.el --- shows package-module-identifier hierarchy in an outline mode -*- lexical-binding: t; -*-

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

(require 'hcel-utils)
(require 'hcel-source)
(require 'outline)

(defvar hcel-outline-buffer-name "*hcel-outline*")
(defvar hcel-outline-indentation 2)

(defvar hcel-outline-mode-map
  (let ((kmap (make-sparse-keymap)))
    (define-key kmap "n"  #'outline-next-visible-heading)
    (define-key kmap "p"  #'outline-previous-visible-heading)
    (define-key kmap "f"  #'outline-forward-same-level)
    (define-key kmap "F"  #'hcel-outline-follow-mode)
    (define-key kmap "b"  #'outline-backward-same-level)
    (define-key kmap "u"  #'outline-up-heading)
    (define-key kmap "\t" #'hcel-outline-toggle-children)
    (define-key kmap "\r" #'hcel-outline-open-thing-at-point)
    (define-key kmap "o"  #'hcel-outline-open-thing-at-point-other-window)
    (define-key kmap "q"  #'quit-window)
    kmap))
(define-derived-mode hcel-outline-mode outline-mode "hcel-outline"
  "Major mode for browsing Haskell codebases."
  (setq-local outline-regexp "\\( *\\)."
              outline-level (lambda () (1+ (/ (length (match-string 1))
                                              hcel-outline-indentation)))
              buffer-read-only t)
  (hcel-minor-mode 1))

(defun hcel ()
  (interactive)
  (let ((buffer (get-buffer hcel-outline-buffer-name)))
    (if buffer (switch-to-buffer buffer)
      (switch-to-buffer (get-buffer-create hcel-outline-buffer-name))
      (save-excursion
        (mapc
         (lambda (package-id)
           (insert
            (concat (propertize
                     (hcel-format-package-id package-id)
                     'thing 'package
                     'package-id package-id
                     'children-loaded nil)
                    "\n")))
         (hcel-api-packages)))
      (hcel-outline-mode))))

(define-key hcel-mode-map "o" #'hcel)

;; TODO: maybe remove
(defun hcel-outline-update-opened (package-id module-path)
  "Update the outline tree depending on openness of packages and modules.

If a package is opened outside of the outline mode (mainly source mode), then
update in the outline mode too."
  (with-current-buffer hcel-outline-buffer-name
    (save-excursion
      (hcel-outline-goto-package package-id)
      (hcel-outline-load-modules-at-point)
      (hcel-outline-goto-module module-path)
      (hcel-outline-load-identifiers-at-point))))

(defun hcel-outline-goto-package (package-id)
  (goto-char (point-min))
  (re-search-forward
   (format "^%s$" (hcel-format-package-id package-id)))
  (beginning-of-line))

(defun hcel-outline-goto-module (module-path)
  "Goto module, assuming point is on the package."
  (re-search-forward
   (format "^%s%s$" (make-string hcel-outline-indentation 32) module-path))
  (beginning-of-line))

(defun hcel-outline-goto-identifier (position)
  "Goto identifier declared at POSITION, assuming point is on the module."
  (text-property-search-forward 'position position))

(defun hcel-outline-load-modules-at-point ()
  (interactive)
  (unless (eq (get-text-property (point) 'thing) 'package)
    (error "Point is not at a package!"))
  (unless (get-text-property (point) 'children-loaded)
    (save-excursion
      (let ((inhibit-read-only t)
            (package-id (get-text-property (point) 'package-id)))
        (put-text-property
         (progn (beginning-of-line) (point))
         (progn (end-of-line) (point))
         'children-loaded t)
        (beginning-of-line 2)
        (mapc
         (lambda (module)
           (insert
            (propertize (concat
                         (make-string hcel-outline-indentation 32) module "\n")
                        'thing 'module
                        'package-id package-id
                        'module-path module
                        'folded t)))
         (hcel-list-modules package-id))))))

(defun hcel-outline-toggle-children ()
  (interactive)
  (let ((thing (get-text-property (point) 'thing))
        (children-loaded (get-text-property (point) 'children-loaded)))
    (cond (children-loaded (outline-toggle-children))
          ((eq thing 'package) (hcel-outline-load-modules-at-point))
          ((eq thing 'module) (hcel-outline-load-identifiers-at-point))
          (t nil))))

(defun hcel-outline-load-identifiers-at-point ()
  (interactive)
  (unless (eq (get-text-property (point) 'thing) 'module)
    (error "Point is not at a module!"))
  (unless (get-text-property (point) 'children-loaded)
    (save-excursion
      (let* ((inhibit-read-only t)
             (package-id (get-text-property (point) 'package-id))
             (module-path (get-text-property (point) 'module-path))
             (imenu-index))
        (put-text-property
         (progn (beginning-of-line) (point))
         (progn (end-of-line) (point))
         'children-loaded t)
        (when (or (get-buffer (hcel-buffer-name package-id module-path))
                  (y-or-n-p "Open module source?"))
          (with-current-buffer
              (hcel-load-module-source package-id module-path)
            (setq imenu-index (save-excursion (hcel-imenu-create-index))))
          (beginning-of-line 2)
          (mapc
           (lambda (pair)
             (insert
              (propertize
               (concat (make-string (* 2 hcel-outline-indentation) 32)
                       (car pair)
                       "\n")
               'thing 'identifier
               'package-id package-id
               'module-path module-path
               'position (cdr pair))))
           imenu-index))))))

(defun hcel-outline-open-module-source-at-point (&optional other-window)
  (interactive)
  (let ((props (text-properties-at (point))))
    (unless (eq (plist-get props 'thing) 'module)
      (error "Point is not at a module!"))
    (let ((buffer
           (hcel-load-module-source
            (plist-get props 'package-id)
            (plist-get props 'module-path))))
      (hcel-outline-load-identifiers-at-point)
      (if other-window
          (switch-to-buffer-other-window buffer)
        (switch-to-buffer buffer)))))

(defun hcel-outline-open-identifier-at-point (&optional other-window)
  (interactive)
  (let ((props (text-properties-at (point))))
    (unless (eq (plist-get props 'thing) 'identifier)
      (error "Point is not at an identifier!"))
    (let ((buffer
           (hcel-load-module-source
            (plist-get props 'package-id)
            (plist-get props 'module-path))))
      (if other-window
          (switch-to-buffer-other-window buffer)
        (switch-to-buffer buffer))
      (goto-char (plist-get props 'position)))))

(defun hcel-outline-open-thing-at-point (&optional other-window)
  (interactive)
  (cond ((eq (get-text-property (point) 'thing) 'module)
         (hcel-outline-open-module-source-at-point other-window))
        ((eq (get-text-property (point) 'thing) 'identifier)
         (hcel-outline-open-identifier-at-point other-window))
        (t nil)))

(defun hcel-outline-open-thing-at-point-other-window ()
  (interactive)
  (let ((current-window (car (window-list))))
    (hcel-outline-open-thing-at-point t)
    (select-window current-window)))

(define-minor-mode hcel-outline-follow-mode
  "Display modules and identifiers as point moves."
  :lighter " hcel-outline-follow"
  :after-hook
  (if hcel-outline-follow-mode
      (if (not (eq major-mode 'hcel-outline-mode))
          (error "Not in hcel-outline mode!")
        (add-hook 'post-command-hook
                  #'hcel-outline-open-thing-at-point-other-window nil t))
    (remove-hook 'post-command-hook
                 #'hcel-outline-open-thing-at-point-other-window t)))

(provide 'hcel-outline)
;;; hcel-outline.el ends here.
