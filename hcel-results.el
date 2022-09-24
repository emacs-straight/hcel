;;; hc-results.el --- Shows query results in a compilation mode -*- lexical-binding: t; -*-

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

;;; Commentary:

;; hcel-results provides functions for hcel modes derived from
;; compilation-mode.

;;; Code:
(require 'hcel-source)
(require 'hcel-utils)
(eval-when-compile (require 'compile))

(defun hcel-results-next-error-no-open (n)
  (interactive "p")
  (hcel-results-next-error-internal n))

(defun hcel-results-previous-error-no-open (n)
  (interactive "p")
  (hcel-results-next-error-no-open (- n)))

(defun hcel-results-next-error-internal (n &optional reset)
  (interactive "p")
  (if reset
      (progn
        (goto-char (point-min))
        (hcel-results-next-error-internal 1 nil))
    (if (> n 0)
        (dotimes (_unused n)
          (condition-case nil
              (progn
                (goto-char (next-single-property-change (point) 'match-line))
                (unless (get-text-property (point) 'match-line)
                  (goto-char
                   (next-single-property-change (point) 'match-line))))
            (error (hcel-results-next-page))))
      (dotimes (_unused (- n))
        (condition-case nil
            (progn
              (goto-char (previous-single-property-change (point) 'match-line))
              (unless (get-text-property (point) 'match-line)
                (goto-char
                 (previous-single-property-change (point) 'match-line))))
          (error (hcel-results-previous-page)))))))

(defun hcel-results-next-error (n &optional reset)
  (interactive "p")
  (hcel-results-next-error-internal n reset)
  (hcel-results-open))

(defun hcel-results-open ()
  (interactive)
  ;; TODO: arrow not working
  (compilation-set-overlay-arrow (selected-window))
  (hcel-load-module-location-info (get-text-property (point) 'location-info)))

(defvar-local hcel-results-page-number nil)
(defvar-local hcel-results-max-page-number nil)

(defun hcel-results-next-page ()
  (interactive)
  (unless (derived-mode-p 'hcel-refs-mode 'hcel-ids-mode)
    (error "Not in hcel-refs or hcel-ids mode: %S" major-mode))
  (when (= hcel-results-page-number hcel-results-max-page-number)
    (error "Already on the last page"))
  (setq hcel-results-page-number (1+ hcel-results-page-number))
  (cond ((derived-mode-p 'hcel-refs-mode) (hcel-refs-update-references))
        ((derived-mode-p 'hcel-ids-mode) (hcel-ids-update))
        (t (error "wrong major mode: %S" major-mode)))
  (hcel-results-next-error-internal 1))

(defun hcel-results-previous-page ()
  (interactive)
  (unless (derived-mode-p 'hcel-refs-mode 'hcel-ids-mode)
    (error "Not in hcel-refs or hcel-ids mode: %S" major-mode))
  (when (= hcel-results-page-number 1)
    (error "Already on the first page."))
  (setq hcel-results-page-number (1- hcel-results-page-number))
  (cond ((derived-mode-p 'hcel-refs-mode) (hcel-refs-update-references))
        ((derived-mode-p 'hcel-ids-mode) (hcel-ids-update))
        (t (error "wrong major mode: %S" major-mode)))
  (goto-char (point-max))
  (hcel-results-next-error-internal -1))

;; hcel-refs-mode
(defcustom hcel-refs-per-page 50
  "hcel refs number of results per page."
  :group 'hcel-refs :type '(natnum))

(defvar-local hcel-refs-id nil)
(defvar-local hcel-refs-package-id nil)

(define-compilation-mode hcel-refs-mode "hcel-refs"
  "Major mode for showing references"
  (setq-local next-error-function #'hcel-results-next-error))

(define-key hcel-refs-mode-map (kbd "M-n")
  #'hcel-results-next-error-no-open)

(define-key hcel-refs-mode-map (kbd "M-p")
  #'hcel-results-previous-error-no-open)

(defun hcel-refs-update-references ()
  "Find references and update the current hcel-refs-mode buffer."
  (unless (derived-mode-p 'hcel-refs-mode)
    (error "Not in hcel-refs mode!"))
  (let ((inhibit-read-only t)
        (modules-refs
         (hcel-api-references
          hcel-refs-package-id hcel-refs-id
          (number-to-string hcel-results-page-number)
          (number-to-string hcel-refs-per-page))))
    (erase-buffer)
    (insert (format "References of %s in %s, Page %d of %d.\n"
                    (hcel-refs-format-id hcel-refs-id)
                    (hcel-format-package-id hcel-refs-package-id)
                    hcel-results-page-number hcel-results-max-page-number))
    (mapc
     (lambda (module-refs)
       (let ((module-path (alist-get 'name module-refs))
             (refs (alist-get 'references module-refs)))
         (insert "References in "
                 (hcel-format-package-id hcel-refs-package-id "-")
                 " " module-path ": \n")
         (mapc
          (lambda (ref)
            (insert
             "--\n"
             (propertize
              (hcel-unquote-html
               (alist-get 'sourceCodeHtml ref))
              'location-info (hcel-id-src-span-to-location-info
                              hcel-refs-package-id module-path
                              (alist-get 'idSrcSpan ref)))))
          refs)))
     modules-refs)
    (goto-char (point-min))
    (while (re-search-forward "<b>\\(.*?\\)</b>" nil t)
      (replace-match
       (propertize (match-string 1) 'font-lock-face 'match))
      (save-excursion
        (add-text-properties (progn (beginning-of-line) (point))
                             (progn (end-of-line) (point))
                             (list 'match-line t)))))
  (goto-char (point-min)))

(defun hcel-refs-reload ()
  (interactive)
  (hcel-refs-update-references))
(define-key hcel-refs-mode-map "g" #'hcel-refs-reload)

(define-key hcel-refs-mode-map "f" #'hcel-results-next-page)

(define-key hcel-refs-mode-map "b" #'hcel-results-previous-page)

(defun hcel-refs-buffer-name (id)
  (format "*hcel-refs %s*" (hcel-refs-format-id id)))

(defun hcel-refs-format-id (id)
  (let* ((tuple (split-string id "|")))
    (format "%s (%s %s)"
            (cadddr tuple) (car tuple) (cadr tuple))))

(defun hcel-refs-update-references-package ()
  "Find references and update the current hcel-refs buffer.

Start by choosing a package."
  (interactive)
  (unless (derived-mode-p 'hcel-refs-mode)
    (error "Not in hcel-refs mode!"))
  (let* ((global-refs (hcel-api-global-references hcel-refs-id))
         (name (cadddr (split-string hcel-refs-id "|")))
         (package-id-and-count
          (split-string
           (completing-read
            (format "References of %s from: " name)
            (mapcar (lambda (pkg-count)
                      (format "%s (%s)"
                              (alist-get 'packageId pkg-count)
                              (alist-get 'count pkg-count)))
                    global-refs)
            nil t)))
         (package-id (car package-id-and-count))
         (count (string-to-number (substring (cadr package-id-and-count) 1 -1)))
         (max-page-number (1+ (/ count hcel-refs-per-page))))
    (setq hcel-refs-package-id (hcel-parse-package-id package-id "-")
          hcel-results-page-number 1
          hcel-results-max-page-number max-page-number)
    (hcel-refs-update-references)))
(define-key hcel-refs-mode-map "P" #'hcel-refs-update-references-package)

(defun hcel-find-references-at-point ()
  "Find references of the identifier at point."
  (interactive)
  (hcel-find-references-internal hcel-package-id hcel-module-path
                                 (hcel-text-property-near-point 'identifier)))
(define-key hcel-mode-map (kbd "M-?") #'hcel-find-references-at-point)

(defun hcel-find-references-internal (package-id module-path identifier)
  (when (and package-id module-path identifier)
    (let ((hcel-buffer (hcel-buffer-name package-id module-path)))
      (when (or (get-buffer hcel-buffer)
                (and (y-or-n-p "Open module source?")
                     (hcel-load-module-source
                      package-id module-path))))
      (with-current-buffer hcel-buffer
        (when-let* ((id (alist-get
                         'externalId
                         (alist-get (intern identifier) hcel-identifiers)))
                    (buffer-name (hcel-refs-format-id id)))
          (with-current-buffer (get-buffer-create buffer-name)
            (hcel-refs-mode)
            (setq hcel-refs-id id)
            (hcel-refs-update-references-package))
          (switch-to-buffer-other-window buffer-name))))))
;; hcel-ids-mode
(defcustom hcel-ids-per-page 20
  "hcel-ids mode number of results per page."
  :group 'hcel-ids :type '(natnum))
(defcustom hcel-ids-live-per-page 10
  "hcel-ids live search results per page."
  :group 'hcel-ids :type '(natnum))

(defvar-local hcel-ids-scope nil)
(defvar-local hcel-ids-query nil)
(defvar-local hcel-ids-package-id nil)

(define-compilation-mode hcel-ids-mode "hcel-ids"
  "Major mode for showing identifiers"
  (setq-local next-error-function #'hcel-results-next-error))

(defun hcel-ids-update ()
  (unless (derived-mode-p 'hcel-ids-mode)
    (error "Not in hcel-ids mode!"))
  (when (and (eq hcel-ids-scope 'package) (not hcel-ids-package-id))
    (error "No package-id supplied for identifiers call!"))
  (let* ((inhibit-read-only t)
         (results
          (hcel-api-identifiers
           hcel-ids-scope hcel-ids-query hcel-ids-package-id
           (number-to-string hcel-results-page-number)
           (number-to-string hcel-ids-per-page)
           t)))
    (erase-buffer)
    (setq hcel-results-max-page-number
          (1+ (/ (string-to-number
                  (alist-get 'X-Total-Count
                             (alist-get 'header results)))
                 hcel-ids-per-page)))
    (insert (format "Results of %s, Page %d of %d.\n"
                    hcel-ids-query hcel-results-page-number
                    hcel-results-max-page-number))
    (mapc
     (lambda (result)
       (let* ((location-info (alist-get 'locationInfo result))
              (doc (hcel-render-html
                    (or (alist-get 'doc result)
                        (alist-get 'documentation
                                   (ignore-errors
                                     (hcel-definition-site-location-info
                                      location-info)))))))
         (insert "--\n")
         (insert (propertize
                  (format "%s :: %s\n"
                          (alist-get 'demangledOccName result)
                          (hcel-render-id-type (alist-get 'idType result)))
                  'location-info location-info
                  'match-line t))
         (insert (format "Defined in %s %s\n"
                         (hcel-format-package-id
                          (alist-get 'packageId location-info) "-")
                         (alist-get 'modulePath location-info)))
         (when doc (insert doc))))
     (alist-get 'json results))
    (goto-char (point-min))))

(defun hcel-ids-reload ()
  (interactive)
  (hcel-ids-update))
(define-key hcel-ids-mode-map "g" #'hcel-ids-reload)

(define-key hcel-ids-mode-map (kbd "M-n")
  #'hcel-results-next-error-no-open)
(define-key hcel-ids-mode-map (kbd "M-p")
  #'hcel-results-previous-error-no-open)
(define-key hcel-ids-mode-map "f" #'hcel-results-next-page)
(define-key hcel-ids-mode-map "b" #'hcel-results-previous-page)

(defun hcel-ids-update-query (query)
  "Search for identities matching query."
  (interactive (list (progn
                       (unless (derived-mode-p 'hcel-ids-mode)
                         (error "Not in hcel-ids mode!"))
                       (read-string "Query: " hcel-ids-query))))
  (setq hcel-ids-query query
        hcel-results-page-number 1)
  (hcel-ids-update))
(define-key hcel-ids-mode-map "s" #'hcel-ids-update-query)

(defun hcel-ids-buffer-name (scope query)
  (format "*hcel-ids-%S %s*" scope query))

;; Caching results to prevent to many hits
(defvar hcel-ids--minibuffer-saved-query nil)
(defvar hcel-ids--minibuffer-saved-results nil)

(defun hcel-ids-minibuffer-collection (scope query &optional package-id)
  (when (and (eq scope 'package) (not package-id))
    (error "No package-id supplied for identifiers call!"))
  (unless (length= query 0)
    (if (string= hcel-ids--minibuffer-saved-query query)
        hcel-ids--minibuffer-saved-results
      (setq hcel-ids--minibuffer-saved-query query
            hcel-ids--minibuffer-saved-results
            (mapcar
             (lambda (result)
               (propertize
                (alist-get 'demangledOccName result)
                'location-info (alist-get 'locationInfo result)))
             (hcel-api-identifiers
              scope query package-id nil
              (number-to-string hcel-ids-live-per-page))))
      hcel-ids--minibuffer-saved-results)))

(defun hcel-global-ids-minibuffer-collection (query &rest _)
  (hcel-ids-minibuffer-collection 'global query))

(defun hcel-package-ids-minibuffer-collection (package-id)
  (lambda (query &rest _)
    (hcel-ids-minibuffer-collection 'package query package-id)))

(defun hcel-ids (scope query &optional package-id)
  (if (length= hcel-ids--minibuffer-saved-results 1)
      (hcel-load-module-location-info
       (with-temp-buffer
         (insert (car hcel-ids--minibuffer-saved-results))
         (get-text-property (point-min) 'location-info)))
    (let ((buffer-name (hcel-ids-buffer-name scope query)))
      (with-current-buffer (get-buffer-create buffer-name)
        (hcel-ids-mode)
        (setq hcel-ids-scope scope
              hcel-ids-package-id package-id)
        (hcel-ids-update-query query))
      (switch-to-buffer buffer-name))))

(defun hcel-global-ids (query)
  (interactive (list
                (let ((minibuffer-allow-text-properties t))
                  (completing-read "Search for identifier globally: "
                                   #'hcel-global-ids-minibuffer-collection))))
  (hcel-ids 'global query))
(define-key hcel-mode-map "I" #'hcel-global-ids)

(defun hcel-package-ids (query)
  (interactive (list
                (let ((minibuffer-allow-text-properties t)
                      (package-id hcel-package-id))
                  (unless (derived-mode-p 'hcel-mode)
                    (error "Not in hcel-mode!"))
                  (completing-read
                   (format "Search for identifier in %s: "
                           (hcel-format-package-id package-id "-"))
                   (hcel-package-ids-minibuffer-collection package-id)))))
  (hcel-ids 'package query hcel-package-id))
(define-key hcel-mode-map "i" #'hcel-package-ids)

(provide 'hcel-results)
;;; hcel-results.el ends here.
