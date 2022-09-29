;;; hcel-source.el --- displays Haskell module source. -*- lexical-binding: t; -*-

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

(require 'array)
(require 'dom)
(require 'hcel-client)
(require 'text-property-search)
(require 'json)
(require 'xref)

(defvar-local hcel-identifiers nil)
(defvar-local hcel-package-id nil)
(defvar-local hcel-module-path nil)
(defvar-local hcel-highlight-id nil)

(define-derived-mode hcel-mode special-mode "hcel"
  "Major mode for exploring Haskell codebases"
  (setq-local eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly
              eldoc-documentation-functions
              '(hcel-eldoc-id-type hcel-eldoc-expression-type hcel-eldoc-docs)
              imenu-create-index-function #'hcel-imenu-create-index
              imenu-space-replacement " ")
  (cursor-sensor-mode 1)
  (add-hook 'xref-backend-functions #'hcel--xref-backend nil t))

(defun hcel-buffer-name (package-id module-path)
  (concat "*hcel " (hcel-format-package-id package-id "-")
          "/" module-path "*"))

(defun hcel-load-module-source (package-id module-path &optional force)
  "Loads module source in a buffer and returns the buffer.

When FORCE is non-nil, kill existing source buffer if any."
  (let ((buffer-name (hcel-buffer-name package-id module-path)))
    (when (or force (not (get-buffer buffer-name)))
      (let ((json (hcel-api-module-info package-id module-path)))
        (ignore-errors (kill-buffer buffer-name))
        (with-current-buffer (get-buffer-create buffer-name)
          ;; (hcel-write-source-to-buffer (alist-get 'tokenizedLines json))
          (hcel-write-html-source-to-buffer (hcel-source-html json)
                                            (alist-get 'occurrences json))
          (hcel-annotate-declarations (alist-get 'declarations json))
          ;; (hcel-fontify-with-haskell-mode)
          ;; it is important the setq of local vars are after the (hcel-mode)
          ;; otherwise they may be rewritten
          (hcel-mode)
          (setq hcel-identifiers (alist-get 'identifiers json)
                hcel-package-id package-id
                hcel-module-path module-path)
          (goto-char (point-min)))))
    (get-buffer buffer-name)))

(defun hcel-reload-module-source ()
  "Reloads current module source."
  (interactive)
  (if (derived-mode-p 'hcel-mode)
      (switch-to-buffer
       (hcel-load-module-source hcel-package-id hcel-module-path t))
    (error "Not in hcel-mode!")))
(define-key hcel-mode-map "g" #'hcel-reload-module-source)

(defun hcel-load-module-location-info (location-info &optional no-jump)
  "Load a module from exact location info.

Example of LOCATION-INFO:

    \"locationInfo\": {
      \"endColumn\": 14,
      \"endLine\": 317,
      \"moduleName\": \"Main\",
      \"modulePath\": \"app/Server.hs\",
      \"packageId\": {
        \"name\": \"hcel\",
        \"version\": \"0.1.0.0\"
      },
      \"startColumn\": 5,
      \"startLine\": 317,
      \"tag\": \"ExactLocation\"
    },

If NO-JUMP is non-nil, just open the source and does not jump to
the location with pulsing.
"
  (unless (string= (hcel-location-tag location-info) "ExactLocation")
    (error "Location tag is not ExactLocation."))
  (when-let ((package-id (alist-get 'packageId location-info))
             (module-path (alist-get 'modulePath location-info))
             (line-beg (alist-get 'startLine location-info))
             (col-beg (alist-get 'startColumn location-info))
             (line-end (alist-get 'endLine location-info))
             (col-end (alist-get 'endColumn location-info))
             (buffer (hcel-load-module-source package-id module-path)))
    (unless no-jump
      (switch-to-buffer-other-window buffer)
      (hcel-goto-line-column line-beg (1- col-beg))
      (pulse-momentary-highlight-region
       (point) (save-excursion
                 (hcel-goto-line-column line-end (1- col-end))
                 (point))
       'next-error))
    buffer))

(defun hcel-switch-buffer ()
  (interactive)
  (switch-to-buffer
   (read-buffer
    "Switch to buffer: " nil t
		(lambda (buffer)
		  (with-current-buffer (if (stringp buffer) buffer (car buffer))
		    (derived-mode-p 'hcel-mode))))))
(define-key hcel-mode-map "b" #'hcel-switch-buffer)

(defun hcel-get-location-info (id occ)
  (or (when id (alist-get 'locationInfo id))
      ;; happens for import modules
      (when occ (alist-get 'contents (alist-get 'sort occ)))))

(defun hcel-occ-symbol-at-point ()
  (when-let* ((col-beg (hcel-text-property-near-point 'span-begin))
              (col-end (hcel-text-property-near-point 'span-end)))
    (save-excursion
      (buffer-substring
       (progn (move-to-column col-beg) (point))
       (progn (move-to-column col-end) (point))))))

(defun hcel-type-at-point ()
  (interactive)
  (hcel-render-type-internal hcel-package-id hcel-module-path
                             (hcel-text-property-near-point 'identifier)
                             (hcel-text-property-near-point 'occurrence)))

(defun hcel-render-type-internal (package-id module-path identifier
                                             &optional occurrence)
  (when (and package-id module-path (or identifier occurrence))
    (let ((hcel-buffer (hcel-buffer-name package-id module-path)))
      (when (get-buffer hcel-buffer)
        (with-current-buffer hcel-buffer
          (let* ((id (when identifier
                       (alist-get (intern identifier) hcel-identifiers)))
                 (id-type (or (alist-get 'idType id)
                              (alist-get 'idOccType occurrence))))
            (concat
             (hcel-render-id-type id-type)
             (when-let* ((external-id (alist-get 'externalId id))
                         (splitted (split-string external-id "|"))
                         (package-id (car splitted))
                         (module-name (cadr splitted)))
               (concat "\nDefined in: " package-id " " module-name "")))))))))

(defun hcel-hoogle-docs-location-info (location-info)
  (when-let* ((package-id (alist-get 'packageId location-info))
              (module-name (alist-get 'moduleName location-info))
              (entity (alist-get 'entity location-info))
              (name (alist-get 'name location-info)))
    (hcel-api-hoogle-docs package-id module-name entity name)))

(defun hcel-id-docs-at-point ()
  (hcel-id-docs-internal hcel-package-id hcel-module-path
                         (hcel-text-property-near-point 'identifier)))

(defun hcel-id-docs-internal (package-id module-path identifier)
  (when (and package-id module-path identifier)
    (let ((hcel-buffer (hcel-buffer-name package-id module-path)))
      (when (get-buffer hcel-buffer)
        (with-current-buffer hcel-buffer
          (when-let*
              ((id (alist-get (intern identifier) hcel-identifiers))
               (location-info (hcel-get-location-info id nil))
               (docs
                (or
                 ;; same module
                 (alist-get 'doc id)
                 ;; other module
                 (alist-get
                  'documentation
                  (ignore-errors
                    (hcel-definition-site-location-info location-info)))
                 ;; hoogle
                 (when-let ((hoogle-docs
                             (ignore-errors
                               (hcel-hoogle-docs-location-info location-info))))
                   (when (length> hoogle-docs 0) (concat "Hoogle: " hoogle-docs))))))
            (hcel-render-html docs)))))))

;; TODO: multiple expressions
(defun hcel-expressions-type (beg end)
  (interactive "r")
  (when mark-active
    (save-excursion
      (let ((line-beg) (col-beg) (line-end) (col-end))
        (goto-char beg)
        (setq line-beg (current-line)
              col-beg (current-column))
        (goto-char end)
        (setq line-end (current-line)
              col-end (current-column))
        (when-let ((expr
                    (ignore-errors
                      (hcel-api-expressions hcel-package-id
                                            hcel-module-path line-beg col-beg
                                            line-end col-end))))
          (unless (length= expr 0)
            (hcel-expression-and-type (elt expr (1- (length expr))))))))))

;; eldoc
(defvar hcel-eldoc-hook nil)

(defun hcel-eldoc-id-type (cb)
  (when-let ((symbol (hcel-occ-symbol-at-point))
             (doc (hcel-type-at-point))
             (docstring
              (propertize
               doc
               'package-id hcel-package-id
               'module-path hcel-module-path)))
    (funcall cb docstring
             :thing symbol
             :face 'font-lock-variable-name-face)
    (run-hooks 'hcel-eldoc-hook)))

(defun hcel-eldoc-docs (cb)
  (when-let ((docstring (hcel-id-docs-at-point)))
    (setq this-command nil)
    (funcall cb docstring)
    (run-hooks 'hcel-eldoc-hook)))

(defun hcel-eldoc-expression-type (cb)
  (when mark-active
    (when-let
        ((expr-and-type
          (hcel-expressions-type (region-beginning) (region-end))))
      (setq this-command nil)
      (funcall cb (cdr expr-and-type)
               :thing (car expr-and-type)
               :face 'font-lock-variable-name-face)
      (run-hooks 'hcel-eldoc-hook))))

;; highlight
(defface hcel-highlight-id-face '((t (:inherit underline)))
  "Face for highlighting hcel identifier at point."
  :group 'hcel-faces)

(defun hcel-highlight-update (&rest _)
  ;; if mark is active, change of face will deactivate the mark in transient
  ;; mark mode
  (unless mark-active
    (let ((id (get-text-property (point) 'identifier))
          (inhibit-read-only t))
      (when (not (string= hcel-highlight-id id))
        (hcel-highlight-stop hcel-highlight-id)
        (hcel-highlight-start id)
        (setq hcel-highlight-id id)))))

(defun hcel-highlight-stop (id)
  (when id
    (save-excursion
      (goto-char (point-min))
      (let ((match))
        (while (setq match
                     (text-property-search-forward 'identifier id 'string=))
          (font-lock--remove-face-from-text-property
           (prop-match-beginning match)
           (prop-match-end match) 'face 'hcel-highlight-id-face))))))

(defun hcel-highlight-start (id)
  (when id
    (save-excursion
      (goto-char (point-min))
      (let ((match))
        (while (setq match
                     (text-property-search-forward 'identifier id 'string=))
          (add-face-text-property
           (prop-match-beginning match)
           (prop-match-end match) 'hcel-highlight-id-face))))))

;; utilities
(defun hcel-write-html-source-line-to-buffer (line occs)
  (mapc
   (lambda (span)
     (let* ((id (dom-attr span 'data-identifier))
            (position (dom-attr span 'data-occurrence))
            (splitted (when position (split-string position "-")))
            (occ (when position (alist-get (intern position) occs)))
            (tag (alist-get 'tag (alist-get 'sort occ)))
            (content (dom-text span)))
       (insert
        (propertize content
                    'identifier (unless (string= id "") id)
                    'span-begin (when splitted
                                  (1- (string-to-number (cadr splitted))))
                    'span-end (when splitted
                                (1- (string-to-number (caddr splitted))))
                    'occurrence occ
                    'face (cond ((equal tag "TypeId") 'hcel-type-face)
                                ((equal tag "ValueId") 'hcel-value-face)
                                ((equal tag "ModuleId") 'hcel-type-face)
                                ((string-match hcel-comment-re content)
                                 'hcel-comment-face)
                                ((string-match hcel-pragma-re content)
                                 'hcel-pragma-face)
                                (t nil))
                    'cursor-sensor-functions
                    (when id (list #'hcel-highlight-update))))))
   (dom-by-tag line 'span))
  (insert "\n"))

(defun hcel-annotate-declarations (decls)
  (save-excursion
    (mapc
     (lambda (decl)
       (goto-char (point-min))
       (forward-line (1- (alist-get 'lineNumber decl)))
       (add-text-properties (point) (1+ (point))
                            (list 'declaration decl)))
     decls)))

(defun hcel-source-next-declaration ()
  (interactive)
  (beginning-of-line)
  (text-property-search-forward 'declaration nil t))
(define-key hcel-mode-map "n" #'hcel-source-next-declaration)

(defun hcel-source-previous-declaration ()
  (interactive)
  (beginning-of-line)
  (text-property-search-backward 'declaration nil t)
  (left-char))
(define-key hcel-mode-map "p" #'hcel-source-previous-declaration)

(defface hcel-type-face '((t :inherit font-lock-type-face))
  "Face used to highlight types" :group 'hcel-faces)
(defface hcel-value-face '((t :inherit font-lock-variable-name-face))
  "Face used to highlight values" :group 'hcel-faces)
(defface hcel-comment-face '((t :inherit font-lock-comment-face))
  "Face used to highlight comments" :group 'hcel-faces)
(defface hcel-pragma-face '((t :inherit font-lock-preprocessor-face))
  "Face used to highlight pragmas" :group 'hcel-faces)
(defface hcel-builtin-face '((t :inherit font-lock-builtin-face))
  "Face used to highlight builtins" :group 'hcel-faces)

(defvar hcel-comment-re "^\\ *--.*$")
(defvar hcel-pragma-re "^\\ *{-# .*? #-}\\ *$")
(defvar hcel-builtin-re "^\\ *\\(module\\|import\\|qualified\\|as\\|if\\|then\\|else\\|in\\|where\\|::\\)\\ *$")


(defun hcel-write-html-source-to-buffer (lines occs)
  (mapc
   (lambda (line)
     (hcel-write-html-source-line-to-buffer line occs))
   lines))

(defun hcel-source-html (json)
  (with-temp-buffer
    (insert (alist-get 'sourceCodeHtml json))
    (dom-by-class
     (libxml-parse-html-region (point-min) (point-max))
     "line-content")))

;; imenu
(defun hcel-imenu-create-index ()
  (hcel-imenu-create-index-internal))

(defun hcel-imenu-create-index-internal (&optional exported-only)
  (unless (derived-mode-p 'hcel-mode)
    (error "Not in hcel-mode!"))
  (goto-char (point-min))
  (let ((index) (match) (exported))
    (while (setq match (text-property-search-forward 'declaration))
      (setq exported (eq (alist-get 'isExported (prop-match-value match)) t))
      (unless (and exported-only (not exported))
        (push (cons
               (propertize
                (hcel-render-components
                 (alist-get 'components
                            (alist-get 'declType (prop-match-value match)))
                 (alist-get 'name (prop-match-value match)))
                'exported exported)
               (1- (point)))
              index)))
    (reverse index)))
(define-key hcel-mode-map "j" #'imenu)

;; xref
(defun hcel--xref-backend () 'hcel-xref)
(cl-defmethod xref-backend-definitions ((_backend (eql hcel-xref)) _identifiers)
  (hcel-find-definition))

(defun hcel-find-definition ()
  (hcel-find-definition-internal
   hcel-package-id hcel-module-path
   (hcel-text-property-near-point 'identifier)
   (hcel-text-property-near-point 'occurrence)))

(defun hcel-find-definition-internal (package-id module-path identifier
                                                 &optional occurrence)
  (when (and package-id module-path (or identifier occurrence))
    (let ((hcel-buffer (hcel-buffer-name package-id module-path)))
      (when (or (get-buffer hcel-buffer)
                (and (y-or-n-p "Open module source?")
                     (hcel-load-module-source
                      package-id module-path))))
      (with-current-buffer hcel-buffer
        (let ((location-info
               (hcel-get-location-info
                (when identifier
                  (alist-get (intern identifier) hcel-identifiers))
                occurrence)))
          (when (string= (hcel-location-tag location-info) "ApproximateLocation")
            (setq location-info (hcel-approx-to-exact-location location-info)))
          (let ((line-beg (alist-get 'startLine location-info))
                (col-beg (alist-get 'startColumn location-info))
                (line-end (alist-get 'endLine location-info))
                (col-end (alist-get 'endColumn location-info)))
            (cond ((string= (hcel-location-tag location-info) "ExactLocation")
                   (let ((pos) (len)
                         (buffer (hcel-load-module-location-info location-info t)))
                     (with-current-buffer buffer
                       (save-excursion
                         (hcel-goto-line-column line-beg col-beg)
                         (setq pos (1- (point)))
                         (hcel-goto-line-column line-end col-end)
                         (setq len (- (point) pos 1))))
                     (list (xref-make-match
                            "hcel match"
                            (xref-make-buffer-location buffer pos)
                            len))))
                  ;; FIXME: error when trying to find definition for an empty
                  ;; string
                  (t nil))))))))

(provide 'hcel-source)
;;; hcel-source.el ends here.
