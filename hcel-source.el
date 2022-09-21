;;; hcel-source.el --- displays Haskell module source. -*- lexical-binding: t; -*-

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

(require 'hcel-client)

(setq-local hcel-identifiers nil)
(setq-local hcel-declarations nil)
(setq-local hcel-occurrences nil)
(setq-local hcel-package-id nil)
(setq-local hcel-module-path nil)
(setq-local hcel-highlight-id nil)

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
          (hcel-write-html-source-to-buffer (hcel-source-html json))
          (hcel-fontify-with-haskell-mode)
          ;; it is important the setq of local vars are after the (hcel-mode)
          ;; otherwise they may be rewritten
          (hcel-mode)
          (setq hcel-declarations (alist-get 'declarations json)
                hcel-identifiers (alist-get 'identifiers json)
                hcel-occurrences (alist-get 'occurrences json)
                hcel-package-id package-id
                hcel-module-path module-path)
          (goto-char (point-min)))))
    (get-buffer buffer-name)))

(defun hcel-reload-module-source ()
  "Reloads current module source."
  (interactive)
  (if (equal major-mode 'hcel-mode)
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
		  (equal 
		   (buffer-local-value
        'major-mode
        (get-buffer (if (stringp buffer) buffer (car buffer))))
		   'hcel-mode)))))
(define-key hcel-mode-map "b" #'hcel-switch-buffer)

(defun hcel-lookup-occurrence-at-point ()
  (when-let ((occurrence (get-text-property (point) 'occurrence)))
    (alist-get (intern occurrence) hcel-occurrences)))

(defun hcel-get-location-info (id occ)
  (or (when id (alist-get 'locationInfo id))
      ;; happens for import modules
      (when occ (alist-get 'contents (alist-get 'sort occ)))))

(defun hcel-occ-symbol-at-point ()
  (when-let* ((occ (hcel-text-property-near-point 'occurrence))
              (splitted (split-string occ "-"))
              (line (string-to-number (car splitted)))
              (col-beg (string-to-number (cadr splitted)))
              (col-end (string-to-number (caddr splitted))))
    (hcel-buffer-substring-line-column line (1- col-beg) line (1- col-end))))

(defun hcel-type-at-point ()
  (interactive)
  (hcel-render-type-internal hcel-package-id hcel-module-path
                             (hcel-text-property-near-point 'identifier)))

(defun hcel-render-type-internal (package-id module-path identifier)
  (when (and package-id module-path identifier)
    (let ((hcel-buffer (hcel-buffer-name package-id module-path)))
      (when (get-buffer hcel-buffer)
        (with-current-buffer hcel-buffer
          (when-let* ((id (alist-get (intern identifier)  hcel-identifiers))
                      (id-type
                       (or (alist-get 'idType id)
                           (alist-get 'idOccType
                                      (hcel-lookup-occurrence-at-point)))))
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

(defun hcel-outline-package-module ()
  (interactive)
  (let ((package-id hcel-package-id)
        (module-path hcel-module-path))
    (hcel)
    (hcel-outline-goto-package package-id)
    (hcel-outline-load-modules-at-point)
    (hcel-outline-goto-module module-path)
    (hcel-outline-load-identifiers-at-point)))
(define-key hcel-mode-map "O" #'hcel-outline-package-module)

;; eldoc
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
    (with-current-buffer eldoc--doc-buffer
      (hcel-minor-mode))))

(defun hcel-minor-eldoc-id-type (cb)
  (when-let* ((identifier (hcel-text-property-near-point 'internal-id))
              (symbol (save-excursion
                        (buffer-substring
                         (progn
                           (text-property-search-backward
                            'internal-id identifier 'string=)
                           (point))
                         (progn
                           (text-property-search-forward
                            'internal-id identifier 'string=)
                           (point)))))
              (docstring
               (cond ((eq major-mode 'hcel-outline-mode)
                      (hcel-render-type-internal
                       (hcel-text-property-near-point 'package-id)
                       (hcel-text-property-near-point 'module-path)
                       identifier))
                     ((eq major-mode 'hcel-ids-mode)
                      (hcel-render-type-internal
                       (alist-get 'packageId (hcel-text-property-near-point 'location-info))
                       (alist-get 'modulePath (hcel-text-property-near-point 'location-info))
                       identifier))
                     (t nil))))
    (funcall cb docstring
             :thing symbol
             :face 'font-lock-variable-name-face)
    (with-current-buffer eldoc--doc-buffer
      (hcel-minor-mode))))

(defun hcel-eldoc-docs (cb)
  (when-let ((docstring (hcel-id-docs-at-point)))
    (setq this-command nil)
    (funcall cb docstring)
    (with-current-buffer eldoc--doc-buffer
      (hcel-minor-mode))))

(defun hcel-minor-eldoc-docs (cb)
  (when-let* ((docstring
               (cond ((eq major-mode 'hcel-outline-mode)
                      (hcel-id-docs-internal
                       (hcel-text-property-near-point 'package-id)
                       (hcel-text-property-near-point 'module-path)
                       (hcel-text-property-near-point 'internal-id)))
                     ((eq major-mode 'hcel-ids-mode)
                      (hcel-id-docs-internal
                       (alist-get 'packageId (hcel-text-property-near-point 'location-info))
                       (alist-get 'modulePath (hcel-text-property-near-point 'location-info))
                       (hcel-text-property-near-point 'internal-id)))
                     (t nil))))
    (setq this-command nil)
    (funcall cb docstring)
    (with-current-buffer eldoc--doc-buffer
      (hcel-minor-mode))))

(defun hcel-eldoc-expression-type (cb)
  (when mark-active
    (when-let
        ((expr-and-type
          (hcel-expressions-type (region-beginning) (region-end))))
      (setq this-command nil)
      (funcall cb (cdr expr-and-type)
               :thing (car expr-and-type)
               :face 'font-lock-variable-name-face)
      (with-current-buffer eldoc--doc-buffer
        (hcel-minor-mode)))))

;; highlight
(defface hcel-highlight-id '((t (:inherit underline)))
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
           (prop-match-end match) 'face 'hcel-highlight-id))))))

(defun hcel-highlight-start (id)
  (when id
    (save-excursion
      (goto-char (point-min))
      (let ((match))
        (while (setq match
                     (text-property-search-forward 'identifier id 'string=))
          (add-face-text-property
           (prop-match-beginning match)
           (prop-match-end match) 'hcel-highlight-id))))))

;; utilities
(defun hcel-write-source-line-to-buffer (line)
  (mapc
   (lambda (token)
     (let* ((idInfo (alist-get 'idInfo token))
            (id (alist-get 'identifier idInfo))
            (occ (alist-get 'occurrence idInfo))
            (content (alist-get 'content token)))
       (insert
        (propertize content
                    'identifier (unless (string= id "") id)
                    'occurrence (unless (string= occ "") occ)
                    'cursor-sensor-functions
                    (when id (list #'hcel-highlight-update))))))
   line))

(defun hcel-write-source-to-buffer (lines)
  (mapc
   (lambda (line)
     (hcel-write-source-line-to-buffer (alist-get 'lineContents line))
     (insert "\n"))
   lines))

(defun hcel-write-html-source-line-to-buffer (line)
  (mapc
   (lambda (span)
     (let* ((id (dom-attr span 'data-identifier))
            (occ (dom-attr span 'data-occurrence))
            (content (dom-text span)))
       (insert
        (propertize content
                    'identifier (unless (string= id "") id)
                    'occurrence (unless (string= occ "") occ)
                    'cursor-sensor-functions
                    (when id (list #'hcel-highlight-update))))))
   (dom-by-tag line 'span))
  (insert "\n"))

(defun hcel-write-html-source-to-buffer (lines)
  (mapc
   #'hcel-write-html-source-line-to-buffer
   lines))

(defun hcel-source-html (json)
  (with-temp-buffer
    (insert (alist-get 'sourceCodeHtml json))
    (dom-by-class
     (libxml-parse-html-region (point-min) (point-max))
     "line-content")))

;; imenu
(defun hcel-imenu-create-index ()
  (unless (eq major-mode 'hcel-mode)
    (error "Not in hcel-mode!"))
  (mapcar
   (lambda (decl)
     (cons
      (hcel-render-components
       (alist-get 'components
                  (alist-get 'declType decl))
       (alist-get 'name decl))
      (progn (goto-char (point-min))
             (forward-line (1- (alist-get 'lineNumber decl)))
             (point))))
   hcel-declarations))
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

(defun hcel-minor--xref-backend () 'hcel-minor-xref)
(cl-defmethod xref-backend-definitions ((_backend (eql hcel-minor-xref)) _identifiers)
  (hcel-minor-find-definition-at-point))
(defun hcel-minor-find-definition-at-point ()
  (interactive)
  (cond ((or (eq major-mode 'hcel-outline-mode)
             (eq (current-buffer) eldoc--doc-buffer))
         (hcel-find-definition-internal
          (hcel-text-property-near-point 'package-id)
          (hcel-text-property-near-point 'module-path)
          (hcel-text-property-near-point 'internal-id)))
        ((eq major-mode 'hcel-ids-mode)
         (hcel-find-definition-internal
          (alist-get 'packageId (hcel-text-property-near-point 'location-info))
          (alist-get 'modulePath (hcel-text-property-near-point 'location-info))
          (hcel-text-property-near-point 'internal-id)))
        (t (error "%S not supported and not in eldoc doc buffer." major-mode))))

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
                (when occurrence
                  (alist-get (intern occurrence) hcel-occurrences)))))
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
                  (t
                   (error "unimplemented: %s" (hcel-location-tag location-info))))))))))

;; hcel-minor mode
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
   ((not (or (memq major-mode hcel-minor-major-modes)
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

(provide 'hcel-source)
;;; hcel-source.el ends here.
