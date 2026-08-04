;;; init.el --- Personal Emacs configuration -*- lexical-binding: t -*-

;;; Commentary:

;; Personal Emacs configuration

;;; Code:

;;; Core customization

(defconst my/autosaves (file-name-concat user-emacs-directory "autosaves/"))
(defconst my/backups (file-name-concat user-emacs-directory "backups/"))

(defun my/ensure-directory (d) (unless (file-exists-p d) (make-directory d)))

(use-package emacs
  ;; [[id:2E2BA485-DA8E-41AF-962F-F44046D546AE][Emacs]]
  :bind (("C-x x l" . #'display-line-numbers-mode)
         ("C-x x w" . #'toggle-word-wrap)
         ("C-x x a" . #'auto-fill-mode)
         ("M-o" . #'other-window)
         ("M-O" . #'other-frame))
  :init
  (my/ensure-directory my/autosaves)
  (my/ensure-directory my/backups)
  :custom
  ;; File Management
  (custom-file (file-name-concat user-emacs-directory "custom.el"))
  (backup-directory-alist `(("." . ,my/backups)))
  (auto-save-file-name-transforms `((".*" ,my/autosaves t)))
  (require-final-newline t)
  ;; macOS Integration
  (mac-command-modifier 'meta)
  (mac-option-modifier 'super)
  ;; Editor Behavior
  (show-paren-style 'expression)
  (blink-cursor-blinks 0)
  ;; Minibuffer
  (enable-recursive-minibuffers t)
  (use-short-answers t)
  ;; UI Elements
  (tool-bar-mode nil)
  (scroll-bar-mode nil)
  (fringe-mode '(8 . 8))
  (indicate-buffer-boundaries 'left)
  (indicate-empty-lines t)
  (scroll-conservatively 100)
  :config
  (load custom-file 'noerror)
  ;; Global Modes
  (electric-pair-mode)
  (global-subword-mode)
  (global-auto-revert-mode)
  (recentf-mode)
  (savehist-mode)
  (save-place-mode)
  (column-number-mode)
  (pixel-scroll-precision-mode)
  ;; et cetera~
  (add-to-list 'load-path (file-name-concat user-emacs-directory "my/")))

;;; Emacs Lisp Package Archives

(use-package use-package
  :defer t
  :custom
  (package-archives
   '(("gnu" . "https://elpa.gnu.org/packages/")
     ("nongnu" . "https://elpa.nongnu.org/nongnu/")
     ("melpa" . "https://melpa.org/packages/")))
  :init
  (package-initialize))

;;; System Package Dependencies

(use-package exec-path-from-shell
  :ensure t
  :init
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package system-packages
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Display & Themes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Fonts

(defun monaspace (variant)
  (pcase variant
    ('neon "MonaspiceNe Nerd Font Mono")    ;; neo-grotesque sans
    ('argon "MonaspiceAr Nerd Font Mono")   ;; humanist sans
    ('xenon "MonaspiceXe Nerd Font Mono")   ;; slab serif
    ('radon "MonaspiceRn Nerd Font Mono")   ;; handwriting
    ('krypton "MonaspiceKr Nerd Font Mono") ;; mechanical sans
    (_ (error "Unknown monaspace variant: %s" variant))))

(use-package emacs ;; [[id:9A7C0849-FBC1-4222-8B80-F36F2233D554][monaspace]]
  :after org
  :ensure-system-package
  ;; checking for one of the font files to verify the installation
  ("/Users/jasbrg/Library/Fonts/MonaspiceArNerdFont-Regular.otf"
   . "brew install --cask font-monaspice-nerd-font")
  :custom-face
  (default                   ((t (:family ,(monaspace 'xenon)
                                  :foundry "nil"
                                  :slant normal
                                  :weight regular
                                  :height 130
                                  :width normal))))
  :config
  (dolist (mapping '((neon . (font-lock-keyword-face
                              font-lock-builtin-face))
                     (argon . (font-lock-variable-name-face
			       font-lock-string-face
			       org-block org-date org-property-value))
		     (xenon . (font-lock-type-face
			       font-lock-constant-face
			       org-document-title
			       org-link
			       org-verbatim))
		     (radon . (font-lock-comment-face variable-pitch))
		     (krypton . (font-lock-function-name-face
				 org-block-begin-line
				 org-block-end-line
				 org-code
				 org-document-info-keyword
				 org-drawer
				 org-meta-line
				 org-special-keyword
				 fixed-pitch))))
    (dolist (face (cdr mapping))
      (set-face-attribute face nil :family (monaspace (car mapping))))))

(use-package diminish
  :demand t
  :ensure t
  :config
  (diminish 'subword-mode)
  (diminish 'visual-line-mode))

(use-package face-remap
  :hook (org-mode . variable-pitch-mode)
  :diminish buffer-face-mode)

;;;; Color Theme

(consult-theme 'modus-operandi-tinted)

;;;; ANSI Color Support

(use-package ansi-color
  :hook (compilation-filter . ansi-color-compilation-filter))

;;;; Highlighting Todo

(use-package hl-todo
  :ensure t
  :custom
  (hl-todo-exclude-modes '())
  (hl-todo-keyword-faces
   '(("HOLD"      . "#d0bf8f")
     ("TODO"      . "#cc9393")
     ("NEXT"      . "#dca3a3")
     ("THEM"      . "#dc8cc3")
     ("PROG"      . "#7cb8bb")
     ("OKAY"      . "#7cb8bb")
     ("DONT"      . "#5f7f5f")
     ("FAIL"      . "#8c5353")
     ("DONE"      . "#afd8af")
     ("NOTE"      . "#d0bf8f")
     ("MAYBE"     . "#d0bf8f")
     ("KLUDGE"    . "#d0bf8f")
     ("HACK"      . "#d0bf8f")
     ("TEMP"      . "#d0bf8f")
     ("FIXME"     . "#cc9393")
     ("XXXX*"     . "#cc9393")
     ;; custom keywords
     ("PROMPT"    . "#7cb8bb")
     ("RESPONSE"  . "#dc8cc3")
     ("WAITING"   . "#dc8cc3")
     ("SOMEDAY"   . "#d0bf8f")
     ("CANCELLED" . "#8c5353")))
  :config
  (global-hl-todo-mode))

;;;; Window Margins

(use-package olivetti
  :ensure t
  :diminish
  :bind (("C-x x o" . #'olivetti-mode))
  :hook
  (text-mode . olivetti-mode)
  :custom
  (fill-column 80))

;;;; Window Management

(use-package shackle
  :vc t
  :commands shackle-mode
  :demand t
  :custom
  ;; hides the bare org-capture buffer, for suppressing window
  ;; movement for a log
  (shackle-rules '(("*Capture*" :ignore t)))
  :config
  (shackle-mode))

;;;; Point visibility

(use-package pulsar
  :ensure t
  :bind
  (:map global-map
        ("C-x l" . #'pulsar-pulse-line)
        ("C-x L" . #'pulsar-highlight-temporarily))
  :init
  (pulsar-global-mode 1)
  :custom
  (pulsar-delay 0.05)
  (pulsar-iterations 10)
  :config
  (add-to-list 'pulsar-pulse-functions 'forward-sentence)
  (add-to-list 'pulsar-pulse-functions 'backward-sentence)
  (add-to-list 'pulsar-pulse-functions 'forward-paragraph)
  (add-to-list 'pulsar-pulse-functions 'backward-paragraph))

;;;; Mode Line

(use-package project-mode-line-tag
  :ensure t
  :config
  (project-mode-line-tag-mode 1))

;;;; Tablist (dependency for other packages)

(use-package tablist
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Navigation, Interaction & Editing ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; S-Expression Movements

(use-package paredit
  :ensure t
  :diminish
  :hook ((emacs-lisp-mode clojure-mode cider-repl-mode) . #'enable-paredit-mode))

;;;; Buffer Hygiene

(use-package whitespace-cleanup-mode
  :ensure t
  :diminish whitespace-cleanup-mode)

;;;; Minibuffer & Completion Improvements

(use-package vertico
  :ensure t
  :config
  (vertico-mode)
  (use-package vertico-sort
    :custom (vertico-sort-function #'vertico-sort-history-length-alpha))
  (use-package vertico-mouse
    :config
    (vertico-mouse-mode)))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic)))

(use-package corfu
  :ensure t
  :bind (:map corfu-map ("SPC" . #'corfu-insert-separator))
  :config
  (global-corfu-mode)
  (corfu-popupinfo-mode)
  :custom
  (corfu-popupinfo-delay '(0.5 . 0.2)))

(use-package cape
  :ensure t
  :bind
  ("C-c p" . cape-prefix-map)
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block))

(use-package consult
  :ensure t
  :ensure-system-package (rg)
  :bind (("M-g M-g" . #'consult-line)
         ("M-g M-f" . #'consult-focus-lines)
         ("M-g M-a" . #'consult-org-agenda)
         ("M-g g"   . #'consult-goto-line)
         ("M-g M-s" . #'consult-ripgrep)
         ("M-g M-x" . #'consult-mode-command)
         ("M-g M-m" . #'consult-minor-mode-menu)
         ("M-g i"   . #'consult-imenu)
         ("M-g o"   . #'consult-outline)
         ("M-g M-o" . #'consult-outline)
         ("M-g m"   . #'consult-mark)
         ("M-g f"   . #'consult-find)
         ("C-x r b" . #'consult-bookmark)
         ("C-c b"   . #'consult-bookmark)
         ("M-y"     . #'consult-yank-pop)
         ("C-x b"   . #'consult-buffer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Discoverability ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package which-key
  :diminish
  :config
  (which-key-mode))

(use-package eldoc
  :diminish)

(use-package embark
  :ensure t
  :bind (("C-." . #'embark-act)
         ("C-;" . #'embark-dwim)
         ("C-h B" . #'embark-bindings))
  :init
  (setq prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult
  :ensure t
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package marginalia
  :ensure t
  :config
  (marginalia-mode)
  :bind (:map minibuffer-mode-map ("C-." . #'marginalia-cycle)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Programming Language Major Modes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Web Languages

(use-package web-beautify
  :ensure t)

;;;; Emacs Lisp

(use-package elisp-mode
  :bind (:map emacs-lisp-mode-map
              ("C-c C-k" . #'eval-buffer)
              ("C-c C-c" . #'eval-defun)))

(use-package inspector
  :ensure t)

;;;; Clojure

(use-package clojure-mode
  :ensure t)

(use-package cider
  :ensure t
  :config
  ;; macOS + cider interaction bug when using full screen mode: the
  ;; tool tip will not overlay the full screen Emacs, but flip to a new
  ;; desktop screen just for the tool tip.  Annoying.
  (customize-set-variable 'cider-use-tooltips nil))

(use-package clj-refactor
  :ensure t)

;;;; Ledger

(use-package ledger-mode
  :ensure t)

(use-package csv-mode
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Org Mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Core Org

(defun my/org-indent-subtree ()
  (interactive)
  (ignore-errors
    (save-excursion
      (org-narrow-to-subtree)
      (org-indent-region (point-min) (point-max))
      (widen))))

(use-package org
  :bind (("C-c o a" . #'org-agenda))
  :hook ((org-mode . (lambda ()
                       (add-hook 'before-save-hook 'my/org-indent-subtree nil t)))
	 (org-mode . (lambda ()
		       (show-paren-local-mode -1))))
  :custom-face
  (org-table ((t (:inherit 'fixed-pitch))))
  :custom
  (org-log-done 'note)
  (org-log-into-drawer t)
  (org-directory "~/Documents/Org")
  (org-agenda-span 'week)
  (org-property-format "%-15s %s") ;; mainly to align the timestamps.
  ;; TIME_MODIFIED is the longest key.  plus three for comment format.
  (org-edit-src-content-indentation 0)
  (org-log-note-headings '((done . "NOTE %t")
			   (state . "State %-12s from %-12S %t")
			   (note . "NOTE %t")
			   (reschedule . "Rescheduled from %S on %t")
			   (delschedule . "Not scheduled, was %S on %t")
			   (redeadline . "New deadline from %S on %t")
			   (deldeadline . "Removed deadline, was %S on %t")
			   (refile . "Refiled on %t")
			   (clock-out . "")))
  (org-reverse-note-order nil)
  (org-log-state-notes-into-drawer t)
  (org-todo-keywords '((sequence "TODO(t)"
				 "NEXT(n!)"
				 "WAITING(w@/!)"
				 "SOMEDAY(s)"
				 "|"
				 "DONE(d!)"
				 "CANCELLED(c@)")))
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((shell . t))))

(use-package org-id
  :after org
  :custom
  (org-id-link-to-org-use-id 'create-if-interactive))

(use-package orglink ;; global links
  :ensure t
  :diminish
  :config
  (global-orglink-mode 1))

;;;; Org Mem/Node

(use-package org-mem
  ;; [[id:34CA82F7-D038-4ED2-A81F-CB45CCA1D7D2][Org-Mem]]
  :ensure t
  :custom
  (org-mem-watch-dirs (list (expand-file-name org-directory)))
  (org-mem-exclude '("/chats/" "/logseq/bak/" "/logseq/version-files/" "/node_modules/" ".sync-conflict-" "/backup" "/.#" "/." "/_"))
  :config
  (org-mem-updater-mode))

(use-package org-agenda
  :after org-mem
  :init
  (defun my/set-agenda-files (&rest _)
    (setq org-agenda-files
          (cl-loop for file in (org-mem-all-files)
                   unless (string-search "archive" file)
                   as entries = (org-mem-entries-in-file file)
                   when (seq-find (##or (org-mem-entry-active-timestamps %)
                                        (org-mem-entry-todo-state %)
                                        (org-mem-entry-scheduled %)
                                        (org-mem-entry-deadline %))
                                  entries)
                   collect file)))
  (defun my/dashboard-refresh-silently (&rest _)
    (save-window-excursion
      (dashboard-refresh-buffer)))
  :config
  (add-hook 'org-mem-post-full-scan-functions #'my/set-agenda-files)
  (add-hook 'org-mem-post-full-scan-functions #'my/dashboard-refresh-silently t))

(use-package org-node
  ;; [[id:DEEE16AC-0637-43E0-A029-4A932B25F8E5][Org Node]]
  :vc t
  :defer t
  :after (consult org-mem org-agenda)
  :bind (("C-c n c" . #'org-capture)
         ("C-c C-<return>" . #'org-store-link)
         ("s-<return>" . (lambda () (interactive) (org-capture nil "c")))
         ("s-\\" . (lambda () (interactive) (org-capture nil "z")))
         :map org-mode-map
         ("C-c n i" . #'org-node-insert-link*))
  :init
  (keymap-set global-map "C-c n" org-node-global-prefix-map)
  (with-eval-after-load 'org
    (keymap-set org-mode-map "C-c n" org-node-org-prefix-map))
  :custom
  (org-node-file-timestamp-format "")
  (org-node-backlink-do-drawers nil)
  (org-node-file-directory-ask (file-name-concat org-directory "notes"))
  (org-node-property-crtime "TIME_CREATED")
  (org-node-property-mtime  "TIME_MODIFIED")
  (org-node-blank-input-hint nil)
  (org-node-display-sort-fn #'org-node-sort-by-mtime-property)
  (org-node-creation-fn #'org-capture)
  (org-node-seq-defs
   (list
    (org-node-seq-def-on-any-sort-by-property
     "c" "All notes by property :TIME_CREATED:" "TIME_CREATED")
    (org-node-seq-def-on-any-sort-by-property
     "m" "All notes by property :TIME_MODIFIED:" "TIME_MODIFIED")))
  :config
  (org-node-cache-mode)
  (org-node-complete-at-point-mode)
  (org-node-backlink-mode)
  (defun my/org-node-sort-properties ()
    "Sort properties in property drawer alphabetically."
    (org-back-to-heading-or-point-min)
    (re-search-forward org-property-drawer-re (org-entry-end-position))
    (let ((end (pos-bol))
          (beg (progn (goto-char (match-beginning 0))
                      (forward-line 1)
                      (point))))
      (sort-lines nil beg end)))
  (add-hook 'org-node-modification-hook #'my/org-node-sort-properties 50)
  (add-hook 'org-node-modification-hook #'org-node-update-mtime-property))

(use-package org-ql
  :ensure t)

;;;; Org Capture Templates

(use-package org-capture
  :after org
  :custom
  (org-capture-templates
   '(("e" "Capture entry into ID node"
      entry (function org-node-capture-target) "* %?")
     ("p" "Capture plain text into ID node"
      plain (function org-node-capture-target) nil
      :empty-lines-after 1)
     ("j" "Jump to ID node"
      plain (function org-node-capture-target) nil
      :prepend t
      :immediate-finish t
      :jump-to-captured t)
     ("q" "Make quick stub ID node"  ; Handy after `org-node-insert-link'
      plain (function org-node-capture-target) nil
      :immediate-finish t)
     ("c" "Capture log"
      entry (file+olp+datetree "~/Documents/Org/journal.org") "* %U\n%^{Log}"
      :tree-type week
      :kill-buffer t
      :jump-to-captured nil
      :immediate-finish t)
     ("C" "Capture notable log"
      entry (file+olp+datetree "~/Documents/Org/journal.org") "* %T %^{Log}%?"
      :tree-type week
      :jump-to-captured t
      :immediate-finish t)
     ("z" "Capture link"
      item (file+olp+datetree "~/Documents/Org/journal.org") "%U %a%?"
      :tree-type week
      :immediate-finish t)
     ("s" "Dated log"
      entry (file+olp+datetree "~/Documents/Org/journal.org") "* %?\nSCHEDULED: %T"
      :tree-type week
      :time-prompt t)
     ("t" "Capture task" entry (file "~/Documents/Org/tasks.org")
      "* TODO %?\n:PROPERTIES:\n:ID: %(org-id-new)\n:TIME_CREATED: %U\n:END:\n"))))

;;;; Blog configuration

(defun my/keybind (f)
  "Helper function, intended for Org-Mode exports
e.g. src_elisp{(my/keybind 'gptel)}"
  (key-description (where-is-internal f nil t)))

(defvar my/org-blog-directory "~/Documents/Org/blog") ;; write
(defvar my/html-blog-directory "~/Public/blog")       ;; read

;;;; HTML Templating in Emacs-Lisp

(use-package esxml :ensure t)

(defun my/org-blog-relative-path (info)
    (thread-first
      info
      (plist-get :input-file)
      (file-relative-name my/org-blog-directory)
      (split-string "/")
      (butlast)))

(defun my/org-blog-section-bar (parts)
  ;; Meant to resemble HTML exports for Org-Mode tables.
  (esxml-to-xml
   `(table ((border . "2") (cellspacing . "0") (cellpadding . "6") (rules . "groups") (frame . "hsides"))
           (colgroup () ,@(mapcar (lambda (_) '(col ((class . "org-left"))))
                                  (cons "Home" parts)))
           (tbody () (tr ()
                         (td ((class . "org-left"))
                             (a ((href . "/index.html"))
                                (code () "Home")))
                         ,@(mapcar (lambda (part)
                                     `(td ((class . "org-left"))
                                          (a ((href . ,(file-name-concat "/" part "index.html")))
                                             (code () ,part))))
                                   parts))))))

(defun my/org-blog-breadcrumb-bar (info)
  (my/org-blog-section-bar (my/org-blog-relative-path info)))

;;;; Blog Publishing Configuration

(defun my/org-publish-filter-publishing (orig-fn format &rest args)
  (if (and (stringp format) (string-prefix-p "Publishing" format))
    (apply orig-fn format args)))

(defun my/org-publish-all ()
  (interactive)
  (advice-add 'message :around #'my/org-publish-filter-publishing)
  (unwind-protect
      (org-publish-all)
    (advice-remove 'message #'my/org-publish-filter-publishing)))

(use-package ox-publish
  ;; :preface, not :init — the jotbook defvars must exist before the
  ;; :custom alist below is evaluated
  :preface (require 'jotbook)
  :custom
  (org-html-validation-link nil)
  (org-publish-project-alist
   `(("blog"
      :recursive t
      :base-directory ,my/org-blog-directory
      :publishing-function org-html-publish-to-html
      :publishing-directory ,my/html-blog-directory
      :html-head "<link rel='stylesheet' type='text/css' href='/css/my.css'>"
      :html-head-include-scripts nil
      :html-head-include-default-style nil
      :with-author nil
      :with-date nil
      :with-email nil
      :html-preamble my/org-blog-breadcrumb-bar
      :html-postamble nil)
     ;; ("posts"
     ;;  :base-directory ,(file-name-concat my/org-blog-directory "posts")
     ;;  :publishing-directory ,(file-name-concat my/html-blog-directory "Posts")
     ;;  :publishing-function org-html-publish-to-html
     ;;  :html-head "<link rel='stylesheet' type='text/css' href='/css/my.css'>"
     ;;  :html-head-include-scripts nil
     ;;  :html-head-include-default-style nil
     ;;  :recursive t
     ;;  :auto-sitemap t
     ;;  :html-preamble my/org-blog-breadcrumb-bar
     ;;  :sitemap-filename "index.org")
     ;; ("notes"
     ;;  :base-directory ,(file-name-concat my/org-blog-directory "notes")
     ;;  :publishing-directory ,(file-name-concat my/html-blog-directory "Notes")
     ;;  :publishing-function org-html-publish-to-html
     ;;  :html-head "<link rel='stylesheet' type='text/css' href='/css/my.css'>"
     ;;  :html-head-include-scripts nil
     ;;  :html-head-include-default-style nil
     ;;  :auto-sitemap t
     ;;  :html-preamble my/org-blog-breadcrumb-bar
     ;;  :sitemap-filename "index.org")
     ("images"
      :base-extension "png"
      :base-directory ,(file-name-concat my/org-blog-directory "images")
      :publishing-directory ,(file-name-concat my/html-blog-directory "images")
      :publishing-function org-publish-attachment)
     ("css"
      :base-extension "css"
      :base-directory ,(file-name-concat my/org-blog-directory "css")
      :publishing-directory ,(file-name-concat my/html-blog-directory "css")
      :publishing-function org-publish-attachment)
     ("jotbooks-pages"
      :base-directory ,my/jotbooks-staging-directory
      :publishing-directory ,my/jotbooks-html-directory
      :publishing-function org-html-publish-to-html
      :recursive t
      :preparation-function my/jotbooks-prepare
      :html-head ,(concat "<link rel='stylesheet' type='text/css' href='/css/my.css'>"
                          my/jotbooks-html-head)
      :html-head-include-scripts nil
      :html-head-include-default-style nil
      :with-author nil
      :with-date nil
      :with-email nil
      :with-toc nil
      :section-numbers nil
      ;; a stray unresolvable link in a meta note must not abort the publish
      :with-broken-links mark
      :html-preamble my/jotbooks-breadcrumb
      :html-postamble nil)
     ("jotbooks-images"
      :base-directory ,my/jotbooks-staging-directory
      :base-extension "jpg\\|xml"
      :recursive t
      :publishing-directory ,my/jotbooks-html-directory
      :publishing-function org-publish-attachment)
     ("jotbooks" :components ("jotbooks-pages" "jotbooks-images"))
     ("site" :components ("blog" "images" "css" "jotbooks")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Applications ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Directory Editing

(use-package dired
  :ensure-system-package (gls . coreutils)
  :custom
  (dired-listing-switches "--group-directories-first -GlhF")
  :hook (dired-mode . dired-hide-details-mode))

;;;; Calendar

(use-package calendar
  :custom
  (calendar-latitude 36.0)
  (calendar-longitude -84.25)
  (diary-file "~/Documents/Org/Diary"))

;;;; Dashboard

(use-package dashboard
  :after org-agenda
  :ensure t
  :bind (("C-c o RET" . #'dashboard-open))
  :custom
  (dashboard-items '((recents . 5)
                     (bookmarks . 5)
                     (projects . 5)
                     (agenda . 5)))
  :config
  (dashboard-setup-startup-hook))

;;;; Spell checking

(use-package jinx
  :ensure t
  :ensure-system-package
  ((enchant-2 . "brew install enchant")
   (pkg-config . "brew install pkg-config"))
  :custom
  (jinx-languages "en_US")
  :bind (("s-;" . #'jinx-correct)
         ("s-:" . #'jinx-correct-all)))

;;;; Document Reader

(use-package pdf-tools
  :ensure t
  :config
  (pdf-tools-install))

;;;; Chatbot assistant

(use-package gptel
  :vc t
  :bind (("C-c g g"     . #'gptel)
         ("C-c g TAB"   . #'gptel-menu)
         ("C-c g SPC"   . #'gptel-add)
         ("C-c g S-SPC" . #'gptel-add-file)
         ("C-c g RET"   . #'gptel-send)
         ("C-c g DEL"   . #'gptel-context-remove-all)
         ("C-c g p"     . #'gptel-org-set-properties))
  :custom
  (gptel-model 'claude-opus-4.6)
  (gptel-default-mode 'org-mode)
  (gptel-system-prompt
   (concat
    "You are a large language model living in Emacs and a helpful assistant. Respond concisely.\n"
    "Use Org-mode colon syntax (: text) for text examples you output including Org-mode.\n"
    "Use Org-mode block syntax (#+begin_src) for code examples you output that may be executed."))
  (gptel-prompt-prefix-alist '((markdown-mode . "# PROMPT ")
                               (org-mode      . "* PROMPT ")
                               (text-mode     . "# PROMPT ")))
  (gptel-response-prefix-alist '((markdown-mode . "# RESPONSE\n")
				 (org-mode      . "* RESPONSE\n")
				 (text-mode     . "# RESPONSE\n")))
  :config
  (setq gptel-backend (gptel-make-gh-copilot "Copilot"))
  ;; NOTE Need to buy tokens...
  ;; (gptel-make-anthropic "Claude"
  ;; 	  :stream t
  ;; 	  :key gptel-api-key)
  )

(use-package gptel-agent
  :vc t
  :bind (("C-c g a" . #'gptel-agent)))

;;;; Terminal Emulator

(use-package vterm
  :ensure t
  :bind (("C-c t" . vterm)
         ("C-c T" . vterm-other-window))
  :custom
  (vterm-max-scrollback 10000))

;;;; HTTP Server

(defun my/http-server ()
  (interactive)
  (async-shell-command "python3 -m http.server" "*HTTP*"))

;;;; Task support

(use-package consult-todo
  :ensure t
  :bind (("M-g M-t" . #'consult-todo)))

(use-package magit-todos
  :ensure t)

;;;; Source Control

(use-package magit
  :ensure t
  :bind (("C-x g" . #'magit))
  :config
  ;; NOTE: this is going to chug on large repos
  (add-hook 'after-save-hook #'magit-after-save-refresh-status t))

(use-package buttercup
  :ensure t)

(use-package git-auto-commit-mode
  :vc t
  :config
  (diminish 'git-auto-commit-mode "AUTOCOMMIT")
  :custom
  (gac-automatically-push-p nil)
  (gac-default-message nil))

(use-package forge
  :ensure t
  :after magit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'init)

;; Local Variables:
;; truncate-lines: t
;; End:

;;; init.el ends here
