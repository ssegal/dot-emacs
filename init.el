;; -*- lexical-binding: t; -*-

(when (version< emacs-version "27.1")
  (error "Emacs version 27.1 or later required"))

;; Prefer newer files.  This is first so that it affects all
;; subsequent loads.
(setq load-prefer-newer t)

;; Add a local path for non-package.el themes.
(when (boundp 'custom-theme-load-path)
  (add-to-list 'custom-theme-load-path (expand-file-name "themes"  user-emacs-directory)))

;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)

;; Reset to a more reasonable value after startup for better responsiveness
(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold 800000)))

;; Move customizer stuff to another file so it doesn't clog up this
;; one.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

;; Backups, go hide!
(defconst my/backup-directory (expand-file-name "backups" user-emacs-directory))
(unless (file-directory-p my/backup-directory)
  (make-directory my/backup-directory))
(setq backup-directory-alist `((".*" . ,my/backup-directory))
      delete-old-versions t
      version-control t)

;; Auto-saves, go hide too!
(defconst my/auto-save-directory (expand-file-name "autosaves" user-emacs-directory))
(unless (file-directory-p my/auto-save-directory)
  (make-directory my/auto-save-directory))
(setq auto-save-file-name-transforms
      `((".*" ,my/auto-save-directory t)))

;; Make my own local lisp directory
(defconst my/lisp-directory (expand-file-name "lisp" user-emacs-directory))
(unless (file-directory-p my/lisp-directory)
  (make-directory my/lisp-directory))
(add-to-list 'load-path my/lisp-directory)

;; package.el
(require 'package)
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa" . "https://melpa.org/packages/"))
      package-archive-priorities
      '(("melpa" . 10)
        ("gnu" . 5)
        ("nongnu" . 5)))

(unless (fboundp 'use-package)
  ;; If we don't have use-package, then this is a first-time running
  ;; on an Emacs without use-package built-in.  In this case, we
  ;; should install it with package.el.
  (setq package-enable-at-startup nil)
  (package-initialize)
  (package-refresh-contents)
  (package-install 'use-package)

  (eval-when-compile
    (require 'use-package)))

;; Have use-package default to downloading the package if it doesn't
;; exist.  This can still be overridden on a package-specific basis
;; with ":ensure nil"
(setq use-package-always-ensure t)

;; Defer by default, since most packages have good autoloads.
(setq use-package-always-defer t)

(use-package bind-key :demand t)

;; Use ibuffer
(bind-key "C-x C-b" 'ibuffer)

;;;; EXTRA KEYMAPPINGS (default)
(when (eq system-type 'darwin)
  (bind-key [home] 'beginning-of-line)
  (bind-key [end] 'end-of-line)
  (bind-key "M-RET" 'toggle-frame-fullscreen))

(cua-mode 1)
(bind-key "C-x r q" 'save-buffers-kill-emacs)
(bind-key "C-c r" 'revert-buffer)

;; recentf-mode
(use-package recentf
  :demand t
  :init
  (setq recentf-exclude '("/elpa/" ".recentf" "COMMIT_EDITMSG"))
  :config
  (recentf-mode 1)
  :bind ("C-x C-r" . recentf-open-files))

(use-package vscode-dark-plus-theme
  :demand t
  :init (load-theme 'vscode-dark-plus t))

(tab-bar-mode)
(xterm-mouse-mode 1)
(delete-selection-mode 1)
(transient-mark-mode 1)

(use-package kkp
  :hook (tty-setup . global-kkp-mode))

(use-package rg
  :bind (("C-c s" . rg-menu))
  :config
  (rg-enable-default-bindings))

;; Default to UTF-8
(set-language-environment "UTF-8")

(use-package helpful
  :bind
  (([remap describe-function] . helpful-callable)
   ([remap describe-command] . helpful-command)
   ([remap describe-variable] . helpful-variable)
   ([remap describe-key] . helpful-key)
   ("C-c C-d" . helpful-at-point)
   ("C-h F" . helpful-function)))

;;;; GIT STUFF

(use-package with-editor
  :hook ((shell-mode . with-editor-export-editor)
         (term-exec . with-editor-export-editor)
         (eshell-mode . with-editor-export-editor)))

(use-package magit
  :defer t
  :init
  (require 'git-commit))

;;;; PROGRAMMING

;; treesit-auto will pull ABI 15 grammars.  Let's not even try to deal
;; with ABI 14.  For some reason this doesn't work in a use-package
;; :if block, so we'll handle it outside.
(when (and (fboundp 'treesit-available-p)
           (treesit-available-p)
           (<= 15 (treesit-library-abi-version)))
  (use-package treesit-auto
    :demand t
    :custom
    (treesit-auto-install 'prompt)
    :config
    (treesit-auto-add-to-auto-mode-alist 'all)
    (global-treesit-auto-mode))

  ;; Use git-commit-ts-mode for commit messages, but only once its
  ;; tree-sitter grammar is actually available; otherwise git-commit
  ;; falls back to its default major mode.
  (use-package git-commit-ts-mode
    :after (treesit-auto git-commit)
    :init
    (add-to-list 'treesit-language-source-alist
                 '(gitcommit "https://github.com/gbprod/tree-sitter-gitcommit"))
    :config
    (when (treesit-ready-p 'gitcommit)
      (setq git-commit-major-mode 'git-commit-ts-mode))))

;; I hate tabs.
(setq-default indent-tabs-mode nil)

;; But Linus likes tabs, so monkey-patch the built-in "linux" style to
;; enable indent-tabs-mode.
(c-add-style "linux"
             (append (cdr (assoc "linux" c-style-alist))
                     '((indent-tabs-mode . t))))

;; A pretty good description of how most C/C++ code at Meraki is
;; formatted.
(c-add-style "meraki"
             '("stroustrup"
               (c-basic-offset . 4)
               (indent-tabs-mode . nil)))

(setq c-default-style (quote ((java-mode . "java") (awk-mode . "awk") (other . "meraki"))))

(if (version< emacs-version "30.1")
    (use-package editorconfig
      :demand t
      :init
      (editorconfig-mode))
  (editorconfig-mode))

;; Use eglot for LSP support.  eglot is built-in for Emacs 29+; on
;; older versions (e.g. the Emacs 27.1 on Ubuntu 22.04) :ensure t pulls
;; it from GNU ELPA.  On 29+ this just installs a possibly-newer ELPA
;; copy, which is harmless.
;; Language servers needed: clangd, gopls, pylsp, yaml-language-server, bash-language-server
(use-package eglot
  :hook ((python-mode python-ts-mode
          go-mode go-ts-mode
          c-mode c-ts-mode c++-mode c++-ts-mode
          yaml-mode yaml-ts-mode
          sh-mode bash-ts-mode) . eglot-ensure))

(use-package rustic
  :custom
  (rustic-lsp-client 'eglot))

(if (version< emacs-version "30.1")
    (use-package which-key
      :demand t
      :init
      (which-key-mode))
  (which-key-mode))

(use-package go-mode)
(use-package go-dlv
  :after go-mode)

(use-package yaml-mode)
(use-package systemd)

(use-package docker)
(use-package dockerfile-mode)
(when (version< emacs-version "29.0")
  (use-package docker-tramp
    :after tramp))
(use-package bitbake)
(use-package bazel)
(use-package cmake-mode)
(use-package meson-mode)
(use-package protobuf-mode)
(use-package jinja2-mode)
(use-package ssh-config-mode)
(use-package terraform-mode)
(use-package toml)
(use-package typescript-mode)

;;;; TRAMP
(use-package tramp
  :custom
  (remote-file-name-inhibit-auto-save-visited t)
  (make-backup-files nil)
  :config
  (add-to-list 'tramp-remote-path "~/.local/bin"))

(defun hostname (host)
  (car (split-string host "\\.")))

;;;; MARKDOWN
(use-package markdown-mode)

;; Corfu - modern completion UI that integrates with Vertico/Orderless
(use-package corfu
  :demand t
  :custom
  (corfu-auto t)
  (corfu-cycle t)
  :init
  (global-corfu-mode))

;; Corfu's popup uses child frames, which don't exist in a TTY.
;; corfu-terminal renders the popup with overlays instead.  Since this
;; is a (pseudo-)daemon setup that mixes GUI and TTY frames, decide
;; per-frame rather than once at startup.  This package won't be
;; necessary in Emacs 31.
(when (version< emacs-version "31.0")
  (use-package corfu-terminal
    :after corfu
    :config
    (defun my/corfu-terminal-set-up (&optional frame)
      "Enable `corfu-terminal-mode' only when FRAME is a TTY frame."
      (with-selected-frame (or frame (selected-frame))
        (corfu-terminal-mode (if (display-graphic-p) -1 1))))
    (add-hook 'server-after-make-frame-hook #'my/corfu-terminal-set-up)
    (my/corfu-terminal-set-up)))

(add-hook 'prog-mode-hook (lambda ()
                            (display-line-numbers-mode 1)
                            (setq show-trailing-whitespace t)))

(use-package dts-mode)
(use-package rainbow-delimiters)

(use-package nerd-icons
  :demand t
  :custom
  (nerd-icons-font-family "Symbols Nerd Font Mono"))

(require 'project)
(use-package treemacs
  :bind (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t d"   . treemacs-select-directory)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-tab-bar
  :after treemacs
  :demand t
  :config
  (treemacs-set-scope-type 'Tabs))
(use-package treemacs-nerd-icons
  :after (treemacs nerd-icons)
  :demand t
  :config
  (treemacs-nerd-icons-config))
(use-package treemacs-magit
  :after (treemacs magit)
  :demand t)
(use-package nerd-icons-dired
  :after (nerd-icons)
  :hook
  (dired-mode . nerd-icons-dired-mode))

;;;; VERTICO AND FRIENDS

;; 1. The UI: Replaces Ivy core
(use-package vertico
  :demand t
  :init
  (vertico-mode)
  :config
  ;; Make candidates clickable (vertico-mouse ships with vertico).
  (vertico-mouse-mode))

;; 2. The Search: Replaces Ivy fuzzy matching
(use-package orderless
  :demand t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; 3. The Commands: Replaces Swiper and Counsel
(use-package consult
  :bind (;; Replaces Swiper
         ("C-s" . consult-line)
         ;; Replaces counsel-switch-to-buffer
         ("C-x b" . consult-buffer)
         ;; Replaces counsel-yank-pop
         ("M-y" . consult-yank-pop)
         ;; Replaces counsel-rg / counsel-ag
         ("M-g g" . consult-ripgrep)))

;; 4. Rich Meta-Information (Optional but highly recommended)
(use-package marginalia
  :demand t
  :init
  (marginalia-mode))

(use-package embark
  :bind
  (("C-." . embark-act)         ;; Replaces M-o from Ivy
   ("M-." . embark-dwim)        ;; "Do What I Mean" contextual action
   ("C-h B" . embark-bindings)) ;; Alternative to major-mode help

  :init
  ;; Optionally replace the minibuffer help with Embark
  (setq prefix-help-command #'embark-prefix-help-command)

  :config
  ;; Hide the mode line of the Embark live-reporting buffer
  (add-to-list 'display-buffer-alist
               '("\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 (display-buffer-at-bottom)
                 (window-parameters (no-other-window . t))
                 (window-height . shrink-to-fit))))

;; Integrates Consult search buffers with Embark actions
(use-package embark-consult
  :ensure t
  :after (embark consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package embark-vc
  :after embark)

;;;; GIT-GUTTER
(use-package git-gutter)

;; (when (featurep 'git-gutter)
;;   (global-git-gutter-mode t))

;;;; OTHER STUFF
(use-package multiple-cursors
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)
         ("C-S-c C-S-c" . mc/edit-lines)))

;;;; SERVER

;; This allows a remote client to "phone home" to a local
;; emacs-server.  I haven't used this in many years.
;;
;;(require 'rclient)

;;;; UNIQUIFY
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;;;; FRAME AND WINDOW NAVIGATION

(windmove-default-keybindings 'meta)
(use-package ace-window
  :demand t
  :bind ("M-o" . ace-window)
  :custom
  ;; Jump to windows across all visible frames, replacing frame-tag.
  (aw-scope 'visible))

;;;; MISC
(if (version<= "28.0" emacs-version)
    (setopt use-short-answers t)
  (fset 'yes-or-no-p 'y-or-n-p))

(winner-mode 1)
(size-indication-mode 1)
(tool-bar-mode 0)
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode 0))
(show-paren-mode 1)
(column-number-mode 1)
(setq inhibit-startup-screen t)

(if (version<= "29.0" emacs-version)
    (pixel-scroll-precision-mode 1)
  (pixel-scroll-mode 1))

;; Since I use widescreen monitors everywhere, prefer
;; horizontal split to vertical split.
(setq split-height-threshold nil)
(setq split-width-threshold 120)

(setq hexl-bits 8)

(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
	     (next-win-buffer (window-buffer (next-window)))
	     (this-win-edges (window-edges (selected-window)))
	     (next-win-edges (window-edges (next-window)))
	     (this-win-2nd (not (and (<= (car this-win-edges)
					 (car next-win-edges))
				     (<= (cadr this-win-edges)
					 (cadr next-win-edges)))))
	     (splitter
	      (if (= (car this-win-edges)
		     (car (window-edges (next-window))))
		  'split-window-horizontally
		'split-window-vertically)))
	(delete-other-windows)
	(let ((first-win (selected-window)))
	  (funcall splitter)
	  (if this-win-2nd (other-window 1))
	  (set-window-buffer (selected-window) this-win-buffer)
	  (set-window-buffer (next-window) next-win-buffer)
	  (select-window first-win)
	  (if this-win-2nd (other-window 1))))))

(defun close-and-kill-next-pane ()
  "If there are multiple windows, then close the other pane and kill the buffer in it also."
  (interactive)
  (other-window 1)
  (kill-this-buffer)
  (if (not (one-window-p))
      (delete-window)))

(bind-key "C-x /" 'toggle-window-split)
(bind-key "C-c x" 'close-and-kill-next-pane)

;;;; SYSTEM-SPECIFIC SETUP

;; This check is built-in for Emacs 30
(when (and
       (eq system-type 'darwin)
       (version< emacs-version "30.1")
       (executable-find "gls"))
  (setq insert-directory-program "gls"))

;; For GUI windows, try fonts
(defun my/set-preferred-font (frame)
  "Set the default font for FRAME if it is a graphical display"
  (with-selected-frame frame
    (when (display-graphic-p)
      (let ((font-name
             (cond
              ((member "JetBrainsMono Nerd Font" (font-family-list)) "JetBrainsMono Nerd Font-12")
              ((member "JetBrains Mono" (font-family-list)) "JetBrains Mono-12")
              ((member "Menlo" (font-family-list)) "Menlo-12")
              ((member "Cascadia Code" (font-family-list)) "Cascadia Code-12")
              ((member "DejaVu Sans Mono" (font-family-list)) "DejaVu Sans Mono-12")
              (t "Monospace-12"))))
        (set-frame-font font-name nil t)))))

(add-hook 'after-init-hook (lambda () (my/set-preferred-font (selected-frame))))
(add-hook 'after-make-frame-functions #'my/set-preferred-font)

(when (memq window-system '(mac ns))
  (use-package exec-path-from-shell
    :demand t
    :config (exec-path-from-shell-initialize)))

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;;;; LOCAL SETUP
(load (expand-file-name "init-local.el" user-emacs-directory) 'noerror)
