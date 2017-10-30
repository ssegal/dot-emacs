;; Prefer newer files.  This is first so that it affects all
;; subsequent loads.
(setq load-prefer-newer t)

;; Add a local path for non-package.el themes.
(when (boundp 'custom-theme-load-path)
  (add-to-list 'custom-theme-load-path (expand-file-name "themes"  user-emacs-directory)))

;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)

;; Move customizer stuff to another file so it doesn't clog up this
;; one.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

;; Backups, go hide!
(defconst my/backup-directory (expand-file-name "backups" user-emacs-directory))
(unless (file-directory-p my/backup-directory)
  (make-directory my/backup-directory))
(setq backup-directory-alist `((".*" . ,(expand-file-name "backups" user-emacs-directory))))

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
(add-to-list 'load-path (expand-file-name "osx-pseudo-daemon" my/lisp-directory))

;; package.el
(require 'package)
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("melpa" . "https://melpa.org/packages/"))
      package-archive-priorities
      '(("melpa-stable" . 10)
        ("gnu" . 5)
        ("melpa" . 0)))

;; Initialize package.el here instead of at startup so we can have
;; use-package automatically install things.
(setq package-enable-at-startup nil)
(package-initialize)

;; Make sure use-package is installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Initialize use-package.  This init code is from the use-package
;; docs.
(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

;; Have use-package default to downloading the package if it doesn't
;; exist.  This can still be overridden on a package-specific basis
;; with ":ensure nil"
(setq use-package-always-ensure t)

;; Use ibuffer
(bind-key "C-x C-b" 'ibuffer)

;;;; EXTRA KEYMAPPINGS (default)
(when (eq system-type 'darwin)
  (bind-key [home] 'beginning-of-line)
  (bind-key [end] 'end-of-line)
  (bind-key "M-RET" 'toggle-frame-fullscreen))

(cua-mode 1)
(bind-key "C-c d" 'vc-git-grep)
(bind-key "C-c f" 'grep)
(bind-key "C-c C-f" 'imenu)
(bind-key "C-x r q" 'save-buffers-kill-emacs)
(bind-key "C-c r" 'revert-buffer)

;; recentf-mode
(use-package recentf
  :demand t
  :init
  (setq recentf-exclude '("/elpa/" '".recentf" '"COMMIT_EDITMSG"))
  :config
  (recentf-mode 1)
  :bind ("C-x C-r" . recentf-open-files))

;; Set theme
(use-package color-theme-sanityinc-tomorrow
  :demand t
  :init (load-theme 'sanityinc-tomorrow-night t))

;; xterm mouse reporting
(xterm-mouse-mode 1)

;; Delete selection with backspace or delete.
(delete-selection-mode 1)

(transient-mark-mode 1)

;; Default to UTF-8
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;;;; GIT STUFF


(use-package with-editor
  :config
  (add-hook 'shell-mode-hook  'with-editor-export-editor)
  (add-hook 'term-exec-hook   'with-editor-export-editor)
  (add-hook 'eshell-mode-hook 'with-editor-export-editor))

(use-package magit-autoloads
  :ensure magit
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch-popup))
  :init
  (setq magit-last-seen-setup-instructions "1.4.0")
  (setq vc-handled-backends (delq 'Git vc-handled-backends)))

(use-package git-commit)
(use-package gitignore-mode)
(use-package gitconfig-mode)

;;;; PROGRAMMING

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

(use-package web-mode
  :commands web-mode
  :mode (("\\.rhtml\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("\\.html?\\'" . web-mode))
  :config
  (setq web-mode-engines-alist
        '(("erb" . "\\.rhtml\\'")
          ("erb" . "\\.erb\\'"))))

(use-package enh-ruby-mode
  :ensure enh-ruby-mode
  :commands enh-ruby-mode
  :interpreter "ruby"
  :mode (("\\.rb$" . enh-ruby-mode)
         ("\\.rake$" . enh-ruby-mode)
         ("Rakefile$" . enh-ruby-mode)
         ("\\.gemspec$" . enh-ruby-mode)
         ("\\.ru$" . enh-ruby-mode)
         ("Gemfile$" . enh-ruby-mode)))

(use-package inf-ruby
  :commands run-ruby)

(use-package yaml-mode
  :commands yaml-mode
  :mode ("\\.yml\\'" . yaml-mode))

(use-package scala-mode
  :commands scala-mode
  :mode ("\\.scala\\'" . scala-mode))

(use-package protobuf-mode
  :commands protobuf-mode
  :mode ("\\.proto\\'" . protobuf-mode))

(use-package js2-mode
  :commands js2-mode
  :mode "\\.js\\'"
  :init (setq-default js2-basic-offset 2))

(use-package click-mode
  :commands click-mode
  :mode ("\\.template\\'" "\\.click\\'"))

(use-package function-args
  :commands function-args-mode)

(use-package elpy
  :config (elpy-enable))

(use-package rust-mode
  :mode "\\.rs\\'")

(use-package toml-mode
  :mode "\\.toml\\'")

(use-package cargo
  :commands cargo-minor-mode
  :init (add-hook 'rust-mode-hook 'cargo-minor-mode))

(use-package git-timemachine
  :if (version< "24.4" emacs-version)
  :commands git-timemachine)

;;;; TRAMP
(use-package tramp
  :config
  (setq tramp-default-method "scp")
  (add-to-list 'tramp-remote-path "/home/ssegal/bin"))

(defun hostname (host)
  (car (split-string host "\\.")))

;;;; ORG-MODE

(use-package org
  :init (setq org-replace-disputed-keys t)
  :config
  (setq org-log-done t)
  (setq org-agenda-files (list "~/Dropbox/org"))
  (setq org-time-stamp-custom-formats '("<%m/%d/%Y %a>" . "<%m/%d/%Y %a %I:%M %p>"))
  (setq-default org-display-custom-times t)
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)))

;;;; GTAGS
;;(use-package ggtags
;;  :ensure ggtags
;;  :commands ggtags-mode
;;  :init (add-hook 'c-mode-common-hook (lambda () (ggtags-mode 1))))

;;;; MARKDOWN
(use-package markdown-mode
  :commands markdown-mode)

(use-package company
  :demand t
  :config
  (global-company-mode 1)
  :init (setq company-tooltip-align-annotations t))

(use-package company-web
  :if (featurep 'company)
  :commands company-web-html
  :init (add-to-list 'company-backends 'company-web-html))

(use-package robe
  :commands robe-mode
  :init
  (add-hook 'enh-ruby-mode-hook 'robe-mode)
  (when (featurep 'company)
    (add-to-list 'company-backends 'company-robe)))

(use-package racer
  :commands racer-mode
  :if (and (fboundp 'rust-mode) (featurep 'company))
  :init
  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'racer-mode-hool #'eldoc-mode)
  :bind (:map rust-mode-map ("TAB" . company-indent-or-complete-common)))  

(add-hook 'prog-mode-hook (lambda ()
                            (if (version< emacs-version "26.0")
                                (linum-mode 1)
                              (display-line-numbers-mode 1))
                            (setq show-trailing-whitespace t)))

(use-package dts-mode
  :pin melpa)

(use-package rainbow-delimiters)

;;;; PROJECTILE
(use-package projectile
  :demand t
  :init
  (setq projectile-use-git-grep t)
  (setq projectile-enable-caching t)
  :config (projectile-global-mode 1))

;;;; IVY
(use-package ivy
  :init
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  :config
  (ivy-mode 1))

(use-package swiper
  :commands swiper
  :if (featurep 'ivy)
  :bind ("C-s" . swiper))

(use-package counsel
  :if (featurep 'ivy)
  :demand t
  :config
  (counsel-mode 1)
  :bind
  (("C-c f" . counsel-grep)
   ("C-c j" . counsel-git-grep)
   ("C-c k" . counsel-ag)))

(use-package counsel-projectile
  :if (and (featurep 'projectile) (featurep 'counsel))
  :config
  (setq projectile-completion-system 'ivy)
  (counsel-projectile-on))

;;;; GIT-GUTTER
(use-package git-gutter
  :commands git-gutter-mode
  :config
  (when (version< emacs-version "26.0")
    (git-gutter:linum-setup)))

;; (when (featurep 'git-gutter)
;;   (global-git-gutter-mode t))

;;;; OTHER STUFF
(use-package multiple-cursors
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)
         ("C-S-c C-S-c" . mc/edit-lines)))

;;;; SERVER
(require 'rclient)

;;;; LINUM MODE
(when (version<= emacs-version "26.0")
  (defvar my/linum-format-fmt)
  (add-hook 'linum-before-numbering-hook
            (lambda ()
              (setq-local my/linum-format-fmt
                          (let ((w (length (number-to-string
			                    (count-lines (point-min) (point-max))))))
	                    (concat "%" (number-to-string w) "d")))
              (setq-local my/linum-format 'linum-format-func)))

  (defun linum-format-func (line)
    (if (display-graphic-p nil)
        (propertize (format my/linum-format-fmt line) 'face 'linum)
      (concat
       (propertize (format my/linum-format-fmt line) 'face 'linum)
       (propertize " " 'face 'fringe))))

  (setq linum-format 'linum-format-func))

;;;; UNIQUIFY
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;;;; FRAME AND WINDOW NAVIGATION

(windmove-default-keybindings 'meta)
(use-package frame-tag
  :config (frame-tag-mode 1))

(use-package framemove
  :init (setq framemove-hook-into-windmove t))

;;;; MISC
(fset 'yes-or-no-p 'y-or-n-p)

(winner-mode 1)
(size-indication-mode 1)
(tool-bar-mode 0)
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode 0))
(show-paren-mode 1)
(column-number-mode 1)
(setq inhibit-startup-screen t)
(when (and (file-directory-p "/usr/local/bin") (not (member "/usr/local/bin" exec-path)))
  (add-to-list 'exec-path "/usr/local/bin"))

(when (version<= "26.0" emacs-version)
  (pixel-scroll-mode 1))

;; Since I use widescreen monitors everywhere, prefer
;; horizontal split to vertical split.
(setq split-height-threshold nil)
(setq split-width-threshold 120)

(setq hexl-bits 8)

;; Force bash as shell (overriding any local path)
;(setq shell-file-name "bash")

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
(cond ((eq system-type 'darwin)
       (when (executable-find "gls")
         (setq insert-directory-program "gls"))
       (when (find-font (font-spec :name "Menlo"))
         (set-face-attribute 'default nil :font "Menlo-12")))
      ((eq system-type 'windows-nt)
       (when (find-font (font-spec :name "Consolas"))
         (set-face-attribute 'default nil :font  "Consolas-10"))))

(use-package osx-clipboard
  :if (eq system-type 'darwin))

(use-package mac-pseudo-daemon
  :if (eq system-type 'darwin)
  :pin melpa)

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;;;; LOCAL SETUP
(load (expand-file-name "init-local.el" user-emacs-directory) 'noerror)

