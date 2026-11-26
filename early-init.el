;; -*- lexical-binding: t; -*-

;; Fix for native comp on MacOS with Emacs.app
(when (eq system-type 'darwin)
  (setenv "MACOSX_DEPLOYMENT_TARGET"
          (car (split-string operating-system-release "\\.")))
  (setq comp-async-report-warnings-errors nil)
  (setq native-comp-async-report-warnings-errors nil)
  (setq native-comp-async-jobs-number 1))
