;; My Emacs file is an Org-mode file

;;; Set up package
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

;;; Bootstrap use-package
;; Install use-package if it's not already installed.
;; use-package is used to configure the rest of the packages.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package)
  (setq-default use-package-always-ensure t)
  (require 'diminish)
  (require 'bind-key))

(setq emacs-config-file "~/.emacs.d/taurusolson.org")
(org-babel-load-file emacs-config-file)
(custom-set-variables '(paradox-github-token t))
