;; My Emacs file is an Org-mode file

;;; Set up package
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package)
  (setq-default use-package-always-ensure t)
  (require 'diminish)
  (require 'bind-key))


;; Load custom files
(load-file (expand-file-name "defuns.el" user-emacs-directory))

(setq emacs-config-file "~/.emacs.d/taurusolson.org")
(org-babel-load-file emacs-config-file)
