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
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(iswitchb-mode t)
 '(package-selected-packages
   (quote
    (evil-escape key-seq wgrep ranger which-key general basic-theme white-theme sexy-monochrome sexy-monochrome-theme doom-themes counsel zenburn-theme worf virtualenvwrapper use-package solarized-theme smooth-scrolling smex smart-mode-line rainbow-delimiters paredit org-bullets org markdown-mode magit linum-relative ido-vertical-mode ido-ubiquitous golden-ratio flx-ido exec-path-from-shell evil-surround evil-search-highlight-persist evil-leader ess elisp-slime-nav deft cyberpunk-theme company color-theme-sanityinc-tomorrow cider))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
