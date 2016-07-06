
(load-file "~/.emacs.d/resources/sensible-defaults.el")
(sensible-defaults/use-all-settings)
(sensible-defaults/use-all-keybindings)

(setq user-full-name "Taurus Olson"
      user-mail-address "taurusolson@gmail.com")

(setq confirm-kill-emacs nil
      auto-save-default nil)

(defun find-emacs-config-file ()
  (interactive)
  (find-file emacs-config-file))

(use-package evil
 :config (evil-mode 1)
 :bind (:map evil-normal-state-map
             ;; Window movements
             ("C-k" . evil-window-up)
             ("C-j" . evil-window-down)
             ("C-l" . evil-window-right)
             ("C-h" . evil-window-left)
             ;; Save
             ;; ("C-SPC" . save-buffer)
             ;; Describe functions
             ("? k" . describe-key)
             ("? v" . describe-variable)
             ("? f" . describe-function)
             ("? m" . describe-mode)
             ("? p" . describe-package)
        :map evil-insert-state-map
             ("C-a" . beginning-of-line)
             ("C-e" . end-of-line)))

(use-package evil-surround
  :config
  (global-evil-surround-mode 1)
)

(use-package evil-search-highlight-persist
  :config
  (global-evil-search-highlight-persist t)
)

(use-package evil-leader
  :init
  (global-evil-leader-mode)
  :config
  (evil-leader/set-leader ",")
  (evil-leader/set-key "e" 'find-emacs-config-file)
  (evil-leader/set-key "b" 'ibuffer)
  (evil-leader/set-key "d" 'dired-jump))

(setq ns-command-modifier 'meta)

(setq mac-option-modifier nil
      mac-command-modifier 'meta
      x-select-enable-clipboard)

;avoid hiding with M-h
(setq mac-pass-command-to-system nil)

(setq org-agenda-skip-deadline-prewarning-if-scheduled t
     org-agenda-skip-scheduled-if-deadline-is-shown t
     org-agenda-todo-ignore-deadlines t
     org-agenda-todo-ignore-scheduled t
     org-agenda-window-setup 'current-window
     org-deadline-warning-days 7
     org-agenda-show-log t
     org-agenda-span 'fortnight)

;; Personal configuration
(setq org-directory "~/Dropbox/olson")
(setq olson-index-file "~/Dropbox/olson/index.org")
(setq org-agenda-files (list olson-index-file))
(setq org-archive-location "archives/%s_archive::")

;; Settings
(setq org-todo-keywords
      '((sequence "TODO(t)" "ACTIVE(a)" "SOMEDAY(s)" "DEFERRED(f)" "|" "CANCELLED(x)" "DONE(d)")))

(defun open-olson-organizer ()
  (interactive)
  (find-file olson-index-file))

(setq org-confirm-babel-evaluate nil)

(setq org-capture-templates
    '(("t" "todo" entry (file+headline olson-index-file "Tasks") "* TODO  %?\n")))

(require 'ob-clojure)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (R . t)))

(use-package org-bullets
    :init
    (setq org-bullets-bullet-list
      '("◉" "◎" "⚫" "○" "►" "◇"))
    :config
    (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
    (setq org-ellipsis "⤵")
    (setq org-hide-leading-stars t))

(setq org-log-done t
      org-log-into-drawer t)

(eval-after-load "org"
  '(require 'ox-md nil t))

(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)

(define-key global-map "\C-co" 'org-capture)
(global-set-key (kbd "C-c C-d") 'org-deadline)
(define-key mode-specific-map [?a] 'org-agenda)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-cL" 'org-insert-link-global)
(global-set-key (kbd "C-x /") 'open-olson-organizer)
;; (global-set-key (kbd "S-<return>") 'eval-last-sexp)

(use-package deft
:config
  (setq deft-extension "org")
  (setq deft-default-extension "org")
  (setq deft-directory "~/Dropbox/olson/notes")
  (setq deft-text-mode 'org-mode))

(setq deft-use-filename-as-title nil)
(setq deft-use-filter-string-for-filename t)
(setq deft-file-naming-rules '((noslash . "_")
                               (nospace . "_")
                               (case-fn . downcase)))
(setq deft-org-mode-title-prefix t)

;;advise deft to save window config
(defun bjm-deft-save-windows (orig-fun &rest args)
  (setq bjm-pre-deft-window-config (current-window-configuration))
  (apply orig-fun args)
  )

(advice-add 'deft :around #'bjm-deft-save-windows)

;function to quit a deft edit cleanly back to pre deft window
(defun bjm-quit-deft ()
  "Save buffer, kill buffer, kill deft buffer, and restore window config to the way it was before deft was invoked"
  (interactive)
  (save-buffer)
  (kill-this-buffer)
  (switch-to-buffer "*Deft*")
  (kill-this-buffer)
  (when (window-configuration-p bjm-pre-deft-window-config)
    (set-window-configuration bjm-pre-deft-window-config)
    )
  )

(global-set-key (kbd "C-c q") 'bjm-quit-deft)
(global-set-key (kbd "C-x n") 'deft)
(global-set-key (kbd "C-x N") 'deft-new-file-named)
(global-set-key (kbd "C-x C-g") 'deft-find-file)

(use-package magit
  :init (use-package evil :config (evil-mode 1))
  :config
  (define-key evil-normal-state-map (kbd "gs") 'magit-status)
  (define-key evil-normal-state-map (kbd "gv") 'magit-log-all)
  )

(setq-default indent-tabs-mode nil)

(use-package company
  :init (add-hook 'after-init-hook 'global-company-mode)
  :config (company-mode)
  )

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(defun paredit-kill-then-insert ()
  (interactive)
  (paredit-kill)
  (evil-insert 1))

(use-package paredit
 :init (use-package evil :config (evil-mode 1))
 :diminish paredit-mode
 :config
 (paredit-mode)
 (define-key evil-normal-state-map (kbd "D") 'paredit-kill)
 (define-key evil-normal-state-map (kbd "C") 'paredit-kill-then-insert)
)

(use-package undo-tree
  :diminish undo-tree-mode)

(use-package eldoc
  :diminish eldoc-mode)

(when window-system
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (tooltip-mode -1))

(setq hrs/default-font "Inconsolata")
(setq hrs/default-font-size 20)
(setq hrs/current-font-size hrs/default-font-size)
(setq hrs/font-change-increment 1.1)

(defun hrs/set-font-size ()
  "Set the font to `hrs/default-font' at `hrs/current-font-size'."
  (set-frame-font
   (concat hrs/default-font "-" (number-to-string hrs/current-font-size))))

(defun hrs/reset-font-size ()
  "Change font size back to `hrs/default-font-size'."
  (interactive)
  (setq hrs/current-font-size hrs/default-font-size)
  (hrs/set-font-size))

(defun hrs/increase-font-size ()
  "Increase current font size by a factor of `hrs/font-change-increment'."
  (interactive)
  (setq hrs/current-font-size
        (ceiling (* hrs/current-font-size hrs/font-change-increment)))
  (hrs/set-font-size))

(defun hrs/decrease-font-size ()
  "Decrease current font size by a factor of `hrs/font-change-increment', down to a minimum size of 1."
  (interactive)
  (setq hrs/current-font-size
        (max 1
             (floor (/ hrs/current-font-size hrs/font-change-increment))))
  (hrs/set-font-size))

(define-key global-map (kbd "C-)") 'hrs/reset-font-size)
(define-key global-map (kbd "C-+") 'hrs/increase-font-size)
(define-key global-map (kbd "C--") 'hrs/decrease-font-size)

(blink-cursor-mode 0)

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
  )

(use-package linum-relative
  :config
  (linum-relative-mode))

(use-package smooth-scrolling
 :init (setq smooth-scroll-margin 2)
 :config (smooth-scrolling-mode 1)
)

(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)
(setq ido-create-new-buffer 'always)

(global-set-key (kbd "M-b") 'ido-switch-buffer)
(global-set-key (kbd "M-f") 'ido-find-file)

(use-package ido-ubiquitous
  :config (ido-ubiquitous)
)

(use-package ido-vertical-mode
  :init (setq ido-vertical-define-keys 'C-n-and-C-p-only)
  :config (ido-vertical-mode 1)
)

(use-package flx-ido
  :config (flx-ido-mode 1))

(use-package smex
  :config (smex-initialize)
  :bind (("M-x" . smex)))

(global-set-key (kbd "C-x C-b") 'ibuffer)

(use-package elisp-slime-nav
  :diminish elisp-slime-nav-mode)

(defun configure-lispy-mode-hooks ()
    ;; (setq show-paren-style 'expression)
    (turn-on-eldoc-mode)
    (paredit-mode)
    (rainbow-delimiters-mode)
    (elisp-slime-nav-mode))

(setq lispy-mode-hooks
    '(clojure-mode-hook
      emacs-lisp-mode-hook
      lisp-mode-hook
      scheme-mode-hook))

(dolist (hook lispy-mode-hooks)
    (add-hook hook 'configure-lispy-mode-hooks))

(evil-define-key 'normal emacs-lisp-mode-map (kbd "K") 'elisp-slime-nav-describe-elisp-thing-at-point)

(use-package zenburn-theme
 :disabled t
 :init (load-theme 'zenburn t))

(use-package solarized-theme
  :disabled t
  :init
  (setq solarized-use-variable-pitch nil)
  (setq solarized-height-plus-1 1.0)
  (setq solarized-height-plus-2 1.0)
  (setq solarized-height-plus-3 1.0)
  (setq solarized-height-plus-4 1.0)
  (setq solarized-high-contrast-mode-line t)
  :config
  (load-theme 'solarized-dark t))

(use-package color-theme-sanityinc-tomorrow
  :config (color-theme-sanityinc-tomorrow-night))
