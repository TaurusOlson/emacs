
(load-file "~/.emacs.d/resources/sensible-defaults.el")
(sensible-defaults/use-all-settings)
(sensible-defaults/use-all-keybindings)

(setq user-full-name "Taurus Olson"
      user-mail-address "taurusolson@gmail.com")

(setq confirm-kill-emacs nil)

(use-package exec-path-from-shell
  :config (exec-path-from-shell-initialize))

(def-find-file emacs-config-file)

(defun taurus/load-appropriate-theme (light-theme dark-theme)
  "Load the light theme in the morning and the dark theme at night"
  (interactive)
  (if (< (string-to-int (format-time-string "%H")) 19)
      (load-theme light-theme :no-confirm)
    (load-theme dark-theme :no-confirm)))

(defun taurus/update-current-date ()
  "Updates the current date in an org file"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (search-forward "#+DATE:" nil t)
    (kill-line)
    (insert (format " %s" (format-time-string "%Y-%m-%d")))))

(use-package evil
 :init
 (setq-default evil-escape-key-sequence "kj" evil-escape-delay 0.2)
 :config (evil-mode 1)

 :bind (:map evil-normal-state-map
             ;; Window movements
             ("C-k" . evil-window-up)
             ("C-j" . evil-window-down)
             ("C-l" . evil-window-right)
             ("C-h" . evil-window-left)
             ;; Emacs style
             ("C-a" . evil-beginning-of-line)
             ("C-e" . evil-end-of-line)
             ;; Save
             ("C-SPC" . save-buffer)
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

(use-package which-key
  :init (which-key-mode 1))

(setq ns-command-modifier 'meta
      select-enable-clipboard t)

(setq mac-option-modifier nil
      mac-command-modifier 'meta)

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

(setq org-agenda-custom-commands
      '(("d" "Daily Action List"
         ((agenda "" ((org-agenda-ndays 1)
                      (org-agenda-sorting-strategy '((agenda time-up priority-down tag-up)))
                      (org-deadline-warning-days 0)))))))

(add-hook 'after-init-hook (lambda () (org-agenda nil "d")))

;; Personal configuration
(defconst org-directory "~/Dropbox/olson")
(defconst olson-dir "~/Dropbox/olson")
(defconst olson-index-file (expand-file-name "index.org" olson-dir))
(defconst olson-diary-file (expand-file-name "diary.org" olson-dir))
(defconst org-agenda-files (list olson-index-file))
(setq org-archive-location "archives/%s_archive::")

;; Settings
(setq org-todo-keywords
      '((sequence "TODO(t)" "ACTIVE(a)" "SOMEDAY(s)" "DEFERRED(f)" "|" "CANCELLED(x)" "DONE(d)")))

;; Define the find-file functions for index and diary
(def-find-file olson-index-file)
(def-find-file olson-diary-file)

(setq org-confirm-babel-evaluate nil
      org-src-window-setup 'current-window)

(setq org-capture-templates
      '(("t" "add new task in index" entry (file+headline olson-index-file "Tasks") "* TODO  %?\n")
        ("d" "add new day in diary" entry (file+headline olson-diary-file "Diary") "* %t %?\n")
))

(require 'ob-clojure)
;; (org-babel-do-load-languages
;;  'org-babel-load-languages
;;  '((python . t)
;;    (R . t)))

(use-package org-bullets
    :init
    (setq org-bullets-bullet-list
      '("◉" "◎" "○" "○" "○" "○"))
    :config
    (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
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
;; (global-set-key (kbd "S-<return>") 'eval-last-sexp)

(use-package deft
  :config
  (setq deft-extension "org"
        deft-default-extension "org"
        deft-directory "~/Dropbox/olson/notes"
        deft-auto-save-interval 0
        deft-text-mode 'org-mode))

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

(use-package ess
  :commands R
  :config
  (progn
    (setq inferior-R-program-name "/usr/bin/R")
    (add-to-list 'auto-mode-alist '("\\.R$" . R-mode))
    (setq comint-input-ring-size 1000)
    ;; Eldoc
    (setq ess-use-eldoc 'script-only)
    (setq ess-eldoc-show-on-symbol t)
    (setq ess-eldoc-abbreviation-style t)
    ;; Indentation
    (setq ess-indent-level 4)
    (setq ess-arg-function-offset 4)
    (setq ess-else-offset 4)
    ;; R repl
    (add-hook 'inferior-ess-mode-hook
           '(lambda nil
              (define-key inferior-ess-mode-map [up]
                'comint-previous-matching-input-from-input)
              (define-key inferior-ess-mode-map [down]
                'comint-next-matching-input-from-input)
              (define-key inferior-ess-mode-map [\C-x \t]
                'comint-dynamic-complete-filename)))))

(use-package virtualenvwrapper
  :init (setq venv-location "~/.virtualenvs")
)

;;(use-package clojure-mode)
;;(use-package cider
;;  :init (use-package evil-leader :config (global-evil-leader-mode))
;;  :config
;;  (add-hook clojure-mode-hook
;;            (lambda () (evil-leader/set-key "x" 'cider-eval-last-sexp))))

(when window-system
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (tooltip-mode -1))

(set-frame-parameter nil 'fullscreen 'fullboth)

(setq hrs/default-font "Fira Code")
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
(hrs/set-font-size)

(blink-cursor-mode 0)
(column-number-mode 1)

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
  )

(use-package linum-relative
  :config
  (linum-relative-mode 1))

(use-package golden-ratio
  :disabled t
  :diminish golden-ratio-mode
  :init
  (golden-ratio-mode t)
  :config
  (add-to-list 'golden-ratio-extra-commands 'switch-window)
  (add-to-list 'golden-ratio-extra-commands 'evil-window-up)
  (add-to-list 'golden-ratio-extra-commands 'evil-window-down)
  (add-to-list 'golden-ratio-extra-commands 'evil-window-right)
  (add-to-list 'golden-ratio-extra-commands 'evil-window-left)
  (setq golden-ratio-exclude-modes '("magit-auto-revert-mode"
                                     "eshell-mode" "dired-mode")))

(use-package smooth-scrolling
 :init (setq smooth-scroll-margin 2)
 :config (smooth-scrolling-mode 1)
)

(use-package ivy
  :diminish (ivy-mode . "")
  :bind
  (:map ivy-mode-map ("M-b" . ivy-switch-buffer))
  :config
  (ivy-mode 1)
  ;; configure regexp engine.
  (setq ivy-re-builders-alist
        '((t . ivy--regex-fuzzy)))
    ;; number of result lines to display
    (setq ivy-height 10)
    ;; does not count candidates
    (setq ivy-count-format "")
    ;; no regexp by default
    (setq ivy-initial-inputs-alist nil))

(use-package counsel
  :bind
  (:map counsel-mode-map ("M-r" . counsel-find-file))
  :config
  (counsel-mode 1))

(use-package swiper)

;; (setq ido-enable-flex-matching t)
;; (setq ido-everywhere t)
;; (ido-mode 1)
;; (setq ido-create-new-buffer 'always)

;; (global-set-key (kbd "M-b") 'ido-switch-buffer)
;; (global-set-key (kbd "M-f") 'ido-find-file)

(use-package ido-ubiquitous
  :disabled t
  :config (ido-ubiquitous)
)

(use-package ido-vertical-mode
  :disabled t
  :init (setq ido-vertical-define-keys 'C-n-and-C-p-only)
  :config (ido-vertical-mode 1)
)

(use-package flx-ido
  :disabled t
  :config (flx-ido-mode 1))

(use-package smex
  :disabled t
  :config (smex-initialize)
  :bind (("M-x" . smex)))

(global-set-key (kbd "C-x C-b") 'ibuffer)

(use-package elisp-slime-nav
    :diminish elisp-slime-nav-mode
    :init (use-package evil-leader :config (global-evil-leader-mode))
    :config
    (evil-leader/set-key "t" 'elisp-slime-nav-find-elisp-thing-at-point)
    (evil-leader/set-key "h" 'elisp-slime-nav-describe-elisp-thing-at-point)
)

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

(use-package general :ensure t
  :config
  (general-define-key
   :states '(normal visual insert emacs)
   :prefix "SPC"
   :non-normal-prefix "C-SPC"

    ;; counsel
    "c" '(:ignore t :which-key "Counsel")
    "ca"   'counsel-ag

    ;; olson
    "o" '(:ignore t :which-key "Olson")
    "oi" 'find-olson-index-file
    "od" 'find-olson-diary-file

    ;; deft
    "d" '(:ignore t :which-key "Deft")
    "dq" 'bjm-quit-deft
    "do" 'deft
    "dn" 'deft-new-file-named
    "df" 'deft-find-file
))

(use-package key-chord :ensure t
  :defer 1 ; do not load right at startup
  :config
  (setq key-chord-two-keys-delay 0.2)
  ;; need to use key-seq. otherwise key order does not matter. that's bad.
  ;; i want latency only on x.
  (use-package key-seq :ensure t
    :config
    (key-seq-define evil-insert-state-map "qf" #'ivy-switch-buffer)
    (key-seq-define evil-insert-state-map "qv" #'git-gutter:stage-hunk)
    (key-seq-define evil-insert-state-map "qc" #'avy-goto-word-1)
    (key-seq-define evil-insert-state-map "ql" #'avy-goto-line)
    (key-seq-define evil-insert-state-map "qs" #'save-buffer)
    (key-seq-define evil-insert-state-map "qp" #'hydra-projectile/body)
    (key-seq-define evil-insert-state-map "QV" #'magit-status)))

(use-package zenburn-theme
 :disabled t
 :init (load-theme 'zenburn t))

(use-package solarized-theme
  :init
  (setq solarized-use-variable-pitch nil)
  (setq solarized-height-plus-1 1.0)
  (setq solarized-height-plus-2 1.0)
  (setq solarized-height-plus-3 1.0)
  (setq solarized-height-plus-4 1.0)
  (setq solarized-high-contrast-mode-line t)
  :config
  (taurus/load-appropriate-theme 'solarized-light 'solarized-dark))

(use-package color-theme-sanityinc-tomorrow
  :disabled t
  :config
  (taurus/load-appropriate-theme 'sanityinc-tomorrow-day
                                 sanityinc-tomorrow-night))

(use-package doom-themes
  :disabled t
  :init
  ;; brighter source buffers
  (add-hook 'find-file-hook 'doom-buffer-mode)
  ;; brighter minibuffer when active
  (add-hook 'minibuffer-setup-hook 'doom-brighten-minibuffer)
  :config
  (load-theme 'doom-one t))

(use-package sexy-monochrome-theme
  :disabled t
  :config (load-theme 'sexy-monochrome t))
