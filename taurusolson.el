
(load-file "~/.emacs.d/resources/sensible-defaults.el")
(sensible-defaults/use-all-settings)
(sensible-defaults/use-all-keybindings)

(setq user-full-name "Taurus Olson"
      user-mail-address "taurusolson@gmail.com")

(setq debug-on-error t)

(require 'cask "~/.cask/cask.el")
(cask-initialize)
(require 'pallet)

(evil-mode 1)

(define-key evil-normal-state-map (kbd "C-p") 'projectile-find-file)

(defun hrs/mac? ()
  "Returns `t' if this is an Apple machine, nil otherwise."
  (eq system-type 'darwin))

(setq lispy-mode-hooks
      '(clojure-mode-hook
        emacs-lisp-mode-hook
        lisp-mode-hook
        scheme-mode-hook))

(dolist (hook lispy-mode-hooks)
  (add-hook hook (lambda ()
                   (setq show-paren-style 'expression)
                   (paredit-mode)
                   (rainbow-delimiters-mode))))

(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)

(defun hrs/search-project-for-symbol-at-point ()
  "Use `projectile-ag' to search the current project for `symbol-at-point'."
  (interactive)
  (projectile-ag (projectile-symbol-at-point)))

(global-set-key (kbd "C-c v") 'projectile-ag)

(setq python-indent 4)

(require 'org)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(setq org-export-html-style-include-scripts nil
      org-export-html-style-include-default nil)

;; Hooks
(add-hook 'remember-mode-hook 'org-remember-apply-template)

;; Personal configuration

(setq olson-goals-file "~/Dropbox/olson/goals.org")
(setq olson-projects-file "~/Dropbox/olson/projects.org")
(setq org-agenda-files (list olson-goals-file olson-projects-file))

;; Bindings
(defun open-olson-organizer ()
  (interactive)
  (find-file olson-goals-file))

(global-set-key (kbd "C-x /") 'open-olson-organizer)

;; Settings
(setq org-todo-keywords
      '("TODO" "ACTIVE" "DEFERRED" "CANCELLED" "DONE"))

(setq org-archive-location "archives/%s_archive::")

;; Keybindings

(define-key global-map "\C-co" 'org-capture)
(define-key mode-specific-map [?a] 'org-agenda)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-cL" 'org-insert-link-global)

(custom-set-faces
 '(org-column ((t (:strike-through nil
                   :underline nil :slant normal :weight normal
                   :height 120 :family "Monaco")))))

;; Calendar
(when (file-exists-p "~/Dropbox/diary")
(setq diary-file "~/Dropbox/diary"))

;; Babel
(require 'ob-clojure)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (R . t)))

(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(add-hook 'after-init-hook 'global-company-mode)

(setq-default indent-tabs-mode nil)

;; (require 'yasnippet)
;; (setq yas-snippet-dirs '("~/.emacs.d/snippets/text-mode"))
;; (yas-global-mode 1)

(setq yas/indent-line nil)

(projectile-global-mode)

(when window-system
  (setq solarized-use-variable-pitch nil)
  (setq solarized-height-plus-1 1.0)
  (setq solarized-height-plus-2 1.0)
  (setq solarized-height-plus-3 1.0)
  (setq solarized-height-plus-4 1.0)
  (setq solarized-high-contrast-mode-line t)
  (load-theme 'solarized-dark t))

;; (setq ido-enable-flex-matching t)
;; (setq ido-everywhere t)
;; (ido-mode 1)
;; (ido-ubiquitous)
;; (flx-ido-mode 1) ; better/faster matching
;; (setq ido-create-new-buffer 'always) ; don't confirm to create new buffers
;; (ido-vertical-mode 1)
;; (setq ido-vertical-define-keys 'C-n-and-C-p-only)

(smex-initialize)

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

(setq hrs/default-font "Inconsolata")
(setq hrs/default-font-size 18)
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
(define-key global-map (kbd "C-=") 'hrs/increase-font-size)
(define-key global-map (kbd "C-_") 'hrs/decrease-font-size)
(define-key global-map (kbd "C--") 'hrs/decrease-font-size)

(setq org-src-fontify-natively t)

(when window-system
  (global-hl-line-mode))

(set-frame-parameter nil 'fullscreen 'fullboth)

(defmacro diminish-minor-mode (filename mode &optional abbrev)
  `(eval-after-load (symbol-name ,filename)
     '(diminish ,mode ,abbrev)))

(defmacro diminish-major-mode (mode-hook abbrev)
  `(add-hook ,mode-hook
             (lambda () (setq mode-name ,abbrev))))

(diminish-minor-mode 'abbrev 'abbrev-mode)
(diminish-minor-mode 'company 'company-mode)
(diminish-minor-mode 'eldoc 'eldoc-mode)
(diminish-minor-mode 'flycheck 'flycheck-mode)
(diminish-minor-mode 'flyspell 'flyspell-mode)
(diminish-minor-mode 'global-whitespace 'global-whitespace-mode)
(diminish-minor-mode 'projectile 'projectile-mode)
(diminish-minor-mode 'subword 'subword-mode)
(diminish-minor-mode 'undo-tree 'undo-tree-mode)
;; (diminish-minor-mode 'yasnippet 'yas-minor-mode)
(diminish-minor-mode 'wrap-region 'wrap-region-mode)

(diminish-minor-mode 'paredit 'paredit-mode " π")

(diminish-major-mode 'emacs-lisp-mode-hook "el")
(diminish-major-mode 'lisp-interaction-mode-hook "λ")
(diminish-major-mode 'python-mode-hook "Py")

(setq ns-command-modifier 'meta)

(setq mac-option-modifier nil
      mac-command-modifier 'meta
      x-select-enable-clipboard)

;avoid hiding with M-h
(setq mac-pass-command-to-system nil)

(setq deft-extension "org")
(setq deft-default-extension "org")
(setq deft-directory "~/Documents/org")
(setq deft-text-mode 'org-mode)

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

(require 'swiper)
(require 'ivy)
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq ivy-height 10)
(setq ivy-count-format "(%d/%d) ")
(setq ivy-re-builders-alist
    '((t . ivy--regex-fuzzy)))

(global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)