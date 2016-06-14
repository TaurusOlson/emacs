
(load-file "~/.emacs.d/resources/sensible-defaults.el")
(sensible-defaults/use-all-settings)
(sensible-defaults/use-all-keybindings)

(setq user-full-name "Taurus Olson"
      user-mail-address "taurusolson@gmail.com")

(setq exec-path (append exec-path '("/usr/local/bin")))

(defun open-config-file ()
  (interactive)
  (find-file "~/.emacs.d/taurusolson.org"))

;; Rename current buffer and file (magnars)
(defun rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))

(global-set-key (kbd "C-x ,") 'open-config-file)
(global-set-key (kbd "C-x C-r") 'rename-current-buffer-file)

(setq ns-command-modifier 'meta)

(setq mac-option-modifier nil
      mac-command-modifier 'meta
      x-select-enable-clipboard)

;avoid hiding with M-h
(setq mac-pass-command-to-system nil)

(require 'cask "~/.cask/cask.el")
(cask-initialize)
(require 'pallet)

(require 'evil)
(evil-mode 1)

(define-key evil-insert-state-map (kbd "C-a") 'beginning-of-line)
(define-key evil-insert-state-map (kbd "C-e") 'end-of-line)
(define-key evil-normal-state-map (kbd "C-e") 'end-of-line)
(define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
(define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
(define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)
(define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)

(define-key evil-normal-state-map (kbd "? k") 'describe-key)
(define-key evil-normal-state-map (kbd "? v") 'describe-variable)
(define-key evil-normal-state-map (kbd "? f") 'describe-function)
(define-key evil-normal-state-map (kbd "? m") 'describe-mode)
(define-key evil-normal-state-map (kbd "? p") 'describe-package)

(require 'paredit)

(when (require 'evil nil 'noerror)
  (define-key evil-insert-state-map (kbd "C-k") 'paredit-kill)
)

(defun hrs/mac? ()
  "Returns `t' if this is an Apple machine, nil otherwise."
  (eq system-type 'darwin))

(defun hrs/split-window-below-and-switch ()
  "Split the window horizontally, then switch to the new pane."
  (interactive)
  (split-window-below)
  (other-window 1))

(defun hrs/split-window-right-and-switch ()
  "Split the window vertically, then switch to the new pane."
  (interactive)
  (split-window-right)
  (other-window 1))

(setq lispy-mode-hooks
      '(clojure-mode-hook
        emacs-lisp-mode-hook
        lisp-mode-hook
        scheme-mode-hook
        org-mode-hook))

(dolist (hook lispy-mode-hooks)
  (add-hook hook (lambda ()
                   ;; (setq show-paren-style 'expression)
                   (paredit-mode)
                   (rainbow-delimiters-mode))))

(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)

(require 'elisp-slime-nav)
  (dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook org-mode-hook))
    (add-hook hook 'elisp-slime-nav-mode))

(define-key evil-normal-state-map (kbd ", t") 'elisp-slime-nav-find-elisp-thing-at-point)
(define-key evil-normal-state-map (kbd "C-t") 'pop-tag-mark)

(setq python-indent 4)

(require 'elpy)
(elpy-enable)
(elpy-use-ipython)

(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

(require 'py-autopep8)
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)

(require 'pony-mode)

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
    ;; (load-theme 'solarized-dark t)
    (load-theme 'zenburn t)
    ;; (load-theme 'sanityinc-tomorrow-day t)
)

(require 'ido)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)
(ido-ubiquitous)
(flx-ido-mode 1) ; better/faster matching
(setq ido-create-new-buffer 'always) ; don't confirm to create new buffers
(ido-vertical-mode 1)
(setq ido-vertical-define-keys 'C-n-and-C-p-only)

(global-set-key (kbd "C-x b") 'ido-switch-buffer)

(smex-initialize)

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

(setq hrs/default-font "Inconsolata")
(setq hrs/default-font-size 16)
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
;; (define-key global-map (kbd "C-=") 'hrs/increase-font-size)
;; (define-key global-map (kbd "C-_") 'hrs/decrease-font-size)
(define-key global-map (kbd "C--") 'hrs/decrease-font-size)

(setq org-src-fontify-natively t)

;; (when window-system (global-hl-line-mode))

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

(diminish-minor-mode 'elisp-slime-nav 'elisp-slime-nav-mode)
(diminish-major-mode 'emacs-lisp-mode-hook "el")
(diminish-major-mode 'lisp-interaction-mode-hook "λ")
(diminish-major-mode 'python-mode-hook "Py")

(tool-bar-mode 0)
(menu-bar-mode 0)
(when window-system
  (scroll-bar-mode -1))

(blink-cursor-mode 0)

(highlight-indentation-mode nil)

(require 'diff-hl)
(global-diff-hl-mode)

(global-prettify-symbols-mode t)

(global-set-key (kbd "C-x C-b") 'ibuffer)

  (add-hook 'ibuffer-hook
    (lambda ()
      (ibuffer-vc-set-filter-groups-by-vc-root)
      (unless (eq ibuffer-sorting-mode 'alphabetic)
        (ibuffer-do-sort-by-alphabetic))))

(setq ibuffer-formats
  '((mark modified read-only vc-status-mini " "
          (name 18 18 :left :elide)
          " "
          (size 9 -1 :right)
          " "
          (mode 16 16 :left :elide)
          " "
          (vc-status 16 16 :left)
          " "
          filename-and-process)))

(global-set-key (kbd "C-x 2") 'hrs/split-window-below-and-switch)
(global-set-key (kbd "C-x 3") 'hrs/split-window-right-and-switch)

(require 'swiper)
(require 'ivy)
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq ivy-height 10)
(setq ivy-count-format "(%d/%d) ")
(setq ivy-re-builders-alist
    '((t . ivy--regex-fuzzy)))

(global-set-key (kbd "C-s") 'swiper)
;;  (global-set-key (kbd "M-x") 'counsel-M-x)
;;  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
;;  (global-set-key (kbd "<f1> f") 'counsel-describe-function)

(global-set-key (kbd "C-x g") 'magit-status)

(require 'org)
(setq org-agenda-skip-deadline-prewarning-if-scheduled t
     org-agenda-skip-scheduled-if-deadline-is-shown t
     org-agenda-todo-ignore-deadlines t
     org-agenda-todo-ignore-scheduled t
     org-agenda-window-setup 'current-window
     org-deadline-warning-days 7
     org-agenda-show-log t
     org-agenda-span 'fortnight)

;; Personal configuration
(setq olson-index-file "~/Dropbox/olson/index.org")
(setq org-agenda-files (list olson-index-file))

;; Bindings
(defun open-olson-organizer ()
  (interactive)
  (find-file olson-index-file))

(global-set-key (kbd "C-x /") 'open-olson-organizer)

;; Settings
(setq org-todo-keywords
      '("TODO" "ACTIVE" "DEFERRED" "CANCELLED" "DONE"))

(setq org-archive-location "archives/%s_archive::")

;; Capture
(setq org-capture-templates
    '(("t" "todo" entry (file+headline olson-index-file "Tasks") "* TODO  %?\n")))

;; Keybindings

(define-key global-map "\C-co" 'org-capture)
(define-key mode-specific-map [?a] 'org-agenda)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-cL" 'org-insert-link-global)

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

(setq org-ellipsis "⤵")
(setq org-hide-leading-stars t)

(setq org-log-done t)

(eval-after-load "org"
  '(require 'ox-md nil t))

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

(defun hrs/search-project-for-symbol-at-point ()
  "Use `projectile-ag' to search the current project for `symbol-at-point'."
  (interactive)
  (projectile-ag (projectile-symbol-at-point)))

(global-set-key (kbd "C-c v") 'projectile-ag)

(require 'perspective)
(persp-mode)

(when (require 'evil nil 'noerror)
  (define-key evil-normal-state-map (kbd "M-r") 'projectile-find-file)
  (define-key evil-normal-state-map (kbd "M-b") 'projectile-switch-to-buffer)
  (define-key evil-normal-state-map (kbd "M-p") 'projectile-persp-switch-project)
  (define-key evil-normal-state-map (kbd "M-u") 'projectile-find-file-in-known-projects)
  (define-key evil-normal-state-map (kbd "M-g") 'projectile-find-tag))
