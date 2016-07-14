;;; defuns.el


;; My first macro
(defmacro def-find-file (my-funcname)
  `(defun ,(intern (concat "find-" (symbol-name my-funcname))) ()
                  (interactive)
                  (find-file ,my-funcname)))


;; Experimental
;; (defun exec (program)
;;   (shell-command (format "open -a %s" program)))

;; (defalias (intern (concat "exec-" name))
;;    `(lambda () ,(format "Run %s via `exec'." name) (interactive) (exec ,name)))



(provide 'defuns)
;;; defuns.el ends here
