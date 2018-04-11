;;; package --- g4z3's custom packages

;;; Commentary:

;;; Code:

(prelude-require-package 'neotree)
(prelude-require-package 'virtualenvwrapper)
(prelude-require-package 'company-c-headers)
(prelude-require-package 'simpleclip)

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
;; backwards compatibility as default-buffer-file-coding-system
;; is deprecated in 23.2.
(if (boundp 'buffer-file-coding-system)
    (setq-default buffer-file-coding-system 'utf-8)
  (setq default-buffer-file-coding-system 'utf-8))

;; Treat clipboard input as UTF-8 string first; compound text next, etc.
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

(define-key global-map "\C-cc" 'org-capture)

(setq projectile-switch-project-action 'neotree-projectile-action)

(global-set-key (kbd "M-x") 'execute-extended-command)

(provide 'g4z3)
;;; g4z3.el ends here
