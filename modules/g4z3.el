;; g4z3's custom packages

;;; Code:

(prelude-require-package 'neotree)
(prelude-require-package 'virtualenvwrapper)
(prelude-require-package 'company-c-headers)

(setq projectile-switch-project-action 'neotree-projectile-action)
(global-set-key (kbd "M-x") 'execute-extended-command)

(provide 'g4z3)
;;; g4z3.el ends here
