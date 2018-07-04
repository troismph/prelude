;;; package --- g4z3's custom packages

;;; Commentary:

;;; Code:

(prelude-require-package 'neotree)
(prelude-require-package 'virtualenvwrapper)
(prelude-require-package 'company-c-headers)
(prelude-require-package 'simpleclip)
(prelude-require-package 'project-explorer)
(prelude-require-package 'workgroups2)
(prelude-require-package 'ob-typescript)
(prelude-require-package 'exec-path-from-shell)
(prelude-require-package 'tide)
(prelude-require-package 'htmlize)
(prelude-require-package 'bshell)
(load "bshell")
(prelude-require-package 'ssh)

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
(define-key global-map "\M-{" 'shrink-window-horizontally)
(define-key global-map "\M-}" 'enlarge-window-horizontally)
(define-key global-map "\M-[" 'shrink-window)
(define-key global-map "\M-]" 'enlarge-window)
(define-key global-map "\C-xf" 'other-frame)

(setq org-icalendar-timezone "Asia/Shanghai")
(setq org-log-done 'time)
(setq org-todo-keywords
      '((sequence "TODO(t@/!)" "PROGRESS(p@/!)" "BLOCKED(b@/!)" "|" "DONE(d@/!)" "CANCELED(c@/!)")))

(setq org-refile-targets (quote (("~/src/notes/tracker.org" :maxlevel . 3)
                                 ("~/src/notes/personal.org" :maxlevel . 3)
                                 ("~/src/notes/journal.org" :maxlevel . 3)
                                 )))
(setq org-refile-use-outline-path 'file)
(setq org-outline-path-complete-in-steps nil)
(setq org-lowest-priority 68)

(workgroups-mode 1)

;; copied from another el

(global-linum-mode 1)
(setq linum-format "%d ")

(custom-set-variables
 '(company-c-headers-path-system
  (quote
   ("/usr/include/" "/usr/local/include/" "/usr/include/c++/7/")))
 '(company-clang-arguments (quote ("-I/usr/include/c++/7" "-I/usr/include/")))
)

(add-to-list 'company-backends 'company-c-headers)
(global-set-key [home] 'move-beginning-of-line)
(global-set-key [select] 'move-end-of-line)

;; pytho virtual env settings
(venv-initialize-interactive-shells) ;; if you want interactive shell support
(venv-initialize-eshell) ;; if you want eshell support
;; note that setting `venv-location` is not necessary if you
;; use the default location (`~/.virtualenvs`), or if the
;; the environment variable `WORKON_HOME` points to the right place

;; end of copied section


(setenv "NODE_PATH"
  (concat
   "/usr/lib/node_modules"  ":"
   (concat (getenv "HOME") "/node_modules") ":"
    (getenv "NODE_PATH")
  )
)

; (setq org-src-fontify-natively t)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((typescript . t)
   )
 )

(setq org-src-window-setup (quote current-window))

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(setq python-shell-interpreter "python3")

(defun eshell-spawn(bn)
  "Open a new instance of eshell."
  (interactive "seshell name:")
  (message bn)
  (eshell 'x)
  (rename-buffer (concat "*eshell-" bn "*"))
)

(defun bshell-spawn(bn)
  "Open a new instance of bshell."
  (interactive "sbshell name:")
  (message bn)
  (bshell-new)
  (rename-buffer (concat "*bshell-" bn "*"))
  )


(provide 'g4z3)
;;; g4z3.el ends here
