;;; Commentary:
;;; g4z3's custom file, copy it to personal/custom.el

;;; Code:

(global-linum-mode 1)
(setq linum-format "%d ")


(custom-set-variables
 '(company-c-headers-path-system
  (quote
   ("/usr/include/" "/usr/local/include/" "/usr/include/c++/7/")))
 '(company-clang-arguments (quote ("-I/usr/include/c++/7" "-I/usr/include/")))
 '(custom-safe-themes
   (quote
    ("a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default))))

(custom-set-faces
 '(whitespace-tab ((t (:background "black" :underline t)))))

(load-theme 'smart-mode-line-light)

(add-to-list 'company-backends 'company-c-headers)
(global-set-key [home] 'move-beginning-of-line)
(global-set-key [select] 'move-end-of-line)

;; pytho virtual env settings
(venv-initialize-interactive-shells) ;; if you want interactive shell support
(venv-initialize-eshell) ;; if you want eshell support
;; note that setting `venv-location` is not necessary if you
;; use the default location (`~/.virtualenvs`), or if the
;; the environment variable `WORKON_HOME` points to the right place

;;; g4z3_custom.el ends here
