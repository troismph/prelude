;;; package --- g4z3's custom packages

;;; Commentary:

;;; Code:

(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)

;; add external dirs to load-path
(let ((default-directory  "~/.emacs.d/external/"))
  (normal-top-level-add-subdirs-to-load-path))

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
(prelude-require-package 'gnus)
(prelude-require-package 'meghanada)
(prelude-require-package 'ensime)
(prelude-require-package 'scala-mode)
(prelude-require-package 'material-theme)
(prelude-require-package 'seq)
(prelude-require-package 'all-the-icons)
(prelude-require-package 'graphviz-dot-mode)
(load "ox-reveal")
(load "org-crypt")
(prelude-require-package 'ansible)
(prelude-require-package 'yaml-mode)
(load "openapi-yaml-mode")
(prelude-require-package 'ox-pandoc)
(load "f")
(prelude-require-package 'git-auto-commit-mode)

;; start frame in maximized state
(add-to-list 'default-frame-alist '(fullscreen . maximized))

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

;; (setq projectile-switch-project-action 'neotree-projectile-action)

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

(setq org-outline-path-complete-in-steps nil)
(setq org-lowest-priority 68)

(defun org-agenda-contemplations()
  (interactive)
  (org-tags-view nil "+DEADLINE=\"\"+SCHEDULED=\"\"/!")
  )
(defun org-agenda-now()
  (interactive)
  (org-agenda-list)
  )

;; (workgroups-mode 1)

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

(setq org-src-window-setup (quote other-window))

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

(add-hook 'java-mode-hook
          (lambda ()
            ;; meghanada-mode on
            (meghanada-mode t)
            (flycheck-mode +1)
            (setq c-basic-offset 2)
            ;; use code format
            (add-hook 'before-save-hook 'meghanada-code-beautify-before-save)))
(cond
 ((eq system-type 'windows-nt)
  (setq meghanada-java-path (expand-file-name "bin/java.exe" (getenv "JAVA_HOME")))
  (setq meghanada-maven-path "mvn.cmd"))
 (t
  (setq meghanada-java-path "java")
  (setq meghanada-maven-path "mvn")))

(setq g4z3-org-refile-exclude '("journal.org"))

(setq g4z3-org-refile-exclude-regex "journal")

(defun g4z3-org-refile-filter(s)
  (and (string-match "^[^#]*\.org$" s) (not (string-match g4z3-org-refile-exclude-regex s)))
  )

(defun g4z3-expand-path-by-project (p)
  (let ((prj-root (projectile-project-root)))
    (expand-file-name p prj-root)
    )
  )

(defun g4z3-org-refile-targets ()
  (seq-filter 'g4z3-org-refile-filter
    (mapcar 'g4z3-expand-path-by-project (projectile-current-project-files)))
  )

(projectile-mode +1)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(defun g4z3-org-archive-done-tasks ()
  (interactive)
  (org-map-entries
   (lambda ()
     (org-archive-subtree)
     (setq org-map-continue-from (outline-previous-heading)))
   "/DONE" 'tree))

(setq neo-theme (if (display-graphic-p) 'icons 'arrow))

(org-crypt-use-before-save-magic)
(setq org-tags-exclude-from-inheritance (quote ("crypt")))
(setq org-crypt-key nil)

(setq neo-window-fixed-size nil)

(add-hook 'c-mode-common-hook
  (lambda()
    (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
      (ggtags-mode 1))
    (local-set-key (kbd "C-c C-<right>") 'hs-show-block)
    (local-set-key (kbd "C-c C-<left>")  'hs-hide-block)
    (local-set-key (kbd "C-c C-<up>")    'hs-hide-all)
    (local-set-key (kbd "C-c C-<down>")  'hs-show-all)
    (local-set-key (kbd "C->") 'highlight-symbol-at-point)
    (local-set-key (kbd "C-<") 'hi-lock-mode)
    (hs-minor-mode t)
    )
  )

(load-library "org-recoll")
(add-hook 'org-mode-hook
  (lambda()
    (local-set-key (kbd "C-c C-g") 'org-recoll-search)
    (local-set-key (kbd "M-.") 'org-open-at-point)
    (local-set-key (kbd "M-,") 'org-mark-ring-goto)
    )
  )

(global-set-key (kbd "C-S-p") 'scroll-down-line)
(global-set-key (kbd "C-S-n") 'scroll-up-line)

(defun g4z3-print-elem (elem)
  "Print AST element ELEM."
  (let* ((cb (org-element-property :contents-begin elem))
         (ce (org-element-property :contents-end elem))
         )
    (buffer-substring-no-properties cb ce)
    )
  )


(defun g4z3-verses-load-random (file_path)
  "Load a random section from FILE_PATH."
  (with-temp-buffer
    (insert-file-contents file_path)
    (let* ((ast (org-element-parse-buffer))
           (cnt (- (length ast) 2)))
      (g4z3-print-elem (nth (random cnt) ast))
      )
    )
  )

(setq g4z3-verse-file-path "~/src/notes/stray_birds.org")

(defun g4z3-verse-of-the-day ()
  "Print a random verse from g4z3-verse-file-path."
  (interactive)
  (with-output-to-temp-buffer "verse_of_the_day"
    (print (g4z3-verses-load-random g4z3-verse-file-path))
    ))

(add-hook 'after-make-frame-functions
          (lambda(frame)
            (select-frame frame)
            (g4z3-verse-of-the-day)
            )
          )

(cond
 ((eq system-type 'windows-nt)
  )
 (t
  (g4z3-verse-of-the-day)
  ))

(global-set-key (kbd "C-c v") 'g4z3-verse-of-the-day)

(defun g4z3-org-link-info (link)
  (when (string= (org-element-property :type link) "file")
    (org-element-property :path link))
  )

;; generate a list of link files from an org file
;; must be invoked in an org buffer
(defun g4z3-org-list-link-files ()
  (org-element-map (org-element-parse-buffer) 'link 'g4z3-org-link-info)
  )


(provide 'g4z3)
;;; g4z3.el ends here
