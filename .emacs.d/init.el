;;; init --- Summary

;;; Commentary:
;; My not so über Emacs init

;;; Code:
(add-to-list 'load-path (concat user-emacs-directory "lisp"))

;; Custom set variables go here.
(setq custom-file (concat user-emacs-directory "lisp/custom.el"))
(load custom-file 'noerror)

;; Elpa
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))
(when (not (package-installed-p 'use-package))
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))

;; Align equals signs - https://stackoverflow.com/a/8129994
(defun align-regexp--around (orig-align-regexp &rest args)
  "Advice to make `align-regexp' use spaces, not tabs."
  (let ((indent-tabs-mode nil))
    (apply orig-align-regexp args)))
(advice-add 'align-regexp :around #'align-regexp--around)

;; Quelpa
(use-package quelpa
  :ensure t)
(use-package quelpa-use-package
  :ensure t
  :config
  (setq-default quelpa-build-tar-executable "~/.local/brew/bin/gtar"))

(use-package copilot
  :ensure t
  :quelpa (copilot :fetcher github
                   :repo "copilot-emacs/copilot.el"
                   :branch "main"
                   :files ("*.el"))
  :config
  (add-hook 'prog-mode-hook 'copilot-mode)
  (define-key copilot-completion-map (kbd "TAB") 'copilot-next-completion)
  (define-key copilot-completion-map (kbd "C-e") 'copilot-accept-completion))

;; Eglot
(defun my/eglot-save-hooks ()
  "Add hooks to format and organize imports."
  (unless (derived-mode-p 'rust-mode 'rust-ts-mode)
    (add-hook 'before-save-hook (lambda ()
                                  (eglot-format-buffer)
                                  (eglot-code-action-organize-imports nil)) t t)))
(add-hook 'prog-mode-hook #'eglot-ensure)
(add-hook 'prog-mode-hook #'my/eglot-save-hooks)
(add-hook 'yaml-ts-mode-hook #'eglot-ensure)
(add-hook 'yaml-ts-mode-hook #'my/eglot-save-hooks)

;; Rust
(use-package rustic
  :ensure t
  :config
  (setq rustic-lsp-client 'eglot)
  (add-hook 'rustic-mode-hook #'my/eglot-save-hooks))

;; Projectile
(use-package projectile
  :ensure t
  :config
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1))
(use-package flx-ido
  :ensure t)

;; Flycheck
(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode))
(use-package flycheck-eglot
  :ensure t
  :after (flycheck eglot)
  :config
  (global-flycheck-eglot-mode 1))

(use-package apheleia
  :ensure apheleia
  :diminish ""
  :defines
  apheleia-formatters
  apheleia-mode-alist
  :functions
  apheleia-global-mode
  :config
  (let ((ts (alist-get 'apheleia-typescript apheleia-formatters)))
    (setf (alist-get 'prettier-npx apheleia-formatters) ts))
  (apheleia-global-mode +1))
(setq-default indent-tabs-mode nil)

;; Appearance
(use-package dracula-theme
  :ensure t
  :config
  (load-theme 'dracula t))

(setq inhibit-splash-screen t)
(setq ring-bell-function 'ignore)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(column-number-mode t)
(let* ((font-size
        (cond ((string= system-type "darwin") "12")
              ((string= system-type "gnu/linux") "14")
              ((string= system-type "windows-nt") "10")))
       (font (concat "Source Code Pro-" font-size)))
  (set-face-attribute 'default nil :font font)
  ;; For emacsclient:
  (setq default-frame-alist `((font . ,font)
                              (menu-bar-lines . 0)
                              (horizontal-scroll-bars . nil)
                              (vertical-scroll-bars . nil))))

;; Keys
(setq mac-command-modifier 'meta)
(global-set-key "\C-w" 'backward-kill-word) ; Redefine C-w
(global-set-key "\C-x\C-k" 'kill-region)    ; Give previously defined C-w a different shortcut

;; Company
(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1))

;; Auto-complete and yasnippet
(add-hook 'after-init-hook 'global-company-mode)

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1))
(use-package yasnippet-snippets
  :ensure t)

;; Move autosave files
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Set exec-path from shell's PATH
(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(use-package paredit
  :ensure t)

(use-package ido
  :config
  (ido-mode t)
  (setq ido-enable-flex-matching t))

(use-package ido-completing-read+
  :ensure t
  :config
  (ido-ubiquitous-mode t))

(use-package smex
  :ensure t
  :config
  (global-set-key (kbd "M-x") 'smex))

(use-package treesit-auto
  :ensure t
  :config
  (setq treesit-auto-install t)
  (global-treesit-auto-mode)
  (treesit-auto-add-to-auto-mode-alist 'all))

;; Web Mode
(use-package web-mode
  :ensure t
  :mode "\\.erb\\'"
  :mode "\\.eex\\'"
  :mode "\\.html?\\'"
  :config
  (setq
   web-mode-code-indent-offset 2
   web-mode-css-indent-offset 2
   web-mode-markup-indent-offset 2))

(use-package go-projectile
  :ensure t
  :config
  (require 'go-projectile))

;; Python
(use-package python-black
  :ensure t
  :after python
  :hook (python-mode . python-black-on-save-mode))

;; Linux kernel
(defun c-lineup-arglist-tabs-only (_)
  "Line up argument lists by tabs, not spaces."
  (let* ((anchor (c-langelem-pos c-syntactic-element))
         (column (c-langelem-2nd-pos c-syntactic-element))
         (offset (- (1+ column) anchor))
         (steps (floor offset c-basic-offset)))
    (* (max steps 1)
       c-basic-offset)))

(add-hook 'c-mode-common-hook
          (lambda ()
            ;; Add kernel style
            (c-add-style
             "linux-tabs-only"
             '("linux" (c-offsets-alist
                        (arglist-cont-nonempty
                         c-lineup-gcc-asm-reg
                         c-lineup-arglist-tabs-only))))))

(add-hook 'c-mode-hook
          (lambda ()
            (let ((filename (buffer-file-name)))
              ;; Enable kernel mode for the appropriate files
              (when (or
                     (and filename
                          (string-match (expand-file-name "~/code/linux")
                                        filename))
                     (and filename
                          (string-match (expand-file-name "~/code/eudyptula")
                                        filename)))
                (setq indent-tabs-mode t)
                (setq show-trailing-whitespace t)
                (c-set-style "linux-tabs-only")))))


;; Terraform
(use-package terraform-mode
  :ensure t
  :custom
  (terraform-format-on-save t)
  :config
  (add-hook 'terraform-mode-hook #'eglot-ensure))

;; Writing
(use-package adoc-mode
  :ensure t
  :config
  (autoload 'adoc-mode "adoc-mode" nil t)
  (add-to-list 'auto-mode-alist '("\\.adoc\\'" . adoc-mode)))
(use-package sphinx-mode
  :ensure t)
(use-package sphinx-frontend
  :ensure t)

;; TOML
(use-package toml-mode
  :ensure t)

;; Zig
(use-package zig-mode
  :ensure t)

(provide 'init)
;;; init.el ends here
