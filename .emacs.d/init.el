;;; init --- Summary

;;; Commentary:
;; My not so über Emacs init

(if (file-exists-p "~/.emacs.d/init/env.el")
    (load "~/.emacs.d/init/env"))

;;; Code:
;; Variables set via customize
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fill-column 80)
 '(js-indent-level 2)
 '(package-selected-packages
   (quote
    (company-lsp lsp-ui reason-mode zig-mode sphinx-frontend sphinx-mode quack go-projectile flx-ido projectile ido-completing-read+ yasnippet terraform-mode a groovy-mode sbt-mode rsense clojure-mode elixir-mode dracula-theme adoc-mode auto-virtualenv pyvenv hindent haskell-snippets racer intero yaml-mode web-mode vagrant-tramp use-package sublime-themes smex rust-mode paredit markdown-mode magit go-snippets go-mode go-autocomplete geiser fold-this flycheck-rust exec-path-from-shell company-ghci company-ghc company-cabal color-theme cider better-defaults alchemist)))
 '(safe-local-variable-values (quote ((encoding . utf-8))))
 '(tramp-default-method "scpx"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Elpa
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))
(package-initialize)
;; Install desired packages automatically
(when (not package-archive-contents)
  (package-refresh-contents))
(defvar package-list
  '(async
    better-defaults
    color-theme
    company
    company-cabal
    company-ghc
    company-ghci
    dash
    f
    magit
    markdown-mode
    queue
    rsense
    sbt-mode
    sublime-themes
    use-package
    vagrant-tramp
    yaml-mode))
(dolist (package package-list)
  (when (not (package-installed-p package))
    (package-install package)))

(eval-when-compile
  (require 'use-package))

;; Align equals signs - https://stackoverflow.com/a/8129994
(defadvice align-regexp (around align-regexp-with-spaces)
  "Never use tabs for alignment."
  (let ((indent-tabs-mode nil))
    ad-do-it))
(ad-activate 'align-regexp)

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
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package flycheck-rust
  :ensure t)

;; Appearance
(use-package dracula-theme
  :ensure t)

(setq inhibit-splash-screen t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(column-number-mode t)
(set-face-attribute 'default nil :font "Source Code Pro-14")

;; Keys
(setq mac-command-modifier 'meta)
(global-set-key "\C-w" 'backward-kill-word) ; Redefine C-w
(global-set-key "\C-x\C-k" 'kill-region)    ; Give previously defined C-w a different shortcut

;; Auto-complete and yasnippet
(add-hook 'after-init-hook 'global-company-mode)

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1))

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

;; Scheme
(use-package quack
  :ensure t)

(use-package geiser
  :ensure t
  :config
  (defvar geiser-active-implementations)
  (defvar geiser-repl-query-on-kill-p)
  (setq
   geiser-active-implementations '(racket)
   geiser-repl-query-on-kill-p nil))

;; Haskell
(use-package haskell-mode
  :ensure t
  :mode "\\.hs\\'")

(use-package intero
  :ensure t
  :config
  (add-hook 'haskell-mode-hook 'intero-mode))

(use-package hindent
  :ensure t
  :config
  (add-hook 'haskell-mode-hook #'hindent-mode)
  (setq
   hindent-reformat-buffer-on-save t
   hindent-style "gibiansky"))

;; Web Mode
(use-package web-mode
  :ensure t
  :mode "\\.erb\\'"
  :mode "\\.eex\\'"
  :mode "\\.js[x]?\\'"
  :mode "\\.ts[x]?\\'"
  :mode "\\.html?\\'"
  :config
  (defun my/project-eslint-hook ()
    "Find eslint installed in project"
    (let* ((root (locate-dominating-file
                  (or (buffer-file-name) default-directory)
                  "node_modules"))
           (eslint (and root
                        (expand-file-name "node_modules/.bin/eslint"
                                          root))))
      (when (and eslint (file-executable-p eslint))
        (setq-local flycheck-javascript-eslint-executable eslint))))
  (add-hook 'flycheck-mode-hook #'my/project-eslint-hook)

  (flycheck-add-mode 'javascript-eslint 'web-mode)

  (setq
   web-mode-code-indent-offset 2
   web-mode-css-indent-offset 2
   web-mode-markup-indent-offset 2
   web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'"))))

;; Elixir
(use-package elixir-mode
  :ensure t
  :mode "\\.ex[s]?\\'")
(use-package alchemist
  :ensure t
  :config
  (setq
   alchemist-goto-erlang-source-dir "~/.emacs.d/alchemist-src/erlang"
   alchemist-goto-elixir-source-dir "~/.emacs.d/alchemist-src/elixir"))

;; Go
(use-package go-mode
  :ensure t
  :mode "\\.go\\'"
  :config
  (add-hook 'before-save-hook 'gofmt-before-save)
  (add-hook 'go-mode-hook
            (lambda ()
              (local-set-key (kbd "C-c C-r") 'go-remove-unused-imports)))
  (add-hook 'go-mode-hook
            (lambda ()
              (local-set-key (kbd "C-c i") 'go-goto-imports)))
  (add-hook 'go-mode-hook
            (lambda ()
              (local-set-key (kbd "M-.") 'godef-jump))))
(use-package go-autocomplete
  :ensure t)
(use-package go-snippets
  :ensure t)
(use-package go-projectile
  :ensure t
  :config
  (require 'go-projectile))

;; Python
(use-package auto-virtualenv
  :ensure t
  :config
  (add-hook 'window-configuration-change-hook 'auto-virtualenv-set-virtualenv))

(use-package vagrant-tramp
  :ensure t
  :config
  (eval-after-load 'tramp '(vagrant-tramp-add-method)))

;; Clojure
(use-package clojure-mode
  :ensure t
  :config
  (define-clojure-indent
    (alet 'defun)
    (mlet 'defun)))
(use-package cider
  :ensure t
  :config
  (setq cider-prompt-for-symbol nil))

(use-package elm-mode
  :ensure t
  :config
  (setq elm-format-on-save t)
  (setq elm-package-json "elm.json")
  (add-to-list 'exec-path (concat (expand-file-name "~") "/.local/npm-packages/bin"))
  (add-to-list 'company-backends 'company-elm))

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

;; Rust
(use-package rust-mode
  :ensure t
  :config
  (define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
  (setq company-tooltip-align-annotations t))

(use-package racer
  :ensure t
  :config
  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'racer-mode-hook #'eldoc-mode)
  (add-hook 'racer-mode-hook #'company-mode)
  (setq racer-rust-src-path "~/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/src"))

;; Groovy
(use-package groovy-mode
  :ensure t)

;; Zig
(use-package zig-mode
  :ensure t)

;; Language server
(use-package lsp-mode
  :ensure t)
(use-package lsp-ui
  :config
  (add-hook 'lsp-mode-hook 'lsp-ui-mode)
  :ensure t)
(use-package company-lsp
  :config
  (push 'company-lsp company-backends)
  :ensure t)

;; Reason ML
(use-package reason-mode
  :config
  (add-hook 'reason-mode-hook #'flycheck-mode)
  (add-hook 'reason-mode-hook #'lsp-deferred)
  (add-to-list 'auto-mode-alist '("\\.re\\'" . reason-mode))
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection
				     (concat (expand-file-name "~") "/.local/bin/reason-language-server"))
		    :major-modes '(reason-mode)
		    :notification-handlers (ht ("client/registerCapability" 'ignore))
		    :priority 1
		    :server-id 'reason-ls))
  :ensure t)

;; Terraform
(use-package terraform-mode
  :ensure t)

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

(provide 'init)
;;; init.el ends here
