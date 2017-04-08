;;; init --- Summary

;;; Commentary:
;; My not so über Emacs init

;;; Code:
;; Variables set via customize
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cider-prompt-for-symbol nil)
 '(fill-column 80)
 '(js-indent-level 2)
 '(org-agenda-files (quote ("~/.emacs.d/org-mode")))
 '(package-selected-packages
   (quote
    (auctex hindent haskell-snippets racer intero yaml-mode web-mode vagrant-tramp use-package sublime-themes smex rust-mode rsense rbenv ponylang-mode paredit org-plus-contrib org markdown-mode magit inf-ruby ido-ubiquitous go-snippets go-mode go-autocomplete geiser fold-this flycheck-rust exec-path-from-shell elm-mode edts company-ghci company-ghc company-cabal color-theme cider better-defaults alchemist)))
 '(safe-local-variable-values (quote ((encoding . utf-8))))
 '(tramp-default-method "scpx")
 '(web-mode-code-indent-offset 2)
 '(web-mode-css-indent-offset 2)
 '(web-mode-markup-indent-offset 2))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Elpa
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")
                         ("org" . "http://orgmode.org/elpa/")))
(package-initialize)
;; Install desired packages automatically
(when (not package-archive-contents)
  (package-refresh-contents))
(defvar package-list
  '(alchemist
    async
    better-defaults
    cider
    clojure-mode
    color-theme
    company
    company-cabal
    company-ghc
    company-ghci
    dash
    edts
    elixir-mode
    elm-mode
    erlang
    exec-path-from-shell
    f
    flycheck
    flycheck-rust
    go-autocomplete
    go-mode
    go-snippets
    inf-ruby
    magit
    markdown-mode
    org
    org-plus-contrib
    queue
    rbenv
    rsense
    rust-mode
    sbt-mode
    smex
    sublime-themes
    use-package
    vagrant-tramp
    web-mode
    yaml-mode))
(dolist (package package-list)
  (when (not (package-installed-p package))
    (package-install package)))

(eval-when-compile
  (require 'use-package))

;; Appearance
(setq inhibit-splash-screen t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(column-number-mode t)
(load-theme 'spolsky t)
(set-face-attribute 'default nil :font "Source Code Pro-12")

;; Keys
(setq mac-command-modifier 'meta)
(global-set-key "\C-w" 'backward-kill-word) ; Redefine C-w
(global-set-key "\C-x\C-k" 'kill-region)    ; Give previously defined C-w a different shortcut

;; Auto-complete and yasnippet
(add-hook 'after-init-hook 'global-company-mode)
(require 'yasnippet)
(yas-global-mode 1)

;; Move autosave files
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Set exec-path from shell's PATH
(exec-path-from-shell-initialize)

(use-package paredit
  :ensure t)

(use-package ido
  :config
  (ido-mode t)
  (setq ido-enable-flex-matching t))

(use-package ido-ubiquitous
  :ensure t
  :config
  (ido-ubiquitous-mode t))

(use-package smex
  :ensure t
  :config
  (global-set-key (kbd "M-x") 'smex))

(use-package ponylang-mode
  :ensure t
  :config
  (progn
    (add-hook
     'ponylang-mode-hook
     (lambda ()
       (set-variable 'indent-tabs-mode nil)
       (set-variable 'tab-width 2)))))

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
  :ensure t)

(use-package haskell-snippets
  :ensure t)

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

;; Flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)

;; Ruby
(require 'rbenv)
(global-rbenv-mode)
(add-hook 'ruby-mode-hook 'robe-mode)
(add-to-list 'auto-mode-alist '("\\.ru\\'" . ruby-mode))

;; Elixir
(require 'elixir-mode)
(require 'alchemist)
(setq alchemist-goto-erlang-source-dir "~/opt/brew/Cellar/erlang-r19/19.0.1/share/src/otp-OTP-19.0.1")
(setq alchemist-goto-elixir-source-dir "~/opt/brew/Cellar/elixir/1.3.0/src/elixir-1.3.0")

;; Web Mode
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.eex\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js[x]?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.handlebars\\'" . web-mode))

;; Org mode
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;; Erlang
(add-hook 'after-init-hook 'my-after-init-hook)
(defun my-after-init-hook ()
  "Start Erlang Development Tool Suite."
  (require 'edts-start))

;; Go
(require 'go-mode)
(require 'go-autocomplete)
(require 'go-snippets)
(add-hook 'before-save-hook 'gofmt-before-save)
(add-hook 'go-mode-hook (lambda ()
                          (local-set-key (kbd "C-c C-r") 'go-remove-unused-imports)))
(add-hook 'go-mode-hook (lambda ()
                          (local-set-key (kbd "C-c i") 'go-goto-imports)))
(add-hook 'go-mode-hook (lambda ()
                          (local-set-key (kbd "M-.") 'godef-jump)))

(use-package vagrant-tramp
  :ensure t
  :config
  (eval-after-load 'tramp '(vagrant-tramp-add-method)))

;; Clojure
(require 'clojure-mode)
(define-clojure-indent
  (alet 'defun)
  (mlet 'defun))

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
              (when (and filename
                         (string-match (expand-file-name "~/Workspace/eudyptula")
                                       filename))
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
  (setq racer-rust-src-path "~/.rustup/toolchains/stable-x86_64-apple-darwin/lib/rustlib/src/rust/src"))

;; TeX
(use-package tex
  :ensure auctex
  :defer t
  :config
  (setq TeX-auto-save t))

(provide 'init)
;;; init.el ends here
