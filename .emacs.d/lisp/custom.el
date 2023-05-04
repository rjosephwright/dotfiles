(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(byte-compile-warnings '(cl-functions))
 '(fill-column 80)
 '(js-indent-level 2)
 '(org-agenda-files
   '("~/Dropbox/org/gtd/inbox.org" "~/Dropbox/org/gtd/projects.org" "~/Dropbox/org/gtd/scheduled.org"))
 '(org-capture-templates
   '(("t" "Todo" entry
      (file "~/Dropbox/org/gtd/inbox.org")
      "* TODO %i%?")
     ("s" "Scheduled" entry
      (file "~/Dropbox/org/gtd/scheduled.org")
      "* %i%? \n %U")))
 '(org-refile-targets
   '(("~/Dropbox/org/gtd/projects.org" :maxlevel . 3)
     ("~/Dropbox/org/gtd/someday.org" :level . 1)
     ("~/Dropbox/org/gtd/scheduled.org" :maxlevel . 2)))
 '(org-refile-use-outline-path 'file)
 '(package-selected-packages
   '(nix-mode toml-mode sphinx-frontend sphinx-mode adoc-mode terraform-mode reason-mode zig-mode groovy-mode rustic omnisharp elm-mode cider clojure-mode lsp-pyright auto-virtualenv go-projectile go-snippets go-mode alchemist elixir-mode web-mode company-ghci company-cabal hindent lsp-haskell haskell-mode geiser quack smex ido-completing-read+ paredit exec-path-from-shell yasnippet-snippets yasnippet company dracula-theme flycheck flx-ido projectile yaml-mode vagrant-tramp use-package sbt-mode queue markdown-mode magit f dash better-defaults async))
 '(safe-local-variable-values '((encoding . utf-8)))
 '(tramp-default-method "scpx"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil)))))
