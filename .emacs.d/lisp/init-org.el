;;; package --- Initialization for org mode.

;;; Commentary:

;;; Code:
(provide 'init-org)

(setq org-gtd-update-ack "3.0.0")
(use-package org-gtd
  :after org
  :ensure t
  :config
  (setq org-edna-use-inheritance t)
  (org-edna-mode)
  (global-set-key (kbd "C-c d c") 'org-gtd-capture)
  (global-set-key (kbd "C-c d e") 'org-gtd-engage)
  (global-set-key (kbd "C-c d n") 'org-gtd-show-all-next)
  (global-set-key (kbd "C-c d p") 'org-gtd-process-inbox)
  (global-set-key (kbd "C-c d s") 'org-gtd-review-stuck-projects)
  (global-set-key (kbd "C-c c") 'org-gtd-organize))

;;; init-org.el ends here
