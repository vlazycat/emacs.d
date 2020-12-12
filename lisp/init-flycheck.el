;; -*- lexical-binding: t; -*-
;; init-flycheck.el

;; Flycheck
(use-package flycheck
  :ensure t
  :config
  ;; (global-flycheck-mode)
  (setq flycheck-global-modes '(not rust-mode))
  (global-set-key (kbd "C-c M-n") 'flycheck-next-error)
  (global-set-key (kbd "C-c M-p") 'flycheck-previous-error))

(provide 'init-flycheck)
