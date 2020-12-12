;; -*- lexical-binding: t; -*-
;; init-ido.el

;; ido
;; Make sure we are using ido mode
(use-package ido
  :ensure t
  :config
  (ido-mode 1)
  (ido-everywhere 1)
  (setq ido-everywhere t)
  (ido-mode (quote both))
  (setq ido-use-faces t)
  ;; Don't magically search for a file that doesn't exist
  (setq ido-auto-merge-work-directories-length -1))

;; Use ido inside ido buffer too
(use-package flx-ido
  :ensure t
  :config
  (flx-ido-mode 1)
  (setq ido-enable-flex-matching t)

  (setq ido-max-directory-size 100000)

  ;; Use the current window when visiting files and buffers with ido
  (setq ido-default-file-method 'selected-window)
  (setq ido-default-buffer-method 'selected-window)

  ;; Disable minibuffer exit for ido
  (put 'ido-exit-minibuffer 'disabled nil)

  ;; Display ido results vertically, rather than horizontally
  (setq ido-decorations (quote ("\n====> " "" "\n   " "\n   ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]")))
  (defun ido-disable-line-truncation ()
    (set (make-local-variable 'truncate-lines) nil))
  (add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-truncation)

  ;; Use normal up/down keyboard shortcuts
  (defun ido-define-keys ()
    (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
    (define-key ido-completion-map (kbd "C-p") 'ido-prev-match))
  (add-hook 'ido-setup-hook 'ido-define-keys))

(provide 'init-ido)
