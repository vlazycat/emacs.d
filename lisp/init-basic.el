;; -*- lexical-binding: t; -*-
;; init-basic.el

; ;; Debug
; (setq debug-on-error t)
; (setq-default lexical-binding t)

;; Personal information
(setq user-full-name "kun")
(setq user-main-address "fanglk@pku.edu.cn")

;; Better defaults for editing
(setq-default indent-tabs-mode nil
              tab-width 4
              fill-column 80
              word-wrap t
              truncate-lines t)

(setq require-final-newline t)

(add-hook 'text-mode-hook #'visual-line-mode)

(defvar show-paren-delay)
(setq show-paren-delay 0.0)
(show-paren-mode t)

;; Restart emacs
(use-package restart-emacs
  :ensure t)

; ;; Swipper
; (use-package swiper
;   :config
;   (ivy-mode 1)
;   (setq ivy-use-virtual-buffers t)
;   (setq enable-recursive-minibuffers t)
;   (setq search-default-mode #'char-fold-to-regexp)
;   (global-set-key "\C-s" 'swiper)
;   )

; ;; Counsel
; (use-package counsel
;   :config
;   (setq ivy-initial-inputs-alist nil)  ;; do not show `^' in counsel command
;   )

;; Max image size when using the builtin viewer
(setq max-image-size 50.0)

;; Disable visual bell
(setq visible-bell nil)

;; Disable backup files
(setq make-backup-files nil)
;; Disable auto save files
(setq auto-save-default nil)

;; Don't suspend emacs with C-z
(global-unset-key [?\C-z])

;; Use iBuffer by default for C-x C-b which is more readable than default
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Shortcut for occur
(global-set-key (kbd "C-x C-o") 'occur)

;; Shortcut for rgrep using ripgrep because it's faster
(global-set-key (kbd "C-x C-r") 'ripgrep-regexp)

;; Shortcut for rectangle edits
(global-set-key (kbd "C-x r i") 'string-insert-rectangle)

;; Shortcuts for going forward and backwards cycling windows
(global-set-key (kbd "C-x p") 'other-window)
(global-set-key (kbd "C-x o") 'previous-multiframe-window)

;; Expand region
(use-package expand-region
  :ensure t
  :config
  (global-set-key (kbd "\C-x\ =") 'er/expand-region))

;; Ace jump mode
(use-package ace-jump-mode
  :ensure t
  :config
  (define-key global-map (kbd "C-c SPC") 'ace-jump-mode))

;; Auto refresh all buffers when files change ie git branch switching
(global-auto-revert-mode t)

;; Don't use any character as the vertical border divider
(set-display-table-slot standard-display-table
                        'vertical-border
                        (make-glyph-code ? ))


(use-package which-key
  :ensure t
  :config
  (which-key-mode))

;; Use golden ratio mode which automatically resizes buffers based on
;; the golden ratio
(use-package golden-ratio
  :ensure t
  :config
  ;; (golden-ratio-mode 1)
  ;; (setq golden-ratio-auto-scale nil)
  (defadvice previous-multiframe-window
      (after golden-ratio-resize-window)
    (golden-ratio) nil))

;; Shortcut to jump word and ignore underscore
(define-key global-map (kbd "M-F") 'forward-sexp)
(define-key global-map (kbd "M-B") 'forward-sexp)

;; Enable system clipboard
(setq x-select-enable-clipboard t)

;; Stackoverflow plugin
(use-package sos 
  :ensure t)

;; Start emacs without all the fanfare
(setq inhibit-startup-echo-area-message t)
(setq inhibit-startup-message t)

(global-set-key (kbd "C-c +") 'increment-and-eval-block)
(global-set-key (kbd "C-c -") 'decrement-and-eval-block)

;; Set a redo command
(global-set-key (kbd "C-x M-r") 'repeat-complex-command)

;; browse-kill-ring with M-y
(use-package browse-kill-ring
  :ensure t
  :config
  (browse-kill-ring-default-keybindings))

;; Prevent accidental upcase/downcase
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(provide 'init-basic)
