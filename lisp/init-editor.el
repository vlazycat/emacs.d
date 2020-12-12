;; init-editor.el

;; Emdash
(defun insert-em-dash ()
  "Insert a em-dash"
  (interactive)
  (insert "—"))
(global-set-key [(meta _)] 'insert-em-dash)

;; Delete trailing whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(use-package ace-window
  :ensure t)

;; Nice writing layout
(use-package writeroom-mode
  :ensure t
  :config
  (add-hook 'writeroom-mode-hook
            (lambda ()
              ;; Use custom font face for this buffer only
              (defface tmp-buffer-local-face
                '((t :family "Fira Code" :height 180))
                "Temporary buffer-local face")
              (buffer-face-set 'tmp-buffer-local-face)
              ;; Add padding to the top of the frame
              (setq header-line-format " ")
              ;; Use a skinny cursor
              (make-local-variable 'cursor-type)
              (setq cursor-type 'bar))))

;; Detect things like weasel words
(use-package writegood-mode
  :ensure t
  :config
  (global-set-key "\C-c\C-gl" 'writegood-grade-level)
  (global-set-key "\C-c\C-ge" 'writegood-reading-ease))

; ;; Unity
; (use-package glsl-mode
;   :ensure t
;   :config
;   (add-to-list 'auto-mode-alist '("\\.shader$" . glsl-mode)))

;; Linum mode shortcut
(global-set-key (kbd "C-x l") 'linum-mode)

;; iEdit mode
(global-set-key (kbd "C-x ;") 'iedit-mode)

;; Web browser (eww)
(global-set-key (kbd "C-x M-w") 'eww)

;; Disable auto fill mode because it's annoying
(auto-fill-mode -1)
(remove-hook 'text-mode-hook #'turn-on-auto-fill)

;; Flyspell mode
(use-package flyspell
  :config
  (setq ispell-program-name "/usr/local/bin/aspell")
  (add-hook 'python-mode-hook (lambda () (flyspell-prog-mode)))
  (add-hook 'coffee-mode-hook (lambda () (flyspell-prog-mode)))
  (add-hook 'hbs-mode-hook (lambda () (flyspell-prog-mode)))
  (add-hook 'org-mode-hook (lambda () (flyspell-prog-mode))))

;; Use spaces instead of tabs when indenting
; (setq-default indent-tabs-mode nil)

;; Ivy auto-complete
(use-package ivy
  :ensure t
  :diminish ivy-mode
  :hook (after-init . ivy-mode))

(provide 'init-editor)
