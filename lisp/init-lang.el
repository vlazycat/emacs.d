;; init-lang.el

;; Web mode
(use-package web-mode 
  :ensure t
  :config
  (defun web-mode-customization ()
    "Customization for web-mode."
    (setq web-mode-js-indent-offset 2)
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-css-indent-offset 2)
    (setq web-mode-code-indent-offset 2)
    (setq web-mode-enable-auto-pairing t)
    (setq web-mode-enable-css-colorization t))
  (add-hook 'web-mode-hook 'web-mode-customization)
  ;; Javascript
  (add-to-list 'auto-mode-alist '("\\.js$" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))
  ;; Turn off auto saving because js build tools hate temp files
  (add-hook 'web-mode-hook '(lambda () (setq auto-save-default nil)))
  ;; HTML
  (add-to-list 'auto-mode-alist '("\\.html$" . web-mode))
  ;; CSS
  (add-to-list 'auto-mode-alist '("\\.css$" . web-mode)))

(use-package typescript-mode 
  :ensure t
  :config
  (setq typescript-indent-level 2)
  (add-to-list 'auto-mode-alist '("\\.ts$" . typescript-mode))
  (add-hook 'typescript-mode-hook #'eglot-ensure))

(use-package toml-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.toml$" . toml-mode)))

(use-package rust-mode
  :ensure t
  :config
  (define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
  (add-hook 'rust-mode-hook #'eglot-ensure))

(use-package eglot
  :ensure t
  :config
  (global-set-key (kbd "M-n") 'flymake-goto-next-error)
  (global-set-key (kbd "M-p") 'flymake-goto-prev-error)
  (define-key eglot-mode-map (kbd "C-c h") 'eglot-help-at-point)

  ;; Better support for rust projects with multiple sub projects
  (defun my-project-try-cargo-toml (dir)
    (when-let* ((output
                 (let ((default-directory dir))
                   (shell-command-to-string "cargo metadata --no-deps --format-version 1")))
                (js (ignore-errors (json-read-from-string output)))
                (found (cdr (assq 'workspace_root js))))
      (cons 'eglot-project found)))

  (cl-defmethod project-roots ((project (head eglot-project)))
    (list (cdr project)))

  (add-hook 'project-find-functions 'my-project-try-cargo-toml nil nil)

  (add-to-list 'eglot-server-programs
               '((typescript-mode) "typescript-language-server" "--stdio")))

; ;; YASnippet
; (use-package yasnippet
;   :ensure t
;   :config
;   (yas-global-mode 1))

;; Elisp
(use-package paredit
  :ensure t
  :config
  (add-hook 'elisp-mode-hook 'paredit-mode)
  (add-hook 'clojure-mode-hook 'paredit-mode))

;; Python
(use-package python-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode)))

(use-package elpy
  :ensure t
  :config
  (elpy-enable)
  (setq elpy-rpc-backend "rope"))

;; Ruby
(use-package robe
  :ensure t
  :config
  ;; Sane indenting
  (setq ruby-deep-indent-paren nil)
  ;; Don't auto add file encodings!!!
  (setq ruby-insert-encoding-magic-comment nil)
  (add-hook 'ruby-mode-hook 'robe-mode)
  ;; Use specific rubocop
  (defun use-custom-rubocop ()
    (let* ((root (locate-dominating-file
		  (or (buffer-file-name) default-directory)
		  "scripts/bin/rubocop"))
	   (rubocop (and root
			 (expand-file-name "scripts/bin/rubocop"
					   root))))
      (when (and rubocop (file-executable-p rubocop))
	(setq-local flycheck-ruby-rubocop-executable rubocop))))
  (add-hook 'flycheck-mode-hook #'use-custom-rubocop))

;; Format line numbers nicely
(setq linum-format (quote " %3d "))

;; Sass-mode
(use-package sass-mode
  :ensure t
  :config (setq sass-tab-width 2))

;; Processing mode
(use-package processing-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.pde$" . processing-mode))
  (setq processing-location "~/Library/Processing"))

;; Markdown
(use-package markdown-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode)))

;; Json mode
(use-package json-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.json\\'" . json-mode))
  (add-hook 'json-mode 'flymake-json-load))

(provide 'init-lang)
