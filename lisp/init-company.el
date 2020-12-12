;; -*- lexical-binding: t; -*-
;; init-company.el

(use-package company
  :bind (:map company-active-map
              ("C-p" . company-select-previous)
              ("C-n" . company-select-next)
              ("C-k" . company-select-previous)
              ("C-j" . company-select-next)
              ("<tab>" . company-complete-common-or-cycle))
  :hook (after-init . global-company-mode)
  :init
  (setq company-require-match nil)
  )

; ;; 著名的Emacs补全框架
; (use-package company 
;   :hook (prog-mode . company-mode) 
;   :init (setq company-tooltip-align-annotations t company-idle-delay 0.1 company-echo-delay 0
;               company-minimum-prefix-length 2 company-require-match nil company-dabbrev-ignore-case
;               nil company-dabbrev-downcase nil company-show-numbers t) 
;   :config 
;   :bind (:map company-active-map
;               ("M-n" . nil) 
;               ("M-p" . nil) 
;               ("C-n" . #'company-select-next) 
;               ("C-p" . #'company-select-previous))) 

; ;; 人工智能补全代码
; (use-package company-tabnine
;   :disabled 
;   :ensure t 
;   :after 'company-mode 
;   'company-tabnine-mode 
;   :config (add-to-list 'company-backends #'company-tabnine))

; ;; Emacs对语言服务器支持的插件
; (use-package lsp-mode
;   :ensure t
;   :commands (lsp)
;   :hook ((java-mode js-mode  web-mode c-mode c++-mode rust-mode python-mode) . lsp)
;   :custom
;   (lsp-idle-delay 200)
;   (lsp-auto-guess-root nil)
;   (lsp-file-watch-threshold 2000)
;   (read-process-output-max (* 1024 10240))
;   (lsp-eldoc-hook nil)
;   (lsp-prefer-flymake nil)
;   :bind (:map lsp-mode-map
;         ("C-c C-f" . lsp-format-buffer)
;         ("M-RET" . lsp-ui-sideline-apply-code-actions))
;   :config
;   (setq lsp-prefer-capf t))

; ;; 各个语言的Debug工具
; (use-package dap-mode
;   :ensure t
;   :functions dap-hydra/nil
;   :diminish
;   :bind (:map lsp-mode-map
;         ("<f5>" . dap-debug)
;         ("M-<f5>" . dap-hydra))
;   :hook ((after-init . dap-mode)
;      (dap-mode . dap-ui-mode)
;      (python-mode . (lambda () (require 'dap-python)))
;      ((c-mode c++-mode) . (lambda () (require 'dap-lldb)))))


; ;; 美化company
; (use-package company-box 
;   :ensure t 
;   :hook (company-mode . company-box-mode))

; ;; 代码片段
; (use-package yasnippet 
;   :ensure t
;   :config
;   (yas-global-mode 1)
;   (setq yas-snippet-dirs '("~/.emacs.d/snippets")))

(provide 'init-company)
