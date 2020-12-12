;; init-lsp.el

;; LSP settings
;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
(setq lsp-keymap-prefix "s-l")

(use-package lsp-mode
    :hook ((rust-mode . lsp-deferred)
            ;; if you want which-key integration
            (lsp-mode . lsp-enable-which-key-integration))
    :commands lsp lsp-deferred)

;; if you are helm user
(use-package helm-lsp :commands helm-lsp-workspace-symbol)

(provide 'init-lsp)
