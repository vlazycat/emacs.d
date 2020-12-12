;; init-osx.el

;; macOS env settings
(when (memq window-system '(mac ns))
  ;; Set the environment to the same as the shell
  (use-package exec-path-from-shell
    :ensure t
    :config
    (exec-path-from-shell-initialize))

  ;; Set option key to be meta key
  (setq mac-control-modifier 'control)
  (setq mac-option-modifier 'meta)
  (setq mac-command-modifier 'super)

  ;; Always use Default Browser when opening links
  (setq browse-url-browser-function 'browse-url-default-macosx-browser))

(provide 'init-osx)
