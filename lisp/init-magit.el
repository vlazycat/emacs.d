;; init-magit.el

;; Git using magit
(use-package magit
  :ensure t
  :config
  (global-set-key (kbd "C-c g") 'magit-status)
  ;; Disable built in VC mode for better performance
  (setq vc-handled-backends nil)
  ;; Disable refreshing status for better performance
  (setq magit-refresh-status-buffer nil)
  ;; Always open the commit edit message in a new window instead of the
  ;; *magit* window so you can see the diff
  (add-to-list 'display-buffer-alist
         '(".*COMMIT_EDITMSG". ((display-buffer-pop-up-window) .
              ((inhibit-same-window . t))))))

(provide 'init-magit)
