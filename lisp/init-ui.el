;; init-ui.el

;; Maxsize window while startup
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; UI settings
(when window-system
  ;; Disable pscroll bars in gui emacs
  (scroll-bar-mode -1)
  ;; Disable the tool bar in gui emacs
  (tool-bar-mode -1)
  (tooltip-mode -1))

;; Change the scratch buffer message to nothing
(setq initial-scratch-message nil)

;; Nice startup screen
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-banner-logo-title "Happy hacking Kun")
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-center-content t)
  (setq dashboard-items '((recents  . 5)
                          (projects . 5)
                          (agenda . 5)))
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq show-week-agenda-p t))

;; Show the time
(display-time-mode 1)

;; When at the end of a buffer don't jump, scroll
(setq scroll-step 1 scroll-conservatively 10000)
(setq scroll-margin 0)

;; Gui emacs still sets a line color in between
(set-face-background 'vertical-border "gray32")
(set-face-foreground 'vertical-border (face-background 'vertical-border))

;; Rainbow parantheses
(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

;; Shells should have color
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; Unload a theme before enabling another
(defadvice load-theme (before theme-dont-propagate activate)
  (mapc #'disable-theme custom-enabled-themes))

;; Use doom theme
(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (doom-themes-org-config)
  (load-theme 'doom-oceanic-next t))

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode))

(use-package all-the-icons 
  :ensure t)

;; Set default font
;; When using gui emacs with emacsclient, the following makes sure the
;; window is opened with the correct font size otherwise it will
;; revert to 13px for font size
(add-to-list 'default-frame-alist '(font . "Sarasa Mono CL 14"))
;; Make the default face the same font
(set-face-attribute 'default t :font "Sarasa Mono CL 14")
(set-face-attribute 'default nil :height 120)

;; Keyboard shortcut for using a big screen
(setq big-screen nil)

(defun toggle-big-screen ()
  (interactive)
  (if big-screen
      (progn
  (setq big-screen nil)
  (set-face-attribute 'default nil :height 120))
    (progn
      (set-face-attribute 'default nil :height 160)
      (setq big-screen 1))))
(global-set-key (kbd "C-x M-b") 'toggle-big-screen)

;; 用GUI tooltips来显示检查到的错误
(progn
  (use-package flycheck-posframe 
  :ensure t 
  :custom-face (flycheck-posframe-border-face ((t 
                          (:inherit default)))) 
  :hook (flycheck-mode . flycheck-posframe-mode) 
  :init (setq flycheck-posframe-border-width 1 flycheck-posframe-inhibit-functions '((lambda 
                                             (&rest _) 
                                             (bound-and-true-p
                                              company-backend))))) 
  (use-package flycheck-pos-tip 
  :ensure t 
  :defines flycheck-pos-tip-timeout 
  :hook (global-flycheck-mode . flycheck-pos-tip-mode) 
  :config (setq flycheck-pos-tip-timeout 30)) 
  (use-package flycheck-popup-tip 
  :ensure t 
  :hook (flycheck-mode . flycheck-popup-tip-mode)))

;; Max-size startup
; (when (display-graphic-p)
;   (toggle-frame-maximized))

; ;;启动emacs时窗口最大化
; (defun fullscreen ()
;   (interactive)
;   (set-frame-parameter nil 'fullscreen
;        (if (frame-parameter nil 'fullscreen) nil 'fullboth)))

; (global-set-key [f12] 'fullscreen)

; (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
; (add-to-list 'package-archives
;              '("melpa-stable" . "https://stable.melpa.org/packages/") t)
; (add-to-list 'package-archives
;               '("org" . "http://orgmode.org/elpa/") t)
; (package-initialize)


(provide 'init-ui)
