;; init-package.el

(require 'package)
(package-initialize) ;; You might already have this line
(setq package-enable-at-startup nil)

;; CN package setting
(setq package-archives '(("gnu" . "http://mirrors.ustc.edu.cn/elpa/gnu/")
                         ("melpa" . "http://mirrors.ustc.edu.cn/elpa/melpa/")
                         ("melpa-stable" . "http://mirrors.ustc.edu.cn/elpa/melpa-stable/")
                         ("org" . "http://mirrors.ustc.edu.cn/elpa/org/")))

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-and-compile
  (setq use-package-always-ensure t)
  (setq use-package-always-defer nil)
  (setq use-package-always-demand nil)
  (setq use-package-expand-minimally nil)
  (setq use-package-enable-imenu-support t))

(eval-when-compile
  (require 'use-package))

;; Bootstrap `quelpa`
(use-package quelpa
  :ensure t
  :custom
  (quelpa-update-melpa-p nil)
  (quelpa-self-upgrade-p nil)
  (quelpa-checkout-melpa-p nil))

(use-package quelpa-use-package
  :ensure t
  :config
  (quelpa-use-package-activate-advice)
  :custom
  (quelpa-use-package-inhibit-loading-quelpa t))

(provide 'init-package)
