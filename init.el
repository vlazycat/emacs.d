;;        ___                                              __      
;;       /\_ \                                            /\ \__   
;; __  __\//\ \      __     ____    __  __    ___     __  \ \ ,_\  
;;/\ \/\ \ \ \ \   /'__`\  /\_ ,`\ /\ \/\ \  /'___\ /'__`\ \ \ \/  
;;\ \ \_/ | \_\ \_/\ \L\.\_\/_/  /_\ \ \_\ \/\ \__//\ \L\.\_\ \ \_ 
;; \ \___/  /\____\ \__/.\_\ /\____\\/`____ \ \____\ \__/.\_\\ \__\
;;  \/__/   \/____/\/__/\/_/ \/____/ `/___/> \/____/\/__/\/_/ \/__/
;;                                      /\___/                     
;;                                      \/__/                      
;;; init.el

;; Emacs 27.1 at least
(let* ((minver "27.1"))
  (when (version< emacs-version minver)
    (error "Emacs v%s or higher is required." minver)))

;; Optimize gc
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; Keep a ref to the actual file-name-handler
(defvar default-file-name-handler-alist file-name-handler-alist)

;; Set the file-name-handler to nil,
;; because regexing is CPU intensive
(setq file-name-handler-alist nil)

;; Reset file-name-handler-alist after initialization
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold 16777216
                  gc-cons-percentage 0.1
                  file-name-handler-alist default-file-name-handler-alist)))

;; Increase the amount of data from the process
(setq read-process-output-max (* 1024 1024))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Load packages
(require 'init-package)

(require 'init-basic)
(require 'init-company)

(require 'init-editor)
(require 'init-flycheck)
(require 'init-function)
(require 'init-hydra)
(require 'init-ido)
(require 'init-evil)

(require 'init-org)

(require 'init-lsp)
(require 'init-lang)
(require 'init-magit)
(require 'init-projectile)

(require 'init-osx)
(require 'init-treemacs)
(require 'init-ui)

;; Store customizations in a separate file
(setq custom-file "~/.emacs.d/.customizations.el")
(load custom-file t)

(provide 'init)
