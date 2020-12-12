;; init-org.el

;; Org-mode

;; Copied from https://github.com/glasserc/etc/commit/3af96f2c780a35d35bdf1b9ac19d80fe2e6ebbf8
;; Work around built-in org-mode so we can load from ELPA.
;; First, remove the built-in org directory from the load-path.
;; Thanks to
;; http://stackoverflow.com/questions/20603578/emacs-does-not-see-new-installation-of-org-mode/20616703#20616703.
;; Without this, use-package will try to require org and succeed.
(eval-when-compile
  (require 'cl))

;; Misc settings
; (setq org-directory "~/Org")
; (setq org-default-notes-file "~/Org/notes/inbox.org")
; (setq org-export-with-sub-superscripts nil)
; (setq org-log-reschedule (quote time))
; (add-hook 'org-mode-hook 'org-indent-mode)

; ;; Set todo keywords
; (setq org-todo-keywords
;       (quote ((sequence "TODO(t)" "STARTED(s)" "MAYBE(m)" "WAITING(w@/!)" "BLOCKED(b@/!)" "|" "DONE(d!/!)" "CANCELLED(c@/!)")
;               (sequence "PROJ(p)" "|" "DONE(d!/!)" "CANCELLED(c@/!)")
;               )))

; (setq org-todo-keyword-faces
;       '(("TODO" . "#008891")
;         ("DONE" . "#bbbbbb")
;         ("STARTED" . "#ff9642")
;         ("MAYBE" . "#cdc9c3")
;         ("WAITING" . "#9088d4")
;         ("BLOCKED" . "#d7385e")
;         ("PROJ" . "#7579e7")
;         ))

; ;; Image
; (setq org-image-actual-width '(650))

; ;; Block tasks when have not done subtasks
; (setq org-enforce-todo-dependencies t)

; ;; Block tasks when have not done checkobx
; (setq org-enforce-todo-checkbox-dependencies t)

; ;; Using unique ID's for links in Org-mode
; (setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id
;       org-clone-delete-id t)

; ;; org-agenda-files
; (setq org-agenda-files '(
;                          "~/Org/inbox.org"
;                          "~/Org/inbox_beorg.org"
;                          "~/Org/inbox_alfred.org"
;                          "~/Org/todo.org"
;                          "~/Org/project.org"
;                          ))

; ;; Org agenda misc settings
; (setq org-agenda-span '1)
; (setq org-agenda-start-on-weekday nil)
; (setq org-agenda-start-day "0d")
; (setq org-log-into-drawer t)
; (setq org-agenda-archives-mode t)
; (setq org-agenda-time-grid
;       (quote
;        ((daily today remove-match)
;         (0800 1000 1200 1400 1600 1800 2000 2200)
;         "......" "----------------")))

; ;; Log done date
; (setq org-log-done t)

; ;; Non-nil means switching todo states with S-cursor counts as state change.
; (setq org-treat-S-cursor-todo-selection-as-state-change nil)

; ;; Warning 30 days before deadline
; (setq org-deadline-warning-days 7)

(setq load-path (remove-if (lambda (x) (string-match-p "org$" x)) load-path))
;; Second, trick emacs into forgetting about the fact that org is
;; a "built-in" package by removing it from package--builtins.
;; Without this, package will refuse to install org, since it's
;; "already installed".
;; package--builtins is only initialized when a query needs it.
(package-built-in-p 'org)   ;; prime package--builtins
(setq package--builtins (assq-delete-all 'org package--builtins))

(use-package org
  :ensure org-plus-contrib
  :pin org
  :config
  (setq org-directory "~/Org")

  ;; When opening a file make sure everything is expanded
  (setq org-startup-folded nil)

  ;; Always wrap lines
  (setq org-startup-truncated nil)

  ;; Hide markers like /emphasis/
  (setq org-hide-emphasis-markers t)

  ;; Show inline images
  (org-display-inline-images t t)

  ;; Don't show full size images otherwise it's too large when
  ;; displaying inline
  (setq org-image-actual-width nil)

  ;; Prettify outlines
  (add-hook 'org-mode-hook
            (lambda ()
              (push '("- [ ]"     . "☐") prettify-symbols-alist)
              (push '("- [X]"     . "☑") prettify-symbols-alist)
              (prettify-symbols-mode)))

  ;; Refile to the root of a file
  (setq org-refile-use-outline-path 'file)
  (setq org-refile-targets '((org-agenda-files :level . 1)))

  ;; Show the agenda helper and viewer in split screen
  (defadvice org-agenda (around split-vertically activate)
    (let ((split-width-threshold 80))
      ad-do-it))

  (defadvice org-agenda-list (around split-vertically activate)
    (let ((split-width-threshold 80))
      ad-do-it))

  (defun org-agenda-and-todos ()
    (interactive)
    (org-agenda nil "n"))

  (defun org-agenda-and-todos-two-weeks ()
    (interactive)
    (let ((current-prefix-arg 14))
      (call-interactively 'org-agenda-and-todos)))

  (defun org-agenda-and-todos-last-week ()
    (interactive)
    (let ((current-prefix-arg -7))
      (call-interactively 'org-agenda-and-todos)))

  ;; Shortcut copy an internal link
  (global-set-key (kbd "C-c o l") 'org-store-link)

  ;; Shortcut to show preferred agenda view
  (global-set-key (kbd "C-c A") 'org-agenda-and-todos-two-weeks)

  ;; Show hours:minutes instead of days:hours
  (setq org-time-clocksum-format (quote (:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t)))

  ;; Shortcut to jump to last running clock
  (global-set-key (kbd "C-c C-x C-j") 'org-clock-jump-to-current-clock)

  ;; On startup show the agenda for the next 2 calendar weeks and all
  ;; todo items
  (add-hook 'after-init-hook 'org-agenda-and-todos-two-weeks)

  ;; Show org timestamps in 12h time
  (setq org-agenda-timegrid-use-ampm 1)

  ;; Always show the current day in org agenda
  (setq org-agenda-time-grid (quote
                              ((daily today today)
                               (800 1000 1200 1400 1600 1800 2000)
                               "......" "----------------")))

  ;; Color code the agenda based on type
  ;; http://dept.stat.lsa.umich.edu/~jerrick/org_agenda_calendar.html
  (defun color-org-header (tag col)
    (interactive)
    (goto-char (point-min))
    (while (re-search-forward tag nil t)
      (add-text-properties (match-beginning 0) (point-at-eol)
         `(face (:foreground ,col)))))

  (add-hook 'org-finalize-agenda-hook
      (lambda ()
        (save-excursion
    (color-org-header "notes:"  "#66D9EF")
    (color-org-header "refile:" "#F92672"))))

  ;; Use longtable as the default table style when exporting
  (setq org-latex-default-table-environment "longtable")

  ;; Don't position tables in the center
  (setq org-latex-tables-centered nil)

  ;; Fix some org-mode + yasnippet conflicts
  ;; http://stackoverflow.com/questions/9418148/conflicts-between-org-mode-and-yasnippet
  (defun yas/org-very-safe-expand ()
    (let ((yas/fallback-behavior 'return-nil)) (yas/expand)))

  (add-hook 'org-mode-hook
      (lambda ()
        (make-variable-buffer-local 'yas/trigger-key)
        (setq yas/trigger-key [tab])
        (add-to-list 'org-tab-first-hook 'yas/org-very-safe-expand)
        (define-key yas/keymap [tab] 'yas/next-field)))

  (add-hook 'org-mode-hook (lambda ()
                             (make-variable-buffer-local 'visual-line-mode)
                             (visual-line-mode)))

  ;; Show syntax highlighting per language native mode in *.org
  (setq org-src-fontify-natively t)
  ;; For languages with significant whitespace like Python:
  (setq org-src-preserve-indentation t)

  ;; Timestamp new todos
  (setq org-log-done 'time)
  (setq org-startup-indented t)

  ;; Agenda
  (setq org-agenda-text-search-extra-files
  '(agenda-archives
    "~/Org/notes.org_archive"))
  (define-key global-map (kbd "C-c a") 'org-agenda)
  (define-key global-map (kbd "C-c C-a") 'org-agenda)

  ;; In org agenda log view also show recurring tasks
  (setq org-agenda-log-mode-items '(closed clock state))

  (defun org-archive-done-tasks ()
    "Archive all DONE and WONT-DO tasks."
    (interactive)
    (org-map-entries
     (lambda ()
       (org-archive-subtree)
       (setq org-map-continue-from (outline-previous-heading)))
     "+TODO=\"DONE\"" 'tree)
    (org-map-entries
     (lambda ()
       (org-archive-subtree)
       (setq org-map-continue-from (outline-previous-heading)))
     "+TODO=\"WONT-DO\"" 'tree))

  ;; Capture
  (global-set-key (kbd "C-c c") 'org-capture)
  (setq org-default-notes-file "~/Org/refile.org")
  ;; Allow the creation of parent headings when refiling
  (setq org-refile-allow-creating-parent-nodes t)
  (setq org-capture-templates
  (quote (("t" "To Do" entry (file "~/Org/refile.org")
     "* TODO %?\n%U" :clock-in t :clock-resume t)
    ("n" "Note" entry (file "~/Org/refile.org")
     "* %? %T :note:\n%U\n%a\n" :clock-in t :clock-resume t)
    ("m" "Meeting" entry (file "~/Org/refile.org")
     "* Meeting w/%? %T :meeting:\n%U" :clock-in t :clock-resume t)
    ("i" "Interview" entry (file "~/Org/refile.org")
     "* Interview w/%? %T :interview:\n%U" :clock-in t :clock-resume t))))

  ;; Auto mark parent todos as done if childrend are done
  (defun org-summary-todo (n-done n-not-done)
    "Switch entry to DONE when all subentries are done, to TODO otherwise."
    (let (org-log-done org-log-states)   ; turn off logging
      (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

  ;; Refile

  ; Targets include this file and any file contributing to the agenda -
  ; up to 9 levels deep
  (setq org-refile-targets (quote ((nil :maxlevel . 9)
           (org-agenda-files :maxlevel . 9))))

  ; Use full outline paths for refile targets - we file directly with
  ; IDO
  (setq org-refile-use-outline-path t)

  ; Targets complete directly with IDO
  (setq org-outline-path-complete-in-steps nil)

  ; Allow refile to create parent tasks with confirmation
  (setq org-refile-allow-creating-parent-nodes (quote confirm))

  ; Use IDO for both buffer and file completion and ido-everywhere to t
  (setq org-completion-use-ido t)
  ; Use the current window for indirect buffer display
  (setq org-indirect-buffer-display 'current-window)

;;;; Refile settings
  (add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

  ;; Time format for clock table durations as h:mm
  (setq org-duration-format (quote h:mm))

  ;; Don't prompt for confirmation when exporting babel blocks
  (setq org-confirm-babel-evaluate nil))


;; Babel setup
; (use-package ob-python
;   :defer t
;   :commands (org-babel-execute:python)
;   :config
;   (setq org-babel-python-command "python3"))

; (use-package ob-shell
;   :defer t
;   :commands
;   (org-babel-execute:sh
;    org-babel-expand-body:sh

;    org-babel-execute:bash
;    org-babel-expand-body:bash))

(use-package htmlize 
  :ensure t)

;; Org export
(use-package ox-reveal 
  :ensure t)
(use-package ox-jira 
  :ensure t)
(use-package ox-hugo 
  :ensure t 
  :after ox)

;; Heavily modified based on https://github.com/novoid/title-capitalization.el/blob/master/title-capitalization.el
(defun title-capitalization (str)
  "Convert str to title case"
  (interactive)
  (with-temp-buffer
    (insert str)
    (let* ((beg (point-min))
           (end (point-max))
     ;; Basic list of words which don't get capitalized according to simplified rules
     ;; http://karl-voit.at/2015/05/25/elisp-title-capitalization/
           (do-not-capitalize-basic-words '("a" "ago" "an" "and" "as" "at" "but" "by" "for"
                                            "from" "in" "into" "it" "next" "nor" "of" "off"
                                            "on" "onto" "or" "over" "past" "so" "the" "till"
                                            "to" "up" "yet"
                                            "n" "t" "es" "s"))
     ;; If user has defined 'my-do-not-capitalize-words, append to basic list
           (do-not-capitalize-words (if (boundp 'my-do-not-capitalize-words)
                                        (append do-not-capitalize-basic-words my-do-not-capitalize-words )
                                      do-not-capitalize-basic-words)))
      ;; Go to begin of first word
      (goto-char beg)
      (setq continue t)

      ;; Go through the region, word by word
      (while continue
        (let ((last-point (point)))
          (let ((word (thing-at-point 'word)))
            (if (stringp word)
                ;; Capitalize current word except when it is list member
                (if (and (member (downcase word) do-not-capitalize-words)
                         ;; Always capitalize first word
                         (not (= (point) 1)))
                    (downcase-word 1)

                  ;; If it's an acronym, don't capitalize
                  (if (string= word (upcase word))
                      (progn
                        (goto-char (+ (point) (length word) 1)))
                    (capitalize-word 1)))))

          (skip-syntax-forward "^w" end)

          ;; Break if we are at the end of the buffer
          (when (= (point) last-point)
            (setq continue nil))))

      ;; Always capitalize the last word
      (backward-word 1)

      (let ((word (thing-at-point 'word)))
        (if (and (>= (point) 0)
                 (not (member (or word "s")
                              '("n" "t" "es" "s")))
                 (not (string= word (upcase word))))
            (capitalize-word 1))))

    (buffer-string)))

;; Hacks
;; 让中文也可以不加空格就使用行内格式
(setcar (nthcdr 0 org-emphasis-regexp-components) " \t('\"{[:nonascii:]")
(setcar (nthcdr 1 org-emphasis-regexp-components) "- \t.,:!?;'\")}\\[[:nonascii:]")
(org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)
(org-element-update-syntax)
;; 规定上下标必须加 {}，否则中文使用下划线时它会以为是两个连着的下标
(setq org-use-sub-superscripts "{}")

;; Capture images
(use-package org-download
  :init
  (setq org-download-heading-lvl nil)
  (setq org-download-image-dir "~/Desktop/shotcuts")
  (setq org-download-image-org-width 400)
  (setq org-download-image-html-width 400))

;; Org roam
;; These are specified so they can be dynamically configured
;; by calling emacs in batch mode in a CI context
(setq org-roam-notes-path "~/Org/notes")
(setq org-roam-publish-path "~/Org/notes_roam")

(use-package org-roam
  :ensure t
  :hook
  ((after-init . org-roam-mode)
   ;; Need to add advice after-init otherwise they won't take
   (after-init . (lambda ()
                   (advice-add 'org-roam-capture
                               :after
                               'my/note-taking-init)

                   (advice-add 'org-roam-dailies-today
                               :after
                               'my/note-taking-init)

                   (advice-add 'org-roam-find-file
                               :after
                               'my/note-taking-init))))
  :custom
  (org-roam-directory org-roam-notes-path)
  :init
  ;; These functions need to be in :init otherwise they will not be
  ;; callable in an emacs --batch context which for some reason
  ;; can't be found in autoloads if it's under :config
  (defun my/org-roam--extract-note-body (file)
    (with-temp-buffer
      (insert-file-contents file)
      (org-mode)
      (first (org-element-map (org-element-parse-buffer) 'paragraph
               (lambda (paragraph)
                 (let ((begin (plist-get (first (cdr paragraph)) :begin))
                       (end (plist-get (first (cdr paragraph)) :end)))
                   (buffer-substring begin end)))))))

  ;; Include backlinks in org exported notes not tagged as private or
  ;; draft
  (defun my/org-roam--backlinks-list (file)
    (if (org-roam--org-roam-file-p file)
        (--reduce-from
         (concat acc (format "- [[file:%s][%s]]\n#+begin_quote\n%s\n#+end_quote\n"
                             (file-relative-name (car it) org-roam-directory)
                             (title-capitalization (org-roam-db--get-title (car it)))
                             (my/org-roam--extract-note-body (car it))))
         ""
         (org-roam-db-query
          [:select :distinct [links:source]
                   :from links
                   :left :outer :join tags :on (= links:source tags:file)
                   :where (and (= dest $s1)
                               (or (is tags:tags nil)
                                   (and
                                    (not-like tags:tags '%private%)
                                    (not-like tags:tags '%draft%))))]
          file))
      ""))

  (defun file-path-to-md-file-name (path)
    (let ((file-name (first (last (split-string path "/")))))
      (concat (first (split-string file-name "\\.")) ".md")))

  (defun file-path-to-slug (path)
    (let* ((file-name (car (last (split-string path "--"))))
           (title (first (split-string file-name "\\."))))
      (replace-regexp-in-string (regexp-quote "_") "-" title nil 'literal)))

  ;; Fetches all org-roam files and exports to hugo markdown
  ;; files. Adds in necessary hugo properties
  ;; e.g. HUGO_BASE_DIR. Ignores notes tagged as private or draft
  (defun org-roam-to-hugo-md ()
    (interactive)
    ;; Make sure the author is set
    (setq user-full-name "Alex Kehayias")

    (let ((files (mapcan
                  (lambda (x) x)
                  (org-roam-db-query
                   [:select [files:file]
                            :from files
                            :left :outer :join tags :on (= files:file tags:file)
                            :where (or (is tags:tags nil)
                                       (and
                                        (not-like tags:tags '%private%)
                                        (not-like tags:tags '%draft%)))]))))
      (mapc
       (lambda (f)
         ;; Use temporary buffer to prevent a buffer being opened for
         ;; each note file.
         (with-temp-buffer
           (message "Working on: %s" f)
           (insert-file-contents f)

           (goto-char (point-min))
           ;; Add in hugo tags for export. This lets you write the
           ;; notes without littering HUGO_* tags everywhere
           ;; HACK:
           ;; org-export-output-file-name doesn't play nicely with
           ;; temp buffers since it attempts to get the file name from
           ;; the buffer. Instead we explicitely add the name of the
           ;; exported .md file otherwise you would get prompted for
           ;; the output file name on every note.
           (insert
            (format "#+HUGO_BASE_DIR: %s\n#+HUGO_SECTION: ./\n#+HUGO_SLUG: %s\n#+EXPORT_FILE_NAME: %s\n"
                    org-roam-publish-path
                    (file-path-to-slug f)
                    (file-path-to-md-file-name f)))

           ;; If this is a placeholder note (no content in the
           ;; body) then add default text. This makes it look ok when
           ;; showing note previews in the index and avoids a headline
           ;; followed by a headline in the note detail page.
           (if (eq (my/org-roam--extract-note-body f) nil)
               (progn
                 (goto-char (point-max))
                 (insert "\n/This note does not have a description yet./\n")))

           ;; Add in backlinks because
           ;; org-export-before-processing-hook won't be useful the
           ;; way we are using a temp buffer
           (let ((links (my/org-roam--backlinks-list f)))
             (unless (string= links "")
               (goto-char (point-max))
               (insert (concat "\n* Links to this note\n") links)))

           (org-hugo-export-to-md)))
       files)))
  :bind (:map org-roam-mode-map
              (("C-c n l" . org-roam)
               ("C-c n f" . org-roam-find-file)
               ("C-c n g" . org-roam-graph)
               ("C-c n c" . org-roam-capture)
               ("C-c n j" . org-roam-dailies-today)
               ("C-c n r" . org-roam-random-note)
               ("C-c n u" . org-roam-unlinked-references)
               ("C-c n e" . org-roam-to-hugo-md)
               ;; Full text search notes with an action to insert
               ;; org-mode link
               ("C-c n s" . helm-rg))
              :map org-mode-map
              (("C-c n i" . org-roam-insert)
               ("C-c n I" . org-roam-insert-immediate))))

  :config
  (setq org-roam-capture-templates
  (quote (("d" "Default" plain (function org-roam--capture-get-point)
                 "%?"
                 :file-name "%(format-time-string \"%Y-%m-%d--%H-%M-%SZ--${slug}\" (current-time) t)"
                 :head "#+TITLE: ${title}\n#+DATE: %<%Y-%m-%d>\n#+ROAM_ALIAS:\n#+ROAM_TAGS:\n\n"
                 :unnarrowed t))))

  (setq org-roam-dailies-capture-templates
  (quote (("d" "Default" plain (function org-roam--capture-get-point)
                 "%?"
                 :file-name "%(format-time-string \"%Y-%m-%d--%H-%M-%SZ--journal\" (current-time) t)"
                 :head "#+TITLE: Journal %<%Y-%m-%d>\n#+DATE: %<%Y-%m-%d>\n#+ROAM_ALIAS:\n#+ROAM_TAGS: private journal\n\n"
                 :unnarrowed t))))

  (setq org-roam-completion-system 'helm)

  ;; Use writeroom mode when capturing new notes. Hide the ugly
  ;; preamble of org attributes by scrolling up.
  (defun my/note-taking-init (&rest r)
    (with-current-buffer (current-buffer)
      (writeroom-mode)
      (scroll-up-command 4)))

(use-package company-org-roam
  :ensure t
  :config
  (push 'company-org-roam company-backends))

(provide 'init-org)
