;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys

;; Global changes

(setq fixed-font-size (if (eq system-type 'darwin) 16 22)
      variable-font-size (if (eq system-type 'darwin) 20 26)
      fancy-splash-image (concat doom-user-dir "splash-small.png")
      user-full-name "Dan Valentine"
      user-mail-address "danvalentine256@gmail.com"
      doom-font (font-spec :family "JetBrains Mono" :size fixed-font-size)
      doom-variable-pitch-font (font-spec :family "ETBembo" :size variable-font-size)
      hl-todo-wrap-movement t
      doom-theme 'doom-gruvbox
      display-line-numbers-type 'relative
      select-enable-clipboard t
      select-enable-primary t
      line-spacing 0.05
      org-directory "~/org/")
(auto-save-visited-mode +1)
(+global-word-wrap-mode +1)
(+word-wrap-mode 1)

(undefine-key! doom-leader-map "SPC SPC")
(setq doom-localleader-key "SPC SPC")
(setq doom-localleader-alt-key "M-SPC SPC")

(defun clear-trailing-whitespace-and-append-newline () ""
       (delete-trailing-whitespace)
       (goto-char (point-max))
       (+evil/insert-newline-below 1)
       )
(defun dan-save-and-close-buffer ()
  "Save and close the current buffer."
  (interactive)
  (save-buffer)
  (kill-current-buffer))


(defun open-dirvish-home-dwim ()
  "Open Dirvish in the home directory using DWIM behavior."
  (interactive)
  (dirvish-dwim "~"))

(defun open-dirvish-home-fullscreen ()
  "Open Dirvish in the home directory in fullscreen."
  (interactive)
  (dirvish "~" ))

(defun open-dirvish-projects-dwim ()
  "Open Dirvish in the projects directory using DWIM behavior."
  (interactive)
  (dirvish-dwim "~/projects"))

(defun open-dirvish-projects-fullscreen ()
  "Open Dirvish in the projects directory in fullscreen."
  (interactive)
  (dirvish "~/projects" ))

(defun open-dirvish-org-dwim ()
  "Open Dirvish in the org directory using DWIM behavior."
  (interactive)
  (dirvish-dwim "~/org"))

(defun open-dirvish-org-fullscreen ()
  "Open Dirvish in the org directory in fullscreen."
  (interactive)
  (dirvish "~/org" ))

;; Package specific changes
(after! org-download
  (setq org-download-image-dir "~/Dropbox/org/zassets/org_download/")
  ;; (setq org-download-annotate-function (defun empty-annotate (link) "" ""))
  )

(after! apheleia
  (add-to-list 'apheleia-mode-alist '(python-mode . ruff))
  (add-to-list 'apheleia-mode-alist '(python-ts-mode . ruff)))

(after! pet
  (add-hook 'python-base-mode-hook 'pet-mode -10)
  )


(after! which-key
  (setq which-key-idle-delay 0.2)
  )

(after! projectile
  (setq projectile-project-search-path '("~/projects/"  ))
  )

(after! flycheck

  (setq flycheck-python-ruff-executable "ruff")
  )


(after! python
  (add-hook 'python-mode-hook 'code-cells-mode)
  (map! :map python-mode-map
        :localleader
        "x" #'python-pytest
        )

  )



(after! vterm

  ;; (set-face-attribute 'vterm-color-black nil :background "#000000" :foreground "#303030") # does not work
  (defun dan/set-terminal-cursor ()
    (setq cursor-type 'bar))

  (add-hook 'vterm-mode-hook 'dan/set-terminal-cursor)
  (setq vterm-shell (if (eq system-type 'darwin) "/bin/zsh" "/usr/bin/zsh"))

  (map! :map vterm-mode-map
        :i  "C-v"   #'vterm-yank
        :n  "p"   #'vterm-yank
        )
  )

(after! code-cells
  (defun dan-jupyter-eval-region (beg end)
    (jupyter-eval-region nil beg end))
  ;; (setq code-cells-convert-ipynb-style '(("pandoc" "--to" "ipynb" "--from" "org")
  ;;                                        ("pandoc" "--to" "org" "--from" "ipynb")
  ;;                                        (lambda () #'org-mode)))
  (add-to-list 'auto-mode-alist '("\\.ipynb\\'" . python-mode))
  (add-to-list 'code-cells-eval-region-commands '(org-mode . python-shell-send-region))
  (add-to-list 'code-cells-eval-region-commands '(jupyter-repl-interaction-mode . dan-jupyter-eval-region))
  (setq jupyter-repl-echo-eval-p t)
  (map!
   :map code-cells-mode-map
   :nm "]c" #'code-cells-forward-cell
   :nm "[c" #'code-cells-backward-cell
   :nmiv "<C-return>" #'code-cells-eval
   :nmiv "<C-RET>" #'code-cells-eval
   )
  (map! :map code-cells-mode-map
        :localleader
        "b" #'code-cells-eval-above
        "c" #'code-cells-comment-or-uncomment
        )
  )

;; could not figure out how to integrate ruff into the lsp diagnostics directly, so it's just a separate checker in flycheck
(after! (:all lsp-mode flycheck lsp-pyright)
  (add-hook 'python-mode-hook (lambda () (flycheck-add-next-checker 'lsp 'python-ruff ))))

(after! evil-snipe
  ;; (setq evil-snipe-scope 'visible)
  ;; (setq evil-snipe-spillover-scope 'whole-visible)
  ;; (setq evil-snipe-repeat-scope 'whole-visible)
  )
(after! dirvish
  (set-popup-rule! "^ ?\\*Dirvish.*" :ignore t)
  (map! :map dirvish-mode-map
        :n  "?"   #'dirvish-dispatch
        :n  "q"   #'dirvish-quit
        :ng "a"   #'dirvish-quick-access
        :ng "h"    #'evil-avy-goto-char-2
        :ng "j" #'evil-next-line
        :ng "k" #'evil-previous-line
        :ng "f"   #'dirvish-file-info-menu
        :ng "y"   #'dirvish-yank-menu
        :ng "s"   #'dirvish-quicksort
        :ng "<left>" #'dired-up-directory
        :ng "<right>" #'dired-find-file
        :ng "-" #'dired-create-empty-file
        :ng "+" #'dired-create-directory
        :ng "TAB" #'dirvish-subtree-toggle
        :ng "M-t" #'dirvish-layout-toggle
        :ng "M-n" #'dirvish-narrow
        :ng "M-m" #'dirvish-mark-menu
        :ng "M-s" #'dirvish-setup-menu
        )
  (setq recentf-exclude (append recentf-exclude '("/$")))
  (setq display-line-numbers-type 'relative)
  (setq dirvish-mode-line-format
        '(:left (sort symlink) :right (omit yank index)))
  (setq dirvish-attributes
        '(nerd-icons file-time collapse subtree-state vc-state))
  (add-hook 'dired-mode-hook
            (lambda ()
              (dired-hide-details-mode)
              (display-line-numbers-mode )
              ))
  (setq delete-by-moving-to-trash t)
  (setq dired-kill-when-opening-new-dired-buffer t)
  (setq dired-listing-switches
        "-l --almost-all --human-readable --group-directories-first --no-group -g")
  (setq dired-omit-files "\\`[.]?#\\|\\`[.][.]?\\'\\|\\`[.]")
  (setq dired-omit-files
        (concat dired-omit-files
                "\\|^.DS_Store$"
                "\\|^__pycache__$"))

  )
(after! org-journal
  (setq org-journal-dir "~/org/journal")
  (setq org-journal-file-format "%Y-%m-%d.org")
  (setq org-extend-today-until 3)
  (setq org-journal-date-format "%A, %e %B %Y")
  (setq org-journal-time-format "%R ")
  (setq org-journal-find-file-fn 'find-file)
  (setq org-journal-hide-entries-p nil)
  (setq org-journal-file-header "")
  (setq org-journal-carryover-items nil)
  (add-hook 'org-journal-after-header-create-hook 'dan-add-id-to-journal-entry)
  )

(after! (:all org-modern org)
  org-modern-star 'replace
  org-modern-list
  '((?+ . "◦")
    (?- . "•")
    (?* . "•"))
  (add-hook 'org-mode-hook #'org-modern-mode)
  )

(after! org-fancy-priorities
  (add-hook 'org-mode-hook #'org-fancy-priorities-mode)
  (setq    org-fancy-priorities-list '("‼️" "❗" "⚠️"))
  )

(after! org

  (defun insert-date-org-tree ()
    "Insert a new org-mode tree below the current tree at the same level, with the current date."
    (interactive)
    (let* ((current-level (org-current-level))
           (asterisks (if current-level
                          (make-string current-level ?*)
                        "*"))
           (current-tree-end (save-excursion
                               (if current-level
                                   (org-end-of-subtree t t)
                                 (point-at-eol)))))
      (goto-char current-tree-end)
      (newline)
      (insert asterisks " ")
      (insert (format-time-string "%A, %e %B %Y"))
      (newline)))

  (defun insert-time-org-tree ()
    "Insert a new org-mode tree below the current tree at the same level, with the current time."
    (interactive)
    (let* ((current-level (org-current-level))
           (asterisks (if current-level
                          (make-string current-level ?*)
                        "*"))
           (current-tree-end (save-excursion
                               (if current-level
                                   (org-end-of-subtree t t)
                                 (point-at-eol)))))
      (goto-char current-tree-end)
      (newline)
      (insert asterisks " ")
      (insert (format-time-string "%H:%M"))
      (newline)
      (insert "- ")
      (evil-append 1)
      ))

  (defun get-org-files-in-directories (dirs)
    "Return a list of all .org files in DIRS and their subdirectories."
    (mapcan (lambda (dir)
              (directory-files-recursively dir "\\.org$"))
            dirs))

  (defun dan-capture-log (prefix) "" (interactive "P")
         (org-journal-new-entry prefix)
         (clear-trailing-whitespace-and-append-newline)
         (goto-char (point-max))
         (insert "- ")
         (evil-append 1)
         )

  (defun dan-capture-task () "" (interactive)
         (org-journal-new-entry t)
         (clear-trailing-whitespace-and-append-newline)
         (goto-char (point-max))
         (insert "** TODO ")
         (evil-append 1)
         )

  (add-hook 'org-mode-hook 'org-download-enable)

  (setq warning-suppress-types (append warning-suppress-types '((org-element)))) ;; disabling this warning because it appears any time an org buffer auto saves in insert mode when there is a trailing space before the cursor. does not seem to cause problems.
  (setq
   org-ellipsis "…"
   org-image-actual-width 600
   org-startup-folded 'showeverything
   org-hide-emphasis-markers t
   org-startup-with-inline-images t
   org-enforce-todo-dependencies nil
   org-pretty-entities t
   org-archive-location "~/org/zarchive/archive.org::* From %s"
   org-agenda-tags-column 0
   org-agenda-block-separator ?─
   org-agenda-time-grid
   '((daily today require-timed)
     (800 1000 1200 1400 1600 1800 2000)
     " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
   org-agenda-current-time-string
   "◀── now ─────────────────────────────────────────────────"
   org-todo-keywords
   '((sequence "TODO(t)" "PROJ(p)" "|" "DONE(d)" "SOMEDAY(s)" "CANCELLED(c)")
     )
   )

  (let* (
         (base-font-color     (face-foreground 'default nil 'default))
         (headline           `(:inherit default :weight bold :foreground ,base-font-color)))

    (custom-theme-set-faces
     'user
     '(org-level-1 ((t (:inherit outline-1 :height 1.8))))
     '(org-level-2 ((t (:inherit outline-2 :height 1.4))))
     '(org-level-3 ((t (:inherit outline-3 :height 1.3))))
     '(org-level-4 ((t (:inherit outline-4 :height 1.2))))
     '(org-level-5 ((t (:inherit outline-5 :height 1.0))))
     )
    )

  (add-hook 'org-mode-hook #'variable-pitch-mode)
  (add-hook 'org-mode-hook #'visual-line-mode)
  (add-hook 'org-mode-hook #'org-super-agenda-mode)
  (add-hook 'org-agenda-finalize-hook #'org-modern-agenda)
  (custom-theme-set-faces
   'user
   '(org-block ((t (:inherit fixed-pitch))))
   '(evil-goggles-default-face ((t (:inherit 'highlight))))
   '(line-number ((t (:inherit fixed-pitch))))
   '(line-number-current-line ((t (:inherit fixed-pitch))))
   '(org-code ((t (:inherit (shadow fixed-pitch)))))
   ;; '(org-document-info ((t (:foreground "dark orange"))))
   '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
   '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
   ;; '(org-drawer ((t (:inherit font-lock-comment-face))))
   '(org-drawer ((t (:foreground "#5D564E"))))
   '(org-document-title ((t (:foreground "#5D564E"))))
   '(org-modern-priority ((t (:inherit (fixed-pitch)))))
   ;; '(org-link ((t (:foreground "royal blue" :underline t))))
   ;; '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
   '(org-property-value ((t (:inherit fixed-pitch))) t)
   '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
   ;; '(org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
   '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
   '(org-verbatim ((t (:inherit (shadow fixed-pitch)))))
   '(org-ql-view-due-date ((t (:foreground "#83a598"))))
   '(org-super-agenda-header ((t (:inherit (outline-1) :height 1.2))))
   )

  (setq task-files (append (get-org-files-in-directories '("~/org/projects" "~/org/journal" "~/org/notes"))))
  (setq project-files (get-org-files-in-directories '("~/org/projects")))

  (setq org-refile-targets
        (append
         (mapcar (lambda (file) (cons file '(:maxlevel . 3)))
                 project-files)
         '(("~/org/zarchive/completed_todos.org" . (:maxlevel . 3)))))

  (setq org-agenda-files task-files)

  (defun dashboard-inbox ()
    ""
    (interactive)
    (org-ql-search task-files
      ;; `(or
      `(and (todo) (not (scheduled :from 1)) (todo "TODO" "PROJ") (not (path "someday"))
            (or
             (or (path "journal") (path "inbox"))
             ))

      :title "Inbox"
      :super-groups '(
                      (:name "📥 Inbox" :file-path "inbox" :file-path "journal")
                      )
      ))
  (defun dashboard-today ()
    ""
    (interactive)
    (org-ql-search task-files
      `(and (todo) (not (scheduled :from 1)) (todo "TODO" "PROJ") (not (path "someday"))
            (or
             (tags "next")
             (or (path "journal") (path "inbox"))
             (deadline auto)
             (scheduled :to today)))

      :title "Today"

      :super-groups '((:auto-outline-path t))
      ))

  (defun dashboard-active-tasks ()
    "Display active tasks grouped by filename."
    (interactive)
    (org-ql-search task-files
      `(and (todo) (not (scheduled :from 1)) (not (path "someday")) ) ; from can take number of days relative to today, eg 1
      :title "Active Tasks"
      :super-groups '((:auto-outline-path t))))


  (defun dashboard-orphans ()
    ""
    (interactive)
    (org-ql-search task-files
      `(and (todo)
            (not (path "projects"))
            (not (tags "next"))
            )
      :title "Orphans"
      )))





(after! evil
  (evil-define-key 'normal global-map (kbd "SPC SPC")
    (lambda () (interactive)
      (execute-kbd-macro (kbd "SPC m"))))
  ;; Navigate by displayed lines
  (map! :nm "k" #'evil-previous-visual-line)
  (map! :nm "j" #'evil-next-visual-line)
  (map! :nmv "h" #'evil-avy-goto-char-2)

  ;; Window navigation
  (map! :nm "<C-left>" #'evil-window-left)
  (map! :nm "<C-down>" #'evil-window-down)
  (map! :nm "<C-up>" #'evil-window-up)
  (map! :nm "<C-right>" #'evil-window-right)
  (map! :nm "C-j" #'evil-window-down)
  (map! :nm "C-h" #'evil-window-left)
  (map! :nm "C-k" #'evil-window-up)
  (map! :nm "C-l" #'evil-window-right)
  (map! :nm "C-+" #'evil-window-increase-height)
  (map! :nm "C--" #'evil-window-decrease-height)
  (map! :nm "C->" #'evil-window-increase-width)
  (map! :nm "C-<" #'evil-window-decrease-width)
  (map! :map evil-insert-state-map
        "C-v" #'clipboard-yank)
  (map! :n "C-t t" nil)


  (map! :leader
        :desc "Split window below" "-" #'+evil/window-split-and-follow
        :desc "Split window right" "|" #'+evil/window-vsplit-and-follow
        :desc "Local" "SPC" nil

        :desc "M-x"       ";"   nil
        :desc "M-x"                   ":"    nil
        :desc "Pop up scratch buffer" "x"    #'doom/open-scratch-buffer
        :desc "Org Capture"           "X"    nil
        :desc "Capture log"           "l"    #'dan-capture-log
        :desc "Capture task"           "="    #'dan-capture-task
        :desc "Universal argument"    "u"    #'universal-argument
        :desc "window"                "w"    evil-window-map
        :desc "help"                  "h"    help-map

        :desc "Toggle last popup"     "~"    nil
        :desc "Toggle last popup"     "`"    #'+popup/toggle
        :desc "Terminal toggle"             "."    nil
        :desc "Terminal toggle"             "."    #'+vterm/toggle
        :desc "Repeat last macro" ","    nil
        :desc "Repeat last macro" ","    #'evil-execute-last-recorded-macro
        :desc "Switch buffer"        "<"        nil
        :desc "Switch to last buffer" "\\"    #'evil-switch-to-windows-last-buffer
        :desc "Resume last search"    "'" #'vertico-repeat

        :desc "Search for symbol in project" "*" nil
        :desc "Search project"               "/" #'+default/search-project
        :desc "Jump to bookmark"      "RET"  #'bookmark-jump

        ;; overriding the doom keymap is super fucky. this approach of nilling out the existing group and defining my own only works if i change the name of the group when defining my own, otherwise the nilling does not take.
      ;;; <leader> TAB --- workspace
        "a" nil
        "TAB" nil
      ;;; <leader> b --- buffer
        "b" nil
        (:prefix-map ("b" . "Buffer")
         :desc "All buffers"                 "a"   #'ibuffer
         :desc "Kill buffer"                 "d"   #'kill-current-buffer
                                        ; TODO move to a bookmark group?
         :desc "Set bookmark"                "m"   #'bookmark-set
         :desc "Delete bookmark"             "M"   #'bookmark-delete
         :desc "New empty buffer"            "n"   #'evil-buffer-new
         :desc "Revert buffer"               "R"   #'revert-buffer
         :desc "Rename buffer"               "r"   #'rename-buffer
         :desc "Save buffer"                 "s"   #'basic-save-buffer
         :desc "Save buffer as root"         "S"   #'doom/sudo-save-buffer
         :desc "Widen buffer" "w" #'widen
         :desc "Switch to scratch buffer"    "X"   #'doom/switch-to-scratch-buffer
         :desc "Yank buffer"                 "y"   #'+default/yank-buffer-contents
         )

      ;;; <leader> c --- code
        "c" nil
        (:prefix-map ("c" . "Code")
                     (:when (and (modulep! :tools lsp) (not (modulep! :tools lsp +eglot)))
                       ;; pyright doesn't really have code actions but other langs/lsps might
                       (:when (modulep! :completion vertico)
                         ;; maybe this should live in search group
                         :desc "Jump to symbol in workspace" "y"   #'consult-lsp-symbols)
                       :desc "LSP Rename"                          "r"   #'lsp-rename)

                     :desc "Restart LSP" "l" #'lsp-workspace-restart
                     :desc "Format buffer/region"                  "f"   #'+format/region-or-buffer
                     :desc "List errors"                           "x"   #'+default/diagnostics
                     :desc "List errors (flycheck)" "e" #'consult-flycheck
                     ;;TODO: this function is bad, but the idea is good. Is there another way to do this with consult?
                     ;; :desc "List project errors" "E" #'lsp-ui-flycheck-list
                     :desc "Start jupyter repl" "j" #'jupyter-run-repl
                     :desc "Attach buffer to jupyter repl" "p" #'jupyter-repl-associate-buffer
                     :desc "Activate virtual environment" "v" #'pyvenv-activate
                     )


        ;; <leader> d --- directory
        "d" nil
        (:prefix-map ("d" . "Directory")
         :desc "Dirvish-fd" "f" #'dirvish-fd
         :desc "Dirvish here" "d" #'dirvish-dwim
         :desc "Dirvish here fullscreen" "D" #'dirvish
         :desc "Dirvish projects" "p" #'open-dirvish-projects-dwim
         :desc "Dirvish projects-fullscreen" "P" #'open-dirvish-projects-fullscreen
         :desc "Dirvish org" "o" #'open-dirvish-org-dwim
         :desc "Dirvish org fullscreen" "O" #'open-dirvish-org-fullscreen
         :desc "Dirvish home" "h" #'open-dirvish-home-dwim
         :desc "Dirvish home fullscreen" "H" #'open-dirvish-home-fullscreen

         )

      ;;; <leader> f --- file
        "f" nil
        (:prefix-map ("f" . "File")
         "c" nil
         :desc "Find file in private config" "c"   #'doom/find-file-in-private-config
         :desc "Copy this file"              "C"   #'doom/copy-this-file
         :desc "Delete this file"            "d"   #'doom/delete-this-file
         :desc "Find file in project"                   "f"   #'projectile-find-file
         :desc "Find file"                   "F"   #'find-file
         :desc "Recent files"                "r"   #'recentf-open-files
         :desc "Rename/move file"            "m"   #'doom/move-this-file
         :desc "Save file"                   "s"   #'save-buffer
         :desc "Save file as..."             "S"   #'write-file
         :desc "Save and kill buffer"        "q"   #'dan-save-and-close-buffer
         :desc "Sudo this file"              "u"   #'doom/sudo-this-file
         :desc "Yank file path"              "y"   #'+default/yank-buffer-path
         :desc "Yank file path from project" "Y"   #'+default/yank-buffer-path-relative-to-project
         )

        ;;; <leader> g --- git/version control - Currently unused
        "g" nil

      ;;; <leader> i --- insert
        "i" nil
        (:prefix-map ("i" . "Insert")
         :desc "Emoji"                       "e"   #'emoji-search
         :desc "From evil register"            "r"   #'evil-show-registers
         :desc "Unicode"                       "u"   #'insert-char
         :desc "From clipboard"                "y"   #'+default/yank-pop)

      ;;; <leader> n --- notes
        "n" nil
        (:prefix-map ("n" . "Notes")
         :desc "Open calendar"            "c" #'calendar
         :desc "Active tasks"                   "a" #'dashboard-active-tasks
         :desc "Find file in notes"           "f" #'+default/find-in-notes
         :desc "Today agenda"                    "t" #'dashboard-today
         :desc "Inbox agenda"                    "i" #'dashboard-inbox
         :desc "Orphans"                    "o" #'dashboard-orphans
         :desc "Search notes"                 "s" #'+default/org-notes-search
         "y" nil
         :desc "Goto today"                "y" #'org-journal-open-current-journal-file
         )

      ;;; <leader> o --- open
        "o" nil
        (:prefix-map ("o" . "Open")
         :desc "New frame"          "f"  #'make-frame
         :desc "Select frame"       "F"  #'select-frame-by-name
         (:when (modulep! :term vterm)
           :desc "Toggle vterm popup"    "t" #'+vterm/toggle
           :desc "Open vterm here"       "T" #'+vterm/here)
         (:when (modulep! :os macos)
           :desc "Reveal in Finder"           "o" #'+macos/reveal-in-finder
           :desc "Reveal project in Finder"   "O" #'+macos/reveal-project-in-finder))

      ;;; <leader> p --- project
        "p" nil
        (:prefix-map ("p" . "Project")
         :desc "Browse project"               "." #'project-dired
         :desc "Add new project"              "a" #'projectile-add-known-project
         :desc "Switch to project buffer"     "b" #'projectile-switch-to-buffer
         :desc "Remove known project"         "d" #'projectile-remove-known-project
         :desc "Discover projects in folder"  "A" #'+default/discover-projects
         :desc "Switch project"               "p" #'projectile-switch-project
         :desc "Find recent project files"    "r" #'projectile-recentf
         :desc "Save project files"           "s" #'projectile-save-project-buffers
         :desc "Test project"                 "t" #'projectile-test-project
         )

      ;;; <leader> q --- quit/session
        "q" nil
        (:prefix-map ("q" . "Quit/session")
                                        ;TODO move to frame group
         :desc "Delete frame"                 "f" #'delete-frame
         :desc "Clear current frame"          "F" #'doom/kill-all-buffers
         :desc "Kill Emacs (and daemon)"      "k" #'save-buffers-kill-emacs
         :desc "Quit Emacs"                   "q" #'save-buffers-kill-terminal
         :desc "Quit Emacs without saving"    "Q" #'evil-quit-all-with-error-code
         :desc "Quick save current session"   "s" #'doom/quicksave-session
         :desc "Restore last session"         "l" #'doom/quickload-session
         :desc "Restart & restore Emacs"      "r" #'doom/restart-and-restore
         :desc "Restart Emacs"                "R" #'doom/restart)

      ;;; <leader> s --- search
        "s" nil
        (:prefix-map ("s" . "Search")
         :desc "Search buffer"                "b"  #'+default/search-buffer
         :desc "Search all open buffers"      "B" (cmd!! #'consult-line-multi 'all-buffers)
         :desc "Search current directory"     "d" #'+default/search-cwd
         :desc "Jump to symbol"               "i" #'imenu
         :desc "Jump to link"                 "l" #'ffap-menu
         :desc "Jump list"                    "j" #'evil-show-jumps
         :desc "Look up online"               "o" #'+lookup/online
         :desc "Look up in local docsets"     "k" #'+lookup/in-docsets ; TODO: does not work
         :desc "Jump to mark"                 "r" #'evil-show-marks
         :desc "Search buffer for thing at point" "s" #'+vertico/search-symbol-at-point
         :desc "Undo history"                 "u"
         (cond ((modulep! :emacs undo +tree)     #'undo-tree-visualize)
               ((modulep! :emacs undo)           #'vundo)))

      ;;; <leader> t --- toggle
        "t" nil
        (:prefix-map ("t" . "Toggle")
         :desc "Big mode"                     "b" #'doom-big-font-mode
         :desc "Frame fullscreen"             "f" #'toggle-frame-fullscreen
         :desc "Flycheck"                      "e" #'flycheck-mode
         :desc "Read-only mode"               "r" #'read-only-mode
         :desc "Visible mode"                 "v" #'visible-mode
         )

        ;; <leader> w --- windows
        ;; these are defined in emacs/modules/editor/evil/config.el
        "w" nil
        (:prefix-map ("w" . "Window")
         :desc "Delete window" "q" #'evil-window-delete
         :desc "Balance windows" "=" #'balance-windows
         :desc "Maximize window" "m" #'doom/window-maximize-buffer
         :desc "Enlarge window" "e" #'doom/window-enlargen
         :desc "Undo window layout change" "u" #'winner-undo
         :desc "Rotate windows" "r" #'evil-window-rotate-downwards
         :desc "Rotate windows reverse" "R" #'evil-window-rotate-upwards)

        ;; <leader> h --- help
        "h" nil
        (:prefix-map ("h" . "Help")
         :desc "Emacs manual" "RET" #'info-emacs-manual
         :desc "Describe char" "'" #'describe-char
         :desc "Browse Info" "i" #'info-other-window
         :desc "Apropos" "a" #'apropos
         :desc "Apropos documentation" "A" #'apropos-documentation
         :desc "View messages" "e" #'view-echo-area-messages
         :desc "Describe function" "f" #'describe-function
         :desc "Describe key" "k" #'describe-key
         :desc "View lossage" "l" #'view-lossage
         :desc "Describe mode" "m" #'describe-mode
         :desc "Describe active modes" "M" #'doom/describe-active-minor-mode
         :desc "Describe symbol" "o" #'describe-symbol
         :desc "Describe package" "p" #'doom/describe-package
         :desc "Find library" "P" #'find-library
         :desc "Find manual" "R" #'info-display-manual
         :desc "Load theme" "t" #'load-theme
         :desc "Toggle emacs profiler" "T" #'doom/toggle-profiler
         :desc "Describe variable" "v" #'describe-variable
         :desc "Man page" "w" #'man
         :desc "List processes" "c" #'list-processes
         (:prefix ("b" . "Bindings")
          :desc "Describe binding" "b"   #'describe-bindings
          :desc "WK minor mode" "i"   #'which-key-show-minor-mode-keymap
          :desc "WK major mode" "m"   #'which-key-show-major-mode
          :desc "WK top level" "t"   #'which-key-show-top-level
          :desc "WK full keymap" "f"   #'which-key-show-full-keymap
          :desc "WK keymap" "k"   #'which-key-show-keymap
          )
         (:prefix ("d" . "Doom")
                  "d"   #'doom-debug-mode
                  "h"   #'doom/help
                  "m"   #'doom/help-modules
                  )
         :desc "Reload private config" "r"   #'doom/reload
         )
        )
  (map!
   ;; mine
   :n "]x" #'next-error
   :n "[x" #'previous-error
   :n "]e" #'flycheck-next-error
   :n "[e" #'flycheck-previous-error
   :n "]g" #'+vc-gutter/next-hunk
   :n "[g" #'+vc-gutter/previous-hunk


   ;;evil/doom
   :nmv  "[s"   nil
   :nmv  "]s"   nil
   ;; ported from vim-unimpaired
   :n  "] SPC" #'+evil/insert-newline-below
   :n  "[ SPC" #'+evil/insert-newline-above
   :n  "]b"    #'next-buffer
   :n  "[b"    #'previous-buffer
   :n  "]f"    #'+evil/next-file
   :n  "[f"    #'+evil/previous-file
   :m  "]u"    nil
   :m  "[u"    nil
   :m  "]y"    nil
   :m  "[y"    nil
   :m "]x"   nil
   :m "[x"   nil
   :m "]d"   nil
   :m "[d"   nil

   :m "]t"   #'hl-todo-next
   :m "[t"   #'hl-todo-previous
   :n "gt"   nil
   :n "gT"   nil
   :n "]w"   nil
   :n "[w"   nil
   :n "] TAB"   nil
   :n "[ TAB"  nil

   ;; custom vim-unmpaired-esque keys
   :m  "]#"    nil
   :m  "[#"    nil
   :m  "]a"    nil
   :m  "[a"    nil
   :m  "]c"    #'+evil/next-comment
   :m  "[c"    #'+evil/previous-comment
   :m  "]e"    nil
   :m  "[e"    nil
   :n  "]r"    #'+evil/next-frame
   :n  "[r"    #'+evil/previous-frame
   :m  "]h"    #'outline-next-visible-heading
   :m  "[h"    #'outline-previous-visible-heading
   :m  "]m"    #'+evil/next-beginning-of-method
   :m  "[m"    #'+evil/previous-beginning-of-method
   :m  "]M"    #'+evil/next-end-of-method
   :m  "[M"    #'+evil/previous-end-of-method
   :n  "[o"    nil
   :n  "]o"    nil
   :n  "gp"    #'+evil/reselect-paste
   :v  "gp"    #'+evil/alt-paste
   :nv "g@"    #'+evil:apply-macro
   :nv "gc"    #'evilnc-comment-operator
   :nv "go"    nil
   :nv "gx"    #'evil-exchange
   :nv "gy"    nil
   :n  "g="    #'evil-numbers/inc-at-pt
   :n  "g-"    #'evil-numbers/dec-at-pt
   :v  "g="    #'evil-numbers/inc-at-pt-incremental
   :v  "g-"    #'evil-numbers/dec-at-pt-incremental
   :v  "g+"    #'evil-numbers/inc-at-pt
   (:when (modulep! :tools lookup)
     :nv "K"   #'+lookup/documentation
     :nv "gd"  #'+lookup/definition
     :nv "gD"  nil
     :nv "gr"  #'+lookup/references
     :nv "gf"  #'+lookup/file
     :nv "gt" #'+lookup/type-definition
     :nv "gI"  #'+lookup/implementations
     :nv "gA"  #'+lookup/assignments)

   ;; custom evil keybinds
   :nv "zn"    #'+evil:narrow-buffer
   :n  "zN"    #'doom/widen-indirectly-narrowed-buffer
   :n  "zx"    nil
   :n  "zX"    nil
   ;; don't leave visual mode after shifting
   :v  "<"     #'+evil/shift-left  ; vnoremap < <gv
   :v  ">"     #'+evil/shift-right  ; vnoremap > >gv

   ;; text objects
   :textobj "a" #'evil-inner-arg                    #'evil-outer-arg
   :textobj "B" #'evil-textobj-anyblock-inner-block #'evil-textobj-anyblock-a-block
   :textobj "c" #'evilnc-inner-comment              #'evilnc-outer-commenter
   :textobj "f" #'+evil:defun-txtobj                #'+evil:defun-txtobj
   :textobj "g" #'+evil:whole-buffer-txtobj         #'+evil:whole-buffer-txtobj
   :textobj "i" #'evil-indent-plus-i-indent         #'evil-indent-plus-a-indent
   :textobj "j" #'evil-indent-plus-i-indent-up-down #'evil-indent-plus-a-indent-up-down
   :textobj "k" #'evil-indent-plus-i-indent-up      #'evil-indent-plus-a-indent-up
   :textobj "q" #'+evil:inner-any-quote             #'+evil:outer-any-quote
   :textobj "u" #'+evil:inner-url-txtobj            #'+evil:outer-url-txtobj
   :textobj "x" #'evil-inner-xml-attr               #'evil-outer-xml-attr

   ;; evil-lion
   :n "gl" #'evil-lion-left
   :n "gL" #'evil-lion-right
   :v "gl" #'evil-lion-left
   :v "gL" #'evil-lion-right
   )

  (after! org
    (map! :map calendar-mode-map
          :nm "<left>" #'calendar-backward-day
          :nm "<right>" #'calendar-forward-day
          :nm "<up>" #'calendar-backward-week
          :nm "<down>" #'calendar-forward-week
          :nm "[[" #'calendar-backward-month
          :nm "]]" #'calendar-forward-month
          :nm "[y" #'calendar-backward-year
          :nm "]y" #'calendar-forward-year
          )
    (map! :map org-mode-map
          :localleader
          "#" nil
          "'" nil
          "*" nil
          "+" nil
          "," nil
          "@" nil
          "." #'consult-org-heading
          "/" nil
          "A" nil
          "a" #'org-archive-subtree
          "e" #'org-export-dispatch
          "f" nil
          "h" #'org-toggle-heading
          "i" nil
          "m" #'org-toggle-item
          "k" nil
          "K" nil
          "n" nil
          "o" #'org-set-property
          "q" #'org-set-tags-command
          "t" #'org-todo
          "T" nil
          "x" #'org-toggle-checkbox
          (:prefix ("i" . "insert")
                   "h" #'org-insert-heading
                   "H" #'org-insert-subheading
                   "t" #'org-insert-todo-heading
                   "T" #'org-insert-todo-subheading
                   "d" #'insert-date-org-tree
                   "l" #'insert-time-org-tree
                   )
          "a" nil
          "b" nil
          "c" nil
          (:prefix ("d" . "date/deadline")
                   "d" #'org-deadline
                   "s" #'org-schedule
                   "t" #'org-time-stamp
                   "T" #'org-time-stamp-inactive)
          "g" nil
          "l" nil
          (:prefix ("l" . "links")
                   "c" #'org-cliplink
                   "d" #'+org/remove-link
                   "i" nil
                   "l" #'org-insert-link
                   "L" nil
                   "s" nil
                   "S" nil
                   "t" #'org-toggle-link-display
                   )
          "p" nil
          "P" nil
          "r" nil
          (:prefix ("r" . "refile")
                   "." #'+org/refile-to-current-file
                   "l" #'+org/refile-to-last-location
                   "f" #'+org/refile-to-file
                   "o" #'+org/refile-to-other-window
                   "O" #'+org/refile-to-other-buffer
                   "r" #'org-refile
                   "g" #'org-refile-goto-last-stored
                   "R" #'org-refile-reverse
                   ) ; to all `org-refile-targets'

          "s" nil
          (:prefix ("s" . "tree/subtree")
                   "d" #'org-cut-subtree
                   "S" #'org-sort
                   "p" #'org-paste-subtree
                   ))

    (map! :after org-agenda
          :map org-agenda-mode-map
          :m "C-SPC" #'org-agenda-show-and-scroll-up
          :localleader
          (:prefix ("d" . "date/deadline")
                   "d" #'org-agenda-deadline
                   "s" #'org-agenda-schedule)
          "c" nil
          "q" #'org-agenda-set-tags
          "r" #'org-agenda-refile
          "t" #'org-agenda-todo)


    (map! :map evil-org-mode-map
          :ni [C-return]   #'+org/insert-item-below
          :ni [C-S-return] #'+org/insert-item-above
          ;; more intuitive RET keybinds
          :n [return]   #'+org/dwim-at-point
          :n "RET"      #'+org/dwim-at-point
          :i [return]   #'+org/return
          :i "RET"      #'+org/return
          :i [S-return] #'+org/shift-return
          :i "S-RET"    #'+org/shift-return
          ;; more vim-esque org motion keys (not covered by evil-org-mode)
          :m "]h"  #'org-forward-heading-same-level
          :m "[h"  #'org-backward-heading-same-level
          :m "]l"  #'org-next-link
          :m "[l"  #'org-previous-link
          :m "]c"  #'org-babel-next-src-block
          :m "[c"  #'org-babel-previous-src-block
          ;; sensible vim-esque folding keybinds
          :n "za"  #'+org/toggle-fold
          :n "zA"  #'org-shifttab
          :n "zc"  #'+org/close-fold
          :n "zC"  #'outline-hide-subtree
          :n "zm"  #'+org/hide-next-fold-level
          :n "zM"  #'+org/close-all-folds
          :n "zn"  #'org-tree-to-indirect-buffer
          :n "zo"  #'+org/open-fold
          :n "zO"  #'outline-show-subtree
          :n "zr"  #'+org/show-next-fold-level
          :n "zR"  #'+org/open-all-folds
          :n "zi"  #'org-toggle-inline-images))

  )
