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

(setq user-full-name "Dan Valentine"
      user-mail-address "danvalentine256@gmail.com")

(setq doom-font (font-spec :family "JetBrains Mono" :size 16)
      doom-variable-pitch-font (font-spec :family "ETBookOT" :size 20)) ;; TODO: re-evaluate this font, I didn't spend much time researching. Also can have a different font for headers.
(setq doom-theme 'doom-gruvbox)

(setq display-line-numbers-type 'relative)
(+global-word-wrap-mode +1)
;; consider turning this on if dired buffers get annoying
;; (setq dired-kill-when-opening-new-dired-buffer t)
(setq org-directory "~/org/")
(setq org-roam-directory "~/org/")


(after! apheleia
  (add-to-list 'apheleia-mode-alist '(python-mode . ruff))
  (add-to-list 'apheleia-mode-alist '(python-ts-mode . ruff)))

(after! pet
  (add-hook 'python-base-mode-hook 'pet-mode -10)
  )

(setq which-key-use-C-h-commands 't)
(setq which-key-idle-delay 0.25)
(setq projectile-project-search-path '("~/projects/"  ))
(add-hook 'window-setup-hook 'toggle-frame-fullscreen t)
(setq line-spacing 0.05)
(add-hook 'dired-mode-hook
          (lambda ()
            (dired-hide-details-mode)
            ))

;; LSP
;; (setq lsp-pylsp-plugins-ruff-enabled 't)
;; (setq lsp-pylsp-plugins-flake8-enabled 'f)
(setq flycheck-python-ruff-executable "ruff")
;; (setq lsp-ruff-lsp-log-level "info")

(defun clear-trailing-whitespace-and-append-newline () ""
       (delete-trailing-whitespace)
       (goto-char (point-max))
       (+evil/insert-newline-below 1)
       )

(defun dan-capture-log (prefix) "" (interactive "P")
       (org-journal-new-entry prefix)
       (clear-trailing-whitespace-and-append-newline)
       (goto-char (point-max))
       (insert "- ")
       (evil-append 1))

(defun dan-capture-task () "" (interactive)
       (org-journal-new-entry t)
       (clear-trailing-whitespace-and-append-newline)
       (goto-char (point-max))
       (insert "** TODO ")
       (evil-append 1)
       )

(defun dan-add-id-to-journal-entry ()
  "Add an ID property to the first heading of the current org-journal file."
  (save-excursion
    (goto-char (point-min)) ; go to the beginning of the buffer
    (org-id-get-create)
    (goto-char (point-max))
    (+evil/insert-newline-above 1)
    )
  )

(defun dan-save-and-close-buffer ()
  "Save and close the current buffer."
  (interactive)
  (save-buffer)
  (kill-current-buffer))

(defun get-org-files-in-directories (dirs)
  "Return a list of all .org files in DIRS and their subdirectories."
  (mapcan (lambda (dir)
            (directory-files-recursively dir "\\.org$"))
          dirs))

;; TODO: fix this
;; (after! python
;;   (setq prettify-symbols-mode nil)
;;   )
;;Org mode
(after! org
  (setq browse-url-browser-function 'browse-url-default-macosx-browser)
  (setq +company-backend-alist (assq-delete-all 'text-mode +company-backend-alist))
  (add-to-list '+company-backend-alist '(text-mode (:separate company-dabbrev company-yasnippet)))
  (setq
   org-ellipsis "…"
   org-hide-emphasis-markers t
   org-startup-with-inline-images t
   org-image-actual-width 1200
   org-enforce-todo-dependencies nil
   org-pretty-entities t
   org-agenda-tags-column 0
   org-agenda-block-separator ?─
   org-roam-extract-new-file-path "${slug}.org"
   org-fancy-priorities-list '("‼️" "❗" "⚠️")
   org-agenda-time-grid
   '((daily today require-timed)
     (800 1000 1200 1400 1600 1800 2000)
     " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
   org-agenda-current-time-string
   "◀── now ─────────────────────────────────────────────────"
   org-modern-list
   '((?+ . "◦")
     (?- . "•")
     (?* . "•"))

   org-log-done 'time
   org-todo-keywords
   '((sequence "TODO(t)" "PROJ(p)" "|" "DONE(d)" "SOMEDAY(s)" "CANCELLED(c)")
     )
   )
  (org-roam-db-autosync-mode)



  (let* ((variable-tuple
          (cond ((x-list-fonts "ETBookOT")         '(:font "ETBookOT"))
                (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
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
  (add-hook 'org-mode-hook #'org-modern-mode)
  (add-hook 'org-mode-hook #'org-fancy-priorities-mode)
  (add-hook 'org-mode-hook #'org-super-agenda-mode)
  (add-hook 'org-agenda-finalize-hook #'org-modern-agenda)
  ;; (set-face-attribute 'line-number nil :inherit 'fixed)
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
   )

  (setq task-files (append (get-org-files-in-directories '("~/org/projects" "~/org/journal"))))
  (setq refile-targets (append (get-org-files-in-directories '("~/org/projects"))))

  (setq org-refile-targets refile-targets)
  (setq org-agenda-files task-files)

  (defun dashboard-today ()
    ""
    (interactive)
    (org-ql-search task-files
      `(or (and (todo)
                (or
                 (tags "next")
                 (deadline auto)
                 (scheduled :to today)))
           (closed :on today))
      :title "Today"
      ;; :sort '(todo priority date)
      :super-groups '((:name "📅 Deadlines" :deadline t)
                      (:name "⏳ Scheduled" :scheduled t)
                      (:name "‼️ Priority" :priority "A")
                      (:name "☕️ Next" :tag ("next"))
                      (:name "✅ Completed Today" :todo "DONE"))
      ))

  (defun dashboard-active-tasks ()
    ""
    (interactive)
    (org-ql-search task-files
      `(and (todo)
            )
      :title "Active Tasks"
      :super-groups '((:auto-map (lambda (item)
                                   (-when-let* ((marker (or (get-text-property 0 'org-marker item)
                                                            (get-text-property 0 'org-hd-marker item)))
                                                (file-path (->> marker marker-buffer buffer-file-name))
                                                (directory-name (->> file-path file-name-directory directory-file-name file-name-nondirectory)))
                                     (concat "Directory: " directory-name)))))
      ))

  (defun dashboard-orphans ()
    ""
    (interactive)
    (org-ql-search task-files
      `(and (todo)
            (not (path "projects"))
            (not (tags "next"))
            )
      :title "Orphans"
      ))
  )


(after! org-journal
  (setq org-journal-dir "~/org/journal")
  ;; (setq org-roam-dailies-directory nil)
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

(after! org-roam
  (setq org-roam-graph-viewer "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome")
  (setq org-roam-capture-templates
        `(("n" "note" entry (file "~/org/templates/note.org")
           :if-new
           (file+head "${slug}.org" "#+title: ${title}\n")
           :unnarrowed t)
          ("p" "project" entry (file "~/org/templates/project.org")
           :if-new (file+head "${slug}.org" "#+title: ${title}\n")
           :unnarrowed t)
          ;; ("P" "inline-project" entry (file "~/org/templates/inline_project.org")
          ;;  :target (node)
          ;;  :unnarrowed t)
          ))
  ;;
  ;; (defun my-org-protocol-capture-task (info)
  ;; "Handle org-protocol capture task with INFO."
  ;; (let ((url (plist-get info :url))
  ;;       (title (plist-get info :title))
  ;;       (description (plist-get info :description)))
  ;;   ;; Your custom function logic here, e.g., creating a task in an Org file.
  ;;   (dan-capture-task)
  ;;   )
  ;; )


  ;; (add-to-list 'org-protocol-protocol-alist
  ;;              '("Capture Task"
  ;;                :protocol "capture-task"
  ;;                :function my-org-protocol-capture-task))


  )

(after! vterm

  (defun dan/set-terminal-cursor ()
    (setq cursor-type 'bar))

  (add-hook 'vterm-mode-hook 'dan/set-terminal-cursor)
  )

(defun gm/jupyter-eval-region (beg end)
  (jupyter-eval-region nil beg end))


(after! code-cells
  ;; (setq code-cells-convert-ipynb-style '(("pandoc" "--to" "ipynb" "--from" "org")
  ;;                                        ("pandoc" "--to" "org" "--from" "ipynb")
  ;;                                        (lambda () #'org-mode)))
  (add-to-list 'auto-mode-alist '("\\.ipynb\\'" . python-mode))
  (add-hook 'python-mode-hook 'code-cells-mode)
  (add-hook 'org-mode-hook 'code-cells-mode-maybe)
  (add-to-list 'code-cells-eval-region-commands '(org-mode . python-shell-send-region))
  (add-to-list 'code-cells-eval-region-commands '(jupyter-repl-interaction-mode . gm/jupyter-eval-region))
  (setq jupyter-repl-echo-eval-p t)
  )

;; could not figure out how to integrate ruff into the lsp diagnostics directly, so it's just a separate checker in flycheck
(after! (:all lsp-mode flycheck lsp-pyright)
  (add-hook 'python-mode-hook (lambda () (flycheck-add-next-checker 'lsp 'python-ruff ))))

(undefine-key! doom-leader-map "SPC SPC")
(setq doom-localleader-key "SPC SPC")
(setq doom-localleader-alt-key "M-SPC SPC")

(after! evil
  (evil-define-key 'normal global-map (kbd "SPC SPC")
    (lambda () (interactive)
      (execute-kbd-macro (kbd "SPC m"))))
  ;; Navigate by displayed lines
  (map! :nm "<up>" #'evil-previous-visual-line)
  (map! :nm "<down>" #'evil-next-visual-line)
  (map! :nm "k" #'evil-previous-visual-line)
  (map! :nm "j" #'evil-next-visual-line)

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

  (map! :n "C-t t" nil)


  (map! :leader
        :desc "Split window below" "-" #'+evil/window-split-and-follow
        :desc "Split window right" "|" #'+evil/window-vsplit-and-follow

        :desc "M-x"       ";"   nil
        :desc "M-x"       ";"   #'execute-extended-command
        :desc "M-x"                   ":"    nil
        :desc "Pop up scratch buffer" "x"    nil
        :desc "Org Capture"           "X"    nil
        :desc "Capture log"           "l"    #'dan-capture-log
        :desc "Capture task"           "="    #'dan-capture-task
        :desc "Universal argument"    "u"    #'universal-argument
        :desc "window"                "w"    evil-window-map
        :desc "help"                  "h"    help-map

        :desc "Toggle last popup"     "`"    #'+popup/toggle
        ;; put something better here (already have ff))
        :desc "Terminal toggle"             "."    nil
        :desc "Terminal toggle"             "."    #'+vterm/toggle
        ","    nil
        :desc "Repeat last macro" ","    #'evil-execute-last-recorded-macro
        :desc "Switch buffer"        "<"        nil
        :desc "Switch to last buffer" "\\"    #'evil-switch-to-windows-last-buffer
        :desc "Resume last search"    "'"
        (cond ((modulep! :completion vertico)    #'vertico-repeat)
              ((modulep! :completion ivy)        #'ivy-resume)
              ((modulep! :completion helm)       #'helm-resume))

        :desc "Search for symbol in project" "*" nil
        :desc "Search project"               "/" #'+default/search-project

        :desc "Jump to bookmark"      "RET"  #'bookmark-jump

        ;; overriding the doom keymap is super fucky. this approach of nilling out the existing group and defining my own only works if i change the name of the group when defining my own, otherwise the nilling does not take.
      ;;; <leader> TAB --- workspace
        "TAB" nil
        (:when (modulep! :ui workspaces)
          (:prefix-map ("TAB" . "Workspace")
           :desc "Display tab bar"           "TAB" #'+workspace/display
           :desc "Switch workspace"          "."   #'+workspace/switch-to
           :desc "Switch to last workspace"  "`"   #'+workspace/other
           :desc "New workspace"             "n"   #'+workspace/new
           :desc "New named workspace"       "N"   #'+workspace/new-named
           :desc "Delete session"            "x"   #'+workspace/kill-session
           :desc "Delete this workspace"     "d"   #'+workspace/delete
           :desc "Rename workspace"          "r"   #'+workspace/rename
           :desc "Restore last session"      "R"   #'+workspace/restore-last-session
           :desc "1st workspace"   "1"   #'+workspace/switch-to-0
           :desc "2nd workspace"   "2"   #'+workspace/switch-to-1
           :desc "3rd workspace"   "3"   #'+workspace/switch-to-2
           :desc "4th workspace"   "4"   #'+workspace/switch-to-3
           :desc "5th workspace"   "5"   #'+workspace/switch-to-4))
      ;;; <leader> b --- buffer
        "b" nil
        (:prefix-map ("b" . "Buffer")
                     (:when (modulep! :ui workspaces)
                       :desc "Switch workspace buffer" "b" #'persp-switch-to-buffer
                       :desc "Switch buffer"           "B" #'switch-to-buffer)

                     :desc "All buffers"                 "a"   #'buffer-menu
                     :desc "Clone buffer"                "c"   #'clone-indirect-buffer
                     :desc "Kill buffer"                 "d"   #'kill-current-buffer
                     :desc "ibuffer"                     "i"   #'ibuffer
                     :desc "Set bookmark"                "m"   #'bookmark-set
                     :desc "Delete bookmark"             "M"   #'bookmark-delete
                     :desc "New empty buffer"            "n"   #'evil-buffer-new
                     :desc "Revert buffer"               "R"   #'revert-buffer
                     :desc "Rename buffer"               "r"   #'rename-buffer
                     :desc "Save buffer"                 "s"   #'basic-save-buffer
                     :desc "Save buffer as root"         "S"   #'doom/sudo-save-buffer
                     :desc "Pop up scratch buffer"       "x"   #'doom/open-scratch-buffer
                     :desc "Widen buffer" "w" #'widen
                     :desc "Switch to scratch buffer"    "X"   #'doom/switch-to-scratch-buffer
                     :desc "Yank buffer"                 "y"   #'+default/yank-buffer-contents
                     )

      ;;; <leader> c --- code
        "c" nil
        (:prefix-map ("c" . "Code")
                     (:when (and (modulep! :tools lsp) (not (modulep! :tools lsp +eglot)))
                       ;; pyright doesn't really have code actions but other langs/lsps might
                       :desc "LSP Execute code action" "a" #'lsp-execute-code-action
                       ;; formatter should handle this
                       (:when (modulep! :completion vertico)
                         ;; maybe this should live in search group
                         :desc "Jump to symbol in workspace" "y"   #'consult-lsp-symbols)
                       :desc "LSP Rename"                          "r"   #'lsp-rename)
                     :desc "Evaluate buffer/region"                "v"   #'+eval/buffer-or-region
                     :desc "Format buffer/region"                  "f"   #'+format/region-or-buffer
                     :desc "Send to repl"                          "s"   #'+eval/send-region-to-repl
                     :desc "List errors"                           "x"   #'+default/diagnostics
                     :desc "List errors (flycheck)" "e" #'flycheck-list-errors
                     :desc "List project errors" "E" #'lsp-ui-flycheck-list
                     :desc "Open ipython repl" "i" #'+python/open-ipython-repl
                     :desc "Start jupyter repl" "j" #'jupyter-run-repl
                     :desc "Attach buffer to jupyter repl" "p" #'jupyter-repl-associate-buffer
                     )


        ;; <leader> d --- directory
        "d" nil
        (:prefix-map ("d" . "Directory")
         :desc "Toggle sidebar" "d" #'dirvish-side
         :desc "Full-frame Dirvish" "m" #'dirvish
         :desc "Dirvish-fd" "f" #'dirvish-fd
         :desc "Dirvish here" "h" #'dirvish-dwim

         )

      ;;; <leader> f --- file
        "f" nil
        (:prefix-map ("f" . "File")
         "c" nil
         :desc "Find file in private config" "c"   #'doom/find-file-in-private-config
         :desc "Copy this file"              "C"   #'doom/copy-this-file
         :desc "Delete this file"            "d"   #'doom/delete-this-file
         :desc "Find file"                   "f"   #'projectile-find-file
         :desc "Recent files"                "r"   #'recentf-open-files
         :desc "Rename/move file"            "m"   #'doom/move-this-file
         :desc "Save file"                   "s"   #'save-buffer
         :desc "Save file as..."             "S"   #'write-file
         :desc "Save and kill buffer"        "q"   #'dan-save-and-close-buffer
         :desc "Sudo this file"              "u"   #'doom/sudo-this-file
         :desc "Yank file path"              "y"   #'+default/yank-buffer-path
         :desc "Yank file path from project" "Y"   #'+default/yank-buffer-path-relative-to-project

         )

            ;;; <leader> g --- git/version control

        "g" nil
        (:prefix-map ("g" . "Git")
         :desc "Revert file"                 "R"   #'vc-revert
         :desc "Copy link to remote file"         "y"   #'+vc/browse-at-remote-kill
         :desc "Copy link to homepage"       "Y"   #'+vc/browse-at-remote-kill-homepage
         (:when (modulep! :ui vc-gutter)
           :desc "Revert hunk at point"      "r"   #'+vc-gutter/revert-hunk
           :desc "stage hunk at point"       "s"   #'+vc-gutter/stage-hunk
           :desc "Git time machine"          "t"   #'git-timemachine-toggle)
         (:when (modulep! :tools magit)
           :desc "Magit dispatch"            "/"   #'magit-dispatch
           :desc "Magit file dispatch"       "."   #'magit-file-dispatch
           :desc "Magit switch branch"       "b"   #'magit-branch-checkout
           :desc "Magit status"              "g"   #'magit-status
           :desc "Magit status here"         "G"   #'magit-status-here
           :desc "Magit file delete"         "D"   #'magit-file-delete
           :desc "Magit blame"               "B"   #'magit-blame-addition
           :desc "Magit clone"               "C"   #'magit-clone
           :desc "Magit fetch"               "F"   #'magit-fetch
           :desc "Magit buffer log"          "L"   #'magit-log-buffer-file
           :desc "Git stage file"            "S"   #'magit-stage-file
           :desc "Git unstage file"          "U"   #'magit-unstage-file
           (:prefix ("f" . "find")
            :desc "Find file"                 "f"   #'magit-find-file
            :desc "Find commit"               "c"   #'magit-show-commit)
           (:prefix ("o" . "open in browser")
            :desc "Browse file or region"     "o"   #'+vc/browse-at-remote
            :desc "Browse homepage"           "h"   #'+vc/browse-at-remote-homepage)
           :desc "List repositories"         "r"   #'magit-list-repositories
           (:prefix ("c" . "create")
            :desc "Initialize repo"           "r"   #'magit-init
            :desc "Commit"                    "c"   #'magit-commit-create
            :desc "Fixup"                     "f"   #'magit-commit-fixup
            :desc "Branch"                    "b"   #'magit-branch-and-checkout)))

      ;;; <leader> i --- insert
        "i" nil
        (:prefix-map ("i" . "Insert")
         :desc "Emoji"                       "e"   #'emoji-search
         :desc "From evil register"            "r"   #'evil-show-registers
         :desc "Snippet"                       "s"   #'yas-insert-snippet
         :desc "Unicode"                       "u"   #'insert-char
         :desc "From clipboard"                "y"   #'+default/yank-pop)

      ;;; <leader> n --- notes
        "n" nil
        (:prefix-map ("n" . "Notes")
         :desc "Capture to node"            "n" #'org-roam-capture
         :desc "Open calendar"            "c" #'calendar
         :desc "Active tasks"                   "a" #'dashboard-active-tasks
         :desc "Org noter"                  "m" #'org-noter
         :desc "Find file in notes"           "F" #'+default/find-in-notes
         :desc "Find roam node"                  "f" #'org-roam-node-find
         :desc "Browse notes"                 "d" #'+default/browse-notes
         :desc "Today agenda"                    "t" #'dashboard-today
         :desc "Orphans"                    "o" #'dashboard-orphans
         :desc "Search notes"                 "s" #'+default/org-notes-search
         :desc "Toggle roam buffer"         "b" #'org-roam-buffer-toggle
         :desc "Insert node"                "i" #'org-roam-node-insert
         :desc "Search org headings" "/" #'consult-org-agenda
         "y" nil
         :desc "Goto today"                "y" #'org-journal-open-current-journal-file
         :desc "Sync database"              "S" #'org-roam-db-sync
         ;; TODO default version of this sucks, check out https://github.com/org-roam/org-roam-ui for a better option
         ;; :desc "Show graph"                 "g" #'org-roam-graph
         )
      ;;; <leader> o --- open
        "o" nil
        (:prefix-map ("o" . "Open")
         :desc "Default browser"    "b"  #'browse-url-of-file
         :desc "New frame"          "f"  #'make-frame
         :desc "Select frame"       "F"  #'select-frame-by-name
         :desc "REPL"               "r"  #'+eval/open-repl-other-window
         :desc "REPL (same window)" "R"  #'+eval/open-repl-same-window
         (:when (modulep! :ui treemacs)
           ;; add this to File
           :desc "Find file in project sidebar" "P" #'treemacs-find-file)
         (:when (modulep! :term shell)
           :desc "Toggle shell popup"    "t" #'+shell/toggle
           :desc "Open shell here"       "T" #'+shell/here)
         (:when (modulep! :term term)
           :desc "Toggle terminal popup" "t" #'+term/toggle
           :desc "Open terminal here"    "T" #'+term/here)
         (:when (modulep! :term vterm)
           :desc "Toggle vterm popup"    "t" #'+vterm/toggle
           :desc "Open vterm here"       "T" #'+vterm/here)
         (:when (modulep! :term eshell)
           :desc "Toggle eshell popup"   "e" #'+eshell/toggle
           :desc "Open eshell here"      "E" #'+eshell/here)
         (:when (modulep! :os macos)
           :desc "Reveal in Finder"           "o" #'+macos/reveal-in-finder
           :desc "Reveal project in Finder"   "O" #'+macos/reveal-project-in-finder)
         (:when (modulep! :tools docker)
           :desc "Docker" "D" #'docker)
         (:when (modulep! :email mu4e)
           :desc "mu4e" "m" #'=mu4e)
         (:when (modulep! :email notmuch)
           :desc "notmuch" "m" #'=notmuch)
         (:when (modulep! :email wanderlust)
           :desc "wanderlust" "m" #'=wanderlust))

        ;; TODO: Add shortcut to config (previously removed these)
      ;;; <leader> p --- project
        "p" nil
        ;; TODO: Seems like a bunch of these can be meged with Files map
        (:prefix-map ("p" . "Project")
         :desc "Browse project"               "." #'+default/browse-project
         :desc "Browse other project"         ">" #'doom/browse-in-other-project
         :desc "Run cmd in project root"      "!" #'projectile-run-shell-command-in-root
         :desc "Async cmd in project root"    "&" #'projectile-run-async-shell-command-in-root
         :desc "Add new project"              "a" #'projectile-add-known-project
         :desc "Switch to project buffer"     "b" #'projectile-switch-to-buffer
         :desc "Compile in project"           "c" #'projectile-compile-project
         :desc "Repeat last command"          "C" #'projectile-repeat-last-command
         :desc "Remove known project"         "d" #'projectile-remove-known-project
         :desc "Discover projects in folder"  "A" #'+default/discover-projects
         :desc "Edit project .dir-locals"     "e" #'projectile-edit-dir-locals
         :desc "Find file in project"         "f" #'projectile-find-file
         :desc "Find file in other project"   "F" #'doom/find-file-in-other-project
         :desc "Configure project"            "g" #'projectile-configure-project
         :desc "Invalidate project cache"     "i" #'projectile-invalidate-cache
         :desc "Kill project buffers"         "k" #'projectile-kill-buffers
         :desc "Switch project"               "p" #'projectile-switch-project
         :desc "Find recent project files"    "r" #'projectile-recentf
         :desc "Run project"                  "R" #'projectile-run-project
         :desc "Save project files"           "s" #'projectile-save-project-buffers
         :desc "List project todos"           "t" #'magit-todos-list
         :desc "Test project"                 "T" #'projectile-test-project
         :desc "Pop up scratch buffer"        "x" #'doom/open-project-scratch-buffer
         :desc "Switch to scratch buffer"     "X" #'doom/switch-to-project-scratch-buffer
         (:when (and (modulep! :tools taskrunner)
                     (or (modulep! :completion ivy)
                         (modulep! :completion helm)))
           :desc "List project tasks"          "z" #'+taskrunner/project-tasks))

      ;;; <leader> q --- quit/session
        "q" nil
        (:prefix-map ("q" . "Quit/session")
         :desc "Restart emacs server"         "d" #'+default/restart-server
         :desc "Delete frame"                 "f" #'delete-frame
         :desc "Clear current frame"          "F" #'doom/kill-all-buffers
         :desc "Kill Emacs (and daemon)"      "K" #'save-buffers-kill-emacs
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
         :desc "Search other directory"       "D" #'+default/search-other-cwd
         :desc "Jump to symbol"               "i" #'imenu
         :desc "Jump to link"                 "l" #'ffap-menu
         :desc "Jump list"                    "j" #'evil-show-jumps
         :desc "Look up online"               "o" #'+lookup/online
         :desc "Look up in local docsets"     "k" #'+lookup/in-docsets
         :desc "Jump to mark"                 "r" #'evil-show-marks
         :desc "Search buffer for thing at point" "s" #'+vertico/search-symbol-at-point
         :desc "Undo history"                 "u"
         (cond ((modulep! :emacs undo +tree)     #'undo-tree-visualize)
               ((modulep! :emacs undo)           #'vundo)))

      ;;; <leader> t --- toggle
        "t" nil
        (:prefix-map ("t" . "Toggle")
         :desc "Big mode"                     "b" #'doom-big-font-mode
         :desc "Fill Column Indicator"        "c" #'global-display-fill-column-indicator-mode
         :desc "Flycheck"                      "f" #'flycheck-mode
         :desc "Frame fullscreen"             "F" #'toggle-frame-fullscreen
         (:when (modulep! :ui indent-guides)
           :desc "Indent guides"              "i" #'highlight-indent-guides-mode)
         :desc "Read-only mode"               "r" #'read-only-mode
         :desc "Visible mode"                 "v" #'visible-mode
         (:when (modulep! :ui zen)
           :desc "Zen mode"                   "z" #'+zen/toggle
           :desc "Zen mode (fullscreen)"      "Z" #'+zen/toggle-fullscreen)

         )
        ;; <leader> w --- windows
        ;; these are defined in emacs/modules/editor/evil/config.el
        "w" nil
        (:prefix-map ("w" . "Window")
         :desc "Delete window" "d" #'evil-window-delete
         :desc "Balance windows" "=" #'balance-windows
         :desc "Maximize window" "m" #'doom/window-maximize-buffer
         :desc "Enlarge window" "e" #'doom/window-enlargen
         :desc "Undo window layout change" "u" #'winner-undo
         :desc "New window" "n" #'evil-window-new
         :desc "Quit window" "q" #'evil-quit
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
  )

;; TODO: Window resizing (emacs has lots of cool options)

(map! :n "gr" #'+lookup/references) ;TODO: was previously +eval/region
(map! :n "gt" #'+lookup/type-definition) ;TODO: Was previously +workspace/switch-next

                                        ; TODO: set up an ipython workflow

(after! evil
  (map!
   ;; mine
   :n "]e" #'flycheck-next-error
   :n "[e" #'flycheck-previous-error
   :n "]g" #'+vc-gutter/next-hunk
   :n "[g" #'+vc-gutter/previous-hunk
   :n "]d" #'org-journal-next-entry
   :n "[d" #'org-journal-previous-entry


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
   :n "] TAB"   #'+workspace/switch-right
   :n "[ TAB"   #'+workspace/switch-left

   ;; custom vim-unmpaired-esque keys
   :m  "]#"    nil
   :m  "[#"    nil
   :m  "]a"    nil
   :m  "[a"    nil
   :m  "]c"    #'+evil/next-comment
   :m  "[c"    #'+evil/previous-comment
   :m  "]e"    nil
   :m  "[e"    nil
   :n  "]F"    #'+evil/next-frame
   :n  "[F"    #'+evil/previous-frame
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
   ))

(map! :after python
      :map python-mode-map
      :localleader
      "x" #'python-pytest
      )
(after! dirvish
  (set-popup-rule! "^ ?\\*Dirvish.*" :ignore t)
  (map! :map dirvish-mode-map
        :n  "?"   #'dirvish-dispatch
        :n  "q"   #'dirvish-quit
        :ng "a"   #'dirvish-quick-access
        :ng "f"   #'dirvish-file-info-menu
        :ng "y"   #'dirvish-yank-menu
        :ng "s"   #'dirvish-quicksort
        :ng "<left>" #'dired-up-directory
        :ng "<right>" #'dired-find-file
        :ng "-" #'dired-create-empty-file
        :ng "+" #'dired-create-directory
        :ng "TAB" #'dirvish-subtree-toggle
        :ng "M-t" #'dirvish-layout-toggle
        :ng "M-b" #'dirvish-history-go-backward
        :ng "M-f" #'dirvish-history-go-forward
        :ng "M-n" #'dirvish-narrow
        :ng "M-m" #'dirvish-mark-menu
        :ng "M-s" #'dirvish-setup-menu
        :ng "M-e" #'dirvish-emerge-menu)
  ;; (setq dirvish-attributes '(file-size collapse)
  ;;       dirvish-mode-line-format
  ;;       '(:left (sort file-time symlink) :right (omit yank index)))

  ;; (dirvish-quick-access-entries ; It's a custom option, `setq' won't work
  ;;  '(("h" "~/"                          "Home")
  ;;    ("d" "~/Downloads/"                "Downloads")
  ;;    ("m" "/mnt/"                       "Drives")
  ;;    ("t" "~/.local/share/Trash/files/" "TrashCan")))
  ;; (dirvish-peek-mode) ; Preview files in minibuffer
  ;; (dirvish-side-follow-mode) ; similar to `treemacs-follow-mode'
  (setq dirvish-mode-line-format
        '(:left (sort symlink) :right (omit yank index)))
  (setq dirvish-attributes
        '(nerd-icons file-time collapse subtree-state vc-state))
  (setq delete-by-moving-to-trash t)
  (setq dired-listing-switches
        "-l --almost-all --human-readable --group-directories-first --no-group -g")
  (setq dired-omit-files (concat dired-omit-files "\\|.DS_Store$"))
  )

(after! code-cells
  (map!
   :map code-cells-mode-map
   :nm "]c" #'code-cells-forward-cell
   :nm "[c" #'code-cells-backward-cell
   :nmiv "<C-return>" #'code-cells-eval
   :nmiv "<C-RET>" #'code-cells-eval
   :nmiv "<M-u>" #'code-cells-move-cell-up
   :nmiv "<M-d>" #'code-cells-move-cell-down)
  (map! :map code-cells-mode-map
        :localleader
        (:prefix ("c" . "cells")
                 "m" #'code-cells-mark-cell
                 "b" #'code-cells-eval-above
                 "w" #'code-cells-write-ipynb
                 "r" #'code-cells-convert-ipynb
                 "c" #'code-cells-comment-or-uncomment

                 )

        )

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
        "'" #'org-edit-special
        "*" nil
        "+" nil
        "," nil
        "@" nil
        "." #'consult-org-heading
        "/" nil
        "A" nil
        "e" #'org-export-dispatch
        "f" nil
        "h" #'org-toggle-heading
        "i" nil
        "I" #'org-toggle-item
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
                 "i" #'org-id-get-create
                 )
        "a" nil
        ;; (:prefix ("a" . "attachments")
        ;;          "a" #'org-attach
        ;;          "d" #'org-attach-delete-one
        ;;          "D" #'org-attach-delete-all
        ;;          "f" #'+org/find-file-in-attachments
        ;;          "l" #'+org/attach-file-and-insert-link
        ;;          "n" #'org-attach-new
        ;;          "o" #'org-attach-open
        ;;          "O" #'org-attach-open-in-emacs
        ;;          "r" #'org-attach-reveal
        ;;          "R" #'org-attach-reveal-in-emacs
        ;;          "u" #'org-attach-url
        ;;          "s" #'org-attach-set-directory
        ;;          "S" #'org-attach-sync
        ;;          (:when (modulep! +dragndrop)
        ;;            "c" #'org-download-screenshot
        ;;            "p" #'org-download-clipboard
        ;;            "P" #'org-download-yank))
        (:prefix ("b" . "tables")
                 "-" #'org-table-insert-hline
                 "a" #'org-table-align
                 "b" #'org-table-blank-field
                 "c" #'org-table-create-or-convert-from-region
                 "e" #'org-table-edit-field
                 "f" #'org-table-edit-formulas
                 "h" #'org-table-field-info
                 "s" #'org-table-sort-lines
                 "r" #'org-table-recalculate
                 "R" #'org-table-recalculate-buffer-tables
                 (:prefix ("d" . "delete")
                          "c" #'org-table-delete-column
                          "r" #'org-table-kill-row)
                 (:prefix ("i" . "insert")
                          "c" #'org-table-insert-column
                          "h" #'org-table-insert-hline
                          "r" #'org-table-insert-row
                          "H" #'org-table-hline-and-move)
                 (:prefix ("t" . "toggle")
                          "f" #'org-table-toggle-formula-debugger
                          "o" #'org-table-toggle-coordinate-overlays)
                 (:when (modulep! +gnuplot)
                   "p" #'org-plot/gnuplot))
        "c" nil
        "c" #'org-capture-goto-last-stored
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
                 (:when (modulep! :os macos)
                   "g" nil
                   ))
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
                 "R" #'org-refile-reverse) ; to all `org-refile-targets'

        "s" nil
        (:prefix ("s" . "tree/subtree")
                 "d" #'org-cut-subtree
                 "h" #'org-promote-subtree
                 "j" #'org-move-subtree-down
                 "k" #'org-move-subtree-up
                 "l" #'org-demote-subtree
                 "<left>" #'org-promote-subtree
                 "<down>" #'org-move-subtree-down
                 "<up>" #'org-move-subtree-up
                 "<right>" #'org-demote-subtree
                 "s" #'org-sparse-tree
                 "S" #'org-sort)
        (:prefix ("p" . "priority")
                 "d" #'org-priority-down
                 "p" #'org-priority
                 "u" #'org-priority-up))

  (map! :after org-agenda
        :map org-agenda-mode-map
        :m "C-SPC" #'org-agenda-show-and-scroll-up
        :localleader
        (:prefix ("d" . "date/deadline")
                 "d" #'org-agenda-deadline
                 "s" #'org-agenda-schedule)
        "c" nil
        (:prefix ("p" . "priority")
                 "d" #'org-agenda-priority-down
                 "p" #'org-agenda-priority
                 "u" #'org-agenda-priority-up)
        "q" #'org-agenda-set-tags
        "r" #'org-agenda-refile
        "t" #'org-agenda-todo)


  (map! :map evil-org-mode-map
        ;; :ni [C-return]   #'+org/insert-item-below
        ;; :ni [C-S-return] #'+org/insert-item-above
        ;; ;; navigate table cells (from insert-mode)
        ;; :i Cright (cmds! (org-at-table-p) #'org-table-next-field
        ;;                  #'org-end-of-line)
        ;; :i Cleft  (cmds! (org-at-table-p) #'org-table-previous-field
        ;;                  #'org-beginning-of-line)
        ;; :i Cup    (cmds! (org-at-table-p) #'+org/table-previous-row
        ;;                  #'org-up-element)
        ;; :i Cdown  (cmds! (org-at-table-p) #'org-table-next-row
        ;;                  #'org-down-element)
        ;; :ni CSright   #'org-shiftright
        ;; :ni CSleft    #'org-shiftleft
        ;; :ni CSup      #'org-shiftup
        ;; :ni CSdown    #'org-shiftdown
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
        :n "gQ"  #'org-fill-paragraph
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
