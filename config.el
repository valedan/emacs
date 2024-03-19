;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Dan Valentine"
      user-mail-address "danvalentine256@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
(setq doom-font (font-spec :family "JetBrains Mono" :size 16)
      doom-variable-pitch-font (font-spec :family "ETBookOT" :size 20)) ;; TODO: re-evaluate this font, I didn't spend much time researching. Also can have a different font for headers.
;; (custom-theme-set-faces
;;  'user
;;  '(variable-pitch ((t (:family "ETBembo" :height 180 :weight thin))))
;;  '(fixed-pitch ((t ( :family "Fira Code Retina" :height 160))))
;;  )
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-gruvbox)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)
(+global-word-wrap-mode +1)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


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
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
;; (map! :leader
;; (:prefix ("=" . "Equality")
                                        ; :desc "Describe bindings" :n "=d"                                             #'describe-bindings
;; :desc "askljd" :v "==" #'indent-region
;; :desc "Popup calc" :i "==" #'calc-popup))
;;


;; could not figure out how to integrate ruff into the lsp diagnostics directly, so it's just a separate checker in flycheck

(after! (:and lsp-mode flycheck)
  (add-hook 'python-mode-hook (lambda () (flycheck-add-next-checker 'lsp 'python-ruff ))))

(after! apheleia
  (add-to-list 'apheleia-mode-alist '(python-mode . ruff))
  (add-to-list 'apheleia-mode-alist '(python-ts-mode . ruff)))


(setq which-key-use-C-h-commands 't)

;; Navigate by displayed lines
(map! :n "<up>" #'evil-previous-visual-line)
(map! :n "<down>" #'evil-next-visual-line)
(map! :n "k" #'evil-previous-visual-line)
(map! :n "j" #'evil-next-visual-line)

;; Window navigation
(map! :n "<C-left>" #'evil-window-left)
(map! :n "<C-down>" #'evil-window-down)
(map! :n "<C-up>" #'evil-window-up)
(map! :n "<C-right>" #'evil-window-right)
(map! :n "C-j" #'evil-window-down)
(map! :n "C-h" #'evil-window-left)
(map! :n "C-k" #'evil-window-up)
(map! :n "C-l" #'evil-window-right)

(map! :leader :desc "Split window below" :n "-" #'split-window-below)
(map! :leader :desc "Split window right" :n "|" #'split-window-right)

;; TODO: Window resizing (emacs has lots of cool options)

(map! :n "gr" #'+lookup/references) ;TODO: was previously +eval/region
(map! :n "gt" #'+lookup/type-definition) ;TODO: Was previously +workspace/switch-next

                                        ; TODO: set up an ipython workflow

(map! :leader :desc "Open project file drawer" :n "d" #'+treemacs/toggle)


                                        ; TODO: fix color of evil-goggles, they are the same as the current line color now

;; TODO: choose bindings
;; (map! :leader :desc "Buffer flycheck" :n "" #'flycheck-list-errors)
;; (map! :leader :desc "Project flycheck" :n "" #'lsp-ui-flycheck-list)
(map! :desc "Next flycheck" :n "]f" #'flycheck-next-error)
(map! :desc "Prev flycheck" :n "[f" #'flycheck-previous-error)

;; TODO: set up ruff for linting and formatting https://github.com/radian-software/apheleia
;;TODO: Do I need a way to quickly toggle format on save on/off?
;; TODO:
;; map("n", ",", "@@", { desc = "Rerun last macro" })
;; map("n", "<leader>;", ",", { desc = "Previous f/t match" })

(setq which-key-idle-delay 0.25)
(setq projectile-project-search-path '("~/projects/"  ))

                                        ;Bindings to make
                                        ;Bookmarks - set, delete, goto (list)
                                        ;








;; LSP
;; (setq lsp-pylsp-plugins-ruff-enabled 't)
;; (setq lsp-pylsp-plugins-flake8-enabled 'f)
(setq flycheck-python-ruff-executable "ruff")
;; (setq lsp-ruff-lsp-log-level "info")

;;Org mode

(after! org

  (setq
   org-ellipsis "…"
   ;; org-superstar-headline-bullets-list '("◉" "●" "○" "◆" "●" "○" "◆")
   org-hide-emphasis-markers t
   org-pretty-entities t
   org-agenda-tags-column 0
   org-agenda-block-separator ?─
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

   ;; org-hide-leading-stars nil
   org-log-done 'time
   )



  (let* ((variable-tuple
          (cond ((x-list-fonts "ETBookOT")         '(:font "ETBookOT"))
                ;; ((x-list-fonts "Source Sans Pro") '(:font "Source Sans Pro"))
                ;; ((x-list-fonts "Lucida Grande")   '(:font "Lucida Grande"))
                ;; ((x-list-fonts "Verdana")         '(:font "Verdana"))
                ;; ((x-family-fonts "Sans Serif")    '(:family "Sans Serif"))
                (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
         (base-font-color     (face-foreground 'default nil 'default))
         (headline           `(:inherit default :weight bold :foreground ,base-font-color)))

    ;; (custom-theme-set-faces
    ;;  'user
    ;;  `(org-level-8 ((t (,@headline ,@variable-tuple))))
    ;;  `(org-level-7 ((t (,@headline ,@variable-tuple))))
    ;;  `(org-level-6 ((t (,@headline ,@variable-tuple))))
    ;;  `(org-level-5 ((t (,@headline ,@variable-tuple))))
    ;;  `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.1))))
    ;;  `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.25))))
    ;;  `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.5))))
    ;;  `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.75))))
    ;;  `(org-document-title ((t (,@headline ,@variable-tuple :height 2.0 :underline nil)))))
    )

  (add-hook 'org-mode-hook 'variable-pitch-mode)
  (add-hook 'org-mode-hook 'visual-line-mode)
  (add-hook 'org-mode-hook 'line-spacing 0.2)
  (add-hook 'org-mode-hook #'org-modern-mode)
  (add-hook 'org-agenda-finalize-hook #'org-modern-agenda)
  ;; (set-face-attribute 'line-number nil :inherit 'fixed)
  (custom-theme-set-faces
   'user
   '(org-block ((t (:inherit fixed-pitch))))
   '(line-number ((t (:inherit fixed-pitch))))
   '(line-number-current-line ((t (:inherit fixed-pitch))))
   '(org-code ((t (:inherit (shadow fixed-pitch)))))
   ;; '(org-document-info ((t (:foreground "dark orange"))))
   '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
   '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
   ;; '(org-link ((t (:foreground "royal blue" :underline t))))
   ;; '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
   '(org-property-value ((t (:inherit fixed-pitch))) t)
   '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
   ;; '(org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
   '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
   '(org-verbatim ((t (:inherit (shadow fixed-pitch)))))
   )
  )

(add-hook 'window-setup-hook 'toggle-frame-fullscreen t)
(setq line-spacing 0.05)
