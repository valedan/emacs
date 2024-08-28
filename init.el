;;; init.el -*- lexical-binding: t; -*-

;; This file controls what Doom modules are enabled and what order they load
;; in. Remember to run 'doom sync' after modifying it!

;; NOTE Press 'SPC h d h' (or 'C-h d h' for non-vim users) to access Doom's
;;      documentation. There you'll find a link to Doom's Module Index where all
;;      of our modules are listed, including what flags they support.

;; NOTE Move your cursor over a module's name (or its flags) and press 'K' (or
;;      'C-c c k' for non-vim users) to view its documentation. This works on
;;      flags as well (those symbols that start with a plus).
;;
;;      Alternatively, press 'gd' (or 'C-c c d') on a module to browse its
;;      directory (for easy access to its source code).

(doom! :input

       :completion
       (corfu +icons +orderless)
       vertico           ; the search engine of the future

       :ui
       doom              ; what makes DOOM look the way it does
       doom-dashboard    ; a nifty splash screen for Emacs
       doom-quit         ; DOOM quit-message prompts when you quit Emacs
       hl-todo           ; highlight TODO/FIXME/NOTE/DEPRECATED/HACK/REVIEW
       ligatures         ; ligatures and symbols to make your code pretty again
       modeline          ; snazzy, Atom-inspired modeline, plus API
       nav-flash         ; blink cursor line after big motions
       ophints           ; highlight the region an operation acts on
       (popup +defaults)   ; tame sudden yet inevitable temporary windows
       (vc-gutter +pretty) ; vcs diff in the fringe
       vi-tilde-fringe   ; fringe tildes to mark beyond EOB

       :editor
       (evil +everywhere); come to the dark side, we have cookies
       (format)  ; automated prettiness
       word-wrap         ; soft wrapping with language-aware indent
       snippets

       :emacs
       (dired +icons)             ; making dired pretty [functional]

       electric          ; smarter, keyword-based electric-indent
       (ibuffer +icons)         ; interactive buffer management
       undo              ; persistent, smarter undo for your inevitable mistakes

       :term
       vterm             ; the best terminal emulation in Emacs

       :checkers
       (syntax +icons)              ; tasing you for every semicolon you forget

       :tools
       (lookup +docsets)              ; navigate your code and its documentation
       lsp               ; M-x vscode
       tree-sitter       ; syntax and parsing, sitting in a tree...
       magit

       :lang
       data              ; config/data formats
       emacs-lisp        ; drown in parentheses
       (json +tree-sitter)              ; At least it ain't XML
       (javascript +lsp +tree-sitter)        ; all(hope(abandon(ye(who(enter(here))))))
       (latex +lsp +tree-sitter)             ; writing papers in Emacs has never been so fun
       (lua +lsp +tree-sitter)               ; one-based indices? one-based indices
       markdown          ; writing docs for people to ignore
       (org +journal)               ; organize your plain life in plain text
       (python +lsp +tree-sitter +pyright +cython)            ; beautiful is better than ugly
       (sh +lsp +tree-sitter)                ; she sells {ba,z,fi}sh shells on the C xor
       (web +lsp +tree-sitter)               ; the tubes
       (yaml +lsp +tree-sitter)              ; JSON, but readable

       :config
       (default +bindings +smartparens))
