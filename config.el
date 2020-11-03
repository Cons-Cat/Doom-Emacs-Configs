;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!
;; (setq! tab-width 3)
(setq-default
 delete-by-moving-to-trash t
 tab-width 3
 x-stretch-cursor t
 )

                                        ; (electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)
(display-time-mode t)
(global-visual-line-mode t)
(global-subword-mode t)
(format-all-mode t)
(rainbow-delimiters-mode t)

;; Code formatting
;; (use-package! format-all
;; :hook (prog-mode . 'format-all-mode)
;; :init
;; :config
;; )

;; Sublimity
(sublimity-mode t)
(use-package! sublimity-scroll
  :config
  (setq sublimity-scroll-weight 2
        sublimity-scroll-drift-length 1)
  )

;; ;; Magit
;;
;; This block gives Magit a pop-up frame.
;; (defun magit-display-buffer-pop-up-frame (buffer)
;;   (if (with-current-buffer buffer (eq major-mode 'magit-status-mode))
;;       (display-buffer buffer
;;                       '((display-buffer-reuse-window
;;                          display-buffer-pop-up-frame)
;;                         (reusable-frames . t)))
;;     (magit-display-buffer-traditional buffer)))
;; (setq magit-display-buffer-function #'magit-display-buffer-pop-up-frame)

;; Ivy
(setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-point)))
(use-package! ivy-posframe
  :hook (ivy-mode . ivy-posframe-mode)
  :init
  :config
  (setq!
   cursor-type 'bar
   )
  )
(setq ivy-posframe-parameters
      '((left-fringe . 8)
	(right-fringe . 8)))

;; Tab Jump-Out
(use-package! tab-jump-out
  ;; :hook (text-mode . tab-jump-out-mode)
  :init (tab-jump-out-mode)
  )
;; (setq yas-fallback-behavior '(apply tab-jump-out 1))

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "William Gooch"
      user-mail-address "wgooch2000@gmail.com")

;; Centaur
(use-package! centaur-tabs
  :init
  (centaur-tabs-mode)
  :config
  (setq!
   centaur-tabs-style "wave"
   centaur-tabs-set-bar 'under
   centaur-tabs-set-icons t
   centaur-tabs-icon-scale-factor 0.65
   centaur-tabs-set-modified-marker nil
   centaur-tabs-set-close-button nil
   centaur-tabs-gray-out-icons 'buffer
   centaur-tabs-bar-height 48
   )
  (centaur-tabs-change-fonts "InputSerifCompressed Black" 90)
  )

;; Modeline
(setq doom-modeline-height 0)           ; Modeline should be no taller than its text.
;; Much of the following is taken from: https://github.com/sunnyhasija/Academic-Doom-Emacs-Config
;; Display battery on laptops.
(unless (equal "Battery status not available"
               (battery))
  (display-battery-mode 1))
;; Display encoding when not UTF-8.
(defun doom-modeline-conditional-buffer-encoding ()
  (setq-local doom-modeline-buffer-encoding
              (unless (or (eq buffer-file-coding-system 'utf-8-unix)
                          (eq buffer-file-coding-system 'utf-8)))))
(add-hook 'after-change-major-mode-hook #'doom-modeline-conditional-buffer-encoding)
;; Hide the Windows titlebar
(setq default-frame-alist '((undecorated . t)))
;; Display current project
(setq doom-modeline-project-detection 'project)
(setq doom-modeline-env-version t)
(setq doom-modeline-enable-word-count nil)
(setq doom-modeline-modal-icon t)

;; doom exposes five (op:barctional) variables for controlling fonts in doom. here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; they all accept either a font-spec, font string ("input mono-12"), or xlfd
;; font string. you generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))
(setq doom-font (font-spec :family "operator mono lig medium" :size 9.5)
      doom-big-font (font-spec :family "operator mono lig medium" :size 13.5)
      doom-unicode-font (font-spec :family "overpass mono bold" :size 8)
      doom-variable-pitch-font (font-spec :family "InputSerifCompressed Bold" :size 8.6)
      )

(custom-set-faces!
  '(font-lock-comment-face :slant italic)
  '(font-lock-keyword-face :slant italic)
  '(mode-line :family "InputSerifCompressed Black" :height 0.9)
  '(mode-line-inactive :family "InputSerifCompressed Black" :height 0.9)
  '(ivy-posframe-border :background "violet")
  '(whitespace-tab :foreground "#282A36" :background nil)     ; Do not highlight indentation.
  '(whitespace-space :background "#282A36" :foreground "#282A36")
  '(whitespace-newline :foreground "#282A36")
  ;; '(ivy-posframe-cursor :cursor-color "FFFFFF")
  )

;; Whitespace
(global-whitespace-mode t)
(use-package! whitespace
  :config
                                        ; (progn
  ;; Make whitespace-mode with very basic background coloring for whitespaces.
  ;; http://ergoemacs.org/emacs/whitespace-mode.html
  (setq
   whitespace-style '(face spaces space-mark tabs tab-mark newline newline-mark trailing)
   ;; Make whitespace-mode and whitespace-newline-mode use “↵" for end of line char and “⇥" for tab.
   whitespace-display-mappings
   ;; all numbers are unicode codepoint in decimal. e.g. (insert-char 182 1)
   '(
     (space-mark 32 [183] [46]) ; SPACE 32 「 」, 183 MIDDLE DOT 「·」, 46 FULL STOP 「.」
     ;; (newline-mark 10 [8629 30]) ; LINE FEED,
     (newline-mark 10 [?↵ 10])
     ;; (tab-mark 9 [8677 9] [92 30]) ; tab
     (tab-mark ?\t [?⇥ ?\t])
     ))
  )

(setq default-tab-width 2)              ; My favored width is 3. 1 is subtracted to fix incompatibility between whitespace-mode and highlight-indent-guides mode.
(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'cperl-indent-level 'tab-width)

(use-package! highlight-indent-guides
  :hook (prog-mode . highlight-indent-guides-mode)
  :init
  :config
  (setq
   highlight-indent-guides-method 'bitmap
   highlight-indent-guides-bitmap-function 'highlight-indent-guides--bitmap-line
   highlight-indent-guides-responsive 'stack
   highlight-indent-guides-delay 0
   )
  )

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-dracula)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

;; Here are some additional functions/macros that could help you configure Doom:
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
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
(fset 'evil-visual-update-x-selection 'ignore)

;; Loading packages
(global-set-key [backspace] 'evil-normal-state) ;; Backspace
(load! "pony-fly-keys.el")
(load! "nav-flash.el")
;; The following are loaded manually, because Doom's package! macro doesn't seem to treat it properly.
(load! "xah-fly-keys.el")

;; Neotree
(after! neotree
  :post-config
  (evil-declare-key 'normal neotree-mode-map
    (kbd "c") 'neotree-previous-line
    (kbd "t") 'neotree-next-line
    )
  )

(after! evil
  :post-config
  (setq evil-move-beyond-eol t)
  (setq evil-cross-lines t)
  (setq evil-move-cursor-back nil)

  (evil-declare-key 'normal global-map
    ;; Modes
    (kbd "u") 'evil-insert-state
    (kbd "a") 'counsel-M-x
    (kbd "y") 'evil-visual-state

    ;; Navigation
    (kbd "H") 'subword-backward
    (kbd "h") 'backward-word
    (kbd "N") 'subword-forward
    (kbd "n") 'forward-word
    (kbd "r") 'evil-forward-char
    (kbd "g") 'evil-backward-char
    (kbd "t") 'evil-next-line
    (kbd "c") 'evil-previous-line
    (kbd "T") 'next-line
    (kbd "C") 'previous-line
    (kbd "d") 'xah-beginning-of-line-or-block
    (kbd "s") 'xah-end-of-line-or-block
    (kbd "D") 'pony-binary-beginning-of-line
    (kbd "S") 'pony-binary-end-of-line
    (kbd "m") 'xah-backward-left-bracket
    (kbd "v") 'xah-forward-right-bracket

    ;; Deletion
    (kbd "e") 'smart-hungry-delete-backward-char
    (kbd "(") 'smart-hungry-delete-forward-char
    (kbd ".") 'doom/delete-backward-word
    (kbd "p") 'kill-word
    (kbd ">") 'subword-backward-kill
    (kbd "P") 'subword-kill
    (kbd ",") 'xah-shrink-whitespaces

    ;; Selection
    (kbd "*") 'pony-mark-line
    (kbd ")") 'er/mark-word
    (kbd "+") 'xah-select-block
    (kbd "]") 'xah-select-text-in-quote

    ;; Clipboard
    (kbd "q") 'xah-cut-all-or-region
    (kbd "j") 'pony-copy-current-word
    (kbd "k") 'xah-paste-or-paste-previous

    ;; Buffer splitting
    (kbd "{") 'delete-other-windows
    (kbd "}") 'split-window-right
    (kbd "w") 'xah-next-window-or-frame

    ;; Other
    (kbd "'") 'xah-comment-dwim
    (kbd "f") 'evil-undo
    (kbd "F") 'evil-redo
    (kbd "O") 'evil-join
    (kbd "o") 'evil-insert-newline-below
    (kbd ";") 'evil-record-macro
    )

  (evil-declare-key 'visual global-map
    ;; Modes
    (kbd "u") 'evil-insert-state
    (kbd "a") 'counsel-M-x
    (kbd "y") 'evil-visual-state

    ;; Navigation
    (kbd "H") 'subword-backward
    (kbd "h") 'backward-word
    (kbd "N") 'subword-forward
    (kbd "n") 'forward-word
    (kbd "r") 'evil-forward-char
    (kbd "g") 'evil-backward-char
    (kbd "t") 'evil-next-line
    (kbd "c") 'evil-previous-line
    (kbd "T") 'next-line
    (kbd "C") 'previous-line
    (kbd "d") 'xah-beginning-of-line-or-block
    (kbd "s") 'xah-end-of-line-or-block
    (kbd "D") 'pony-binary-beginning-of-line
    (kbd "S") 'pony-binary-end-of-line
    (kbd "m") 'xah-backward-left-bracket
    (kbd "v") 'xah-forward-right-bracket

    ;; Selection
    (kbd "*") 'pony-mark-line
    (kbd ")") 'er/mark-word
    (kbd "+") 'xah-select-block
    (kbd "]") 'xah-select-text-in-quote

    ;; Clipboard
    (kbd "q") 'xah-cut-all-or-region
    (kbd "j") 'kill-ring-save
    (kbd "k") 'xah-paste-or-paste-previous

    ;; Deletion
    (kbd "e") 'evil-delete

    ;; Other
    (kbd "'") 'comment-line
    (kbd "y") 'evil-exit-visual-state
    )

  ;; Leader Key
  (map! :leader "a" 'mark-whole-buffer)
  (map! :leader "b" 'exchange-point-and-mark)
  (map! :leader "d" 'beginning-of-buffer)
  (map! :leader "s" 'end-of-buffer)
  (map! :leader "." 'evil-delete-back-to-indentation)
  (map! :leader "p" 'evil-delete-line)
  (map! :leader "x" 'swiper)
  ;; (map! :leader ";" 'format!)

  (map! :leader "{" 'delete-window)
  (map! :leader "}" 'split-window-below)
  (map! :leader "tb" 'quit-window)

  ;; Scrolling
  (map! :leader "tt" 'evil-scroll-page-down)
  (map! :leader "tT" 'zz-scroll-half-page-down)
  (map! :leader "tc" 'evil-scroll-page-up)
  (map! :leader "tC" 'zz-scroll-half-page-up)
  (map! :leader "tu" 'evil-scroll-line-to-center)
  (map! :leader "te" 'move-to-window-line-top-bottom)

  ;; Embracing
  (map! :leader "eh" 'xah-insert-brace)
  (map! :leader "et" 'xah-insert-paren)
  (map! :leader "en" 'xah-insert-square-bracket)
  (map! :leader "eg" 'xah-insert-ascii-double-quote)
  (map! :leader "ec" 'xah-insert-ascii-single-quote)
  (map! :leader "er" 'pony-insert-region-pair)
  ;; (map! :leader "el" 'xah-insert-angle-bracket) ; These are not <> brackets.

  ;; File Commands
  (map! :leader "ca" 'magit-status)
  (map! :leader "cs" 'evil-save)
  (map! :leader "c." 'dired)
  (map! :leader "cn" 'xah-new-empty-buffer)
  (map! :leader "ch" 'neotree-toggle)
  )
