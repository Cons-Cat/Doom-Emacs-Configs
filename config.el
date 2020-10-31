;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!
(setq-default
 ;; setq-hook! 'text-mode-hook tab-jump-out-mode t
 ;; (add-hook! 'text-mode-hook 'turn-on-visual-line-mode)
 tab-jump-out-mode t
 tab-width 3
 electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit
 global-visual-line-mode t
 ivy-posframe-mode t
)

;; ;; Magit
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
;; (setq-hook! 'text-mode-hook ivy-posframe-mode t)
;; (ivy-posframe-border ((t (:background "#ffffff"))))

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "William Gooch"
      user-mail-address "wgooch2000@gmail.com")

;; Centaur
(after! centaur-tabs
  :post-config
  (setq
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

;; doom exposes five (optional) variables for controlling fonts in doom. here
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
      doom-big-font (font-spec :family "operator mono lig medium" :size 20)
)
(custom-set-faces!
  '(font-lock-comment-face :slant italic)
  '(font-lock-keyword-face :slant italic)
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
;; XFK is loaded manually, because Doom's package! macro doesn't seem to treat it properly.
(load! "xah-fly-keys.el")
(load! "pony-fly-keys.el")

(after! evil
;; (use-package-hook! evil      ;; This seems to cause a failure to load at start-up, but works after doom sync.
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
   (kbd "d") 'xah-beginning-of-line-or-block
   (kbd "s") 'xah-end-of-line-or-block
   (kbd "D") 'pony-binary-beginning-of-line
   (kbd "S") 'pony-binary-end-of-line

   ;; Deletion
   (kbd "e") 'smart-hungry-delete-backward-char
   (kbd "(") 'smart-hungry-delete-forward-char
   (kbd ".") 'doom/delete-backward-word
   (kbd "p") 'kill-word
   (kbd ">") 'subword-backward-kill
   (kbd "P") 'subword-kill
   (kbd ",") 'xah-shrink-whitespaces

   ;; Selection
   (kbd "*") 'xah-select-line
   (kbd ")") 'er/mark-word
   (kbd "+") 'xah-select-block
   (kbd "]") 'xah-select-text-in-quote

   ;; Clipboard
   (kbd "q") 'xah-cut-all-or-region
   (kbd "j") 'kill-ring-save
   (kbd "k") 'evil-paste-after

   ;; Buffer splitting
   (kbd "{") 'delete-other-windows
   (kbd "}") 'split-window-right
   (kbd "w") 'xah-next-window-or-frame

   ;; Other
   (kbd "'") 'comment-line
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
   (kbd "d") 'xah-beginning-of-line-or-block
   (kbd "s") 'xah-end-of-line-or-block
   (kbd "D") 'pony-binary-beginning-of-line
   (kbd "S") 'pony-binary-end-of-line

   ;; Selection
   (kbd "*") 'xah-select-line
   (kbd ")") 'er/mark-word
   (kbd "+") 'xah-select-block
   (kbd "]") 'xah-select-text-in-quote

   ;; Clipboard
   (kbd "q") 'xah-cut-all-or-region
   (kbd "j") 'kill-ring-save
   (kbd "k") 'evil-paste-after

   ;; Deletion
   (kbd "e") 'smart-hungry-delete-backward-char

   ;; Other
   (kbd "'") 'comment-line
   )

 ;; Leader Key
 (map! :leader "a" 'mark-whole-buffer)
 (map! :leader "s" 'exchange-point-and-mark)
 (map! :leader "d" 'beginning-of-buffer)
 (map! :leader "b" 'end-of-buffer)
 (map! :leader "." 'evil-delete-back-to-indentation)
 (map! :leader "p" 'evil-delete-line)
 ;; (map! :leader ";" 'format!)

 (map! :leader "{" 'delete-window)
 (map! :leader "}" 'split-window-below)
 (map! :leader "tb" 'quit-window)

 ;; Embracing
 (map! :leader "eh" 'xah-insert-brace)
 (map! :leader "et" 'xah-insert-paren)
 (map! :leader "en" 'xah-insert-square-bracket)
 (map! :leader "eg" 'xah-insert-ascii-double-quote)
 (map! :leader "ec" 'xah-insert-ascii-single-quote)

 ;; File Commands
 (map! :leader "ca" 'magit-status)
 (map! :leader "cs" 'save-buffer)
 (map! :leader "c." 'dired)
)
