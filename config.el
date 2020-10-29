;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "William Gooch"
      user-mail-address "wgooch2000@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-dracula)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.

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
(setq display-line-numbers-type 'relative)
(setq tab-bar-mode t)
(setq custom-tab-width 3)
(setq tab-jump-out-mode t)

;; Text prettifying
(setq doom-font (font-spec :family "Operator Mono Lig Medium" :size 9.5)
      doom-big-font (font-spec :family "Operator Mono Lig Medium" :size 20)
)
(custom-set-faces!
  '(font-lock-comment-face :slant italic)
  '(font-lock-keyword-face :slant italic)
)

(global-set-key [backspace] 'evil-normal-state) ;; Backspace
;; This package is loaded manually, because Doom's package! macro doesn't seem to treat it properly.
(load! "xah-fly-keys.el")
(load! "pony-fly-keys.el")
;; (load! "smart-operator.el")

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

   ;; Other
   (kbd "'") 'comment-line
   (kbd "f") 'evil-undo
   (kbd "F") 'evil-redo
   (kbd "q") 'xah-cut-all-or-region
   (kbd "j") 'kill-ring-save
   (kbd "k") 'evil-paste-after
   (kbd "O") 'evil-join
   (kbd "o") 'evil-insert-newline-below
   (kbd ";") 'evil-record-macro
   )

 (evil-declare-key 'visual global-map
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

   ;; Deletion
   (kbd "e") 'smart-hungry-delete-backward-char
   )

 ;; Leader Key
 (map! :leader "a" 'mark-whole-buffer)
 (map! :leader "s" 'exchange-point-and-mark)
)
