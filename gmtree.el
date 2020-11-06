(define-derived-mode gmtree-mode special-mode "GMTree"
  "A major mode for displaying a Game Maker project tree."
  (setq indent-tabs-mode nil            ; only spaces
        buffer-read-only t              ; read only
        truncate-lines -1
        )
  )

(add-hook 'gmtree-mode-hook 'centaur-tabs-local-mode)

;; ------------ Custom Variables -------------

(defgroup gmtree nil
  "Options for GMTree."
  :prefix "gmtree-"
  :group 'files
  )

(defconst gmtree-buffer-name "*GMTree*"
  "Name of the buffer where GMTree shows project contents."
  )

(cl-defstruct gmnode text child parent)

(defvar gmtree-node-list (list "Head" '(2 3 4)))

(defcustom gmtree-dock-side 'right
  "The side to dock the GMTree window to."
  :group 'gmtree
  :type '(choice (const left)
                 (const right))
  )

(defcustom gmtree-window-width 25
  "Specifies the width of the GMTree window."
  :type 'integer
  :group 'gmtree
  )

;; ------------ Global Functions -------------

(defun gmtree-get-window()
    (interactive)
    (if (get-buffer gmtree-buffer-name)
        (progn
          ;; Open the GMTree buffer which already exists.
          (display-buffer gmtree-buffer-name)
          (pop-to-buffer gmtree-buffer-name)
          ;; (message (format "%s" (window-dedicated-p)))
          )

      (progn
        ;; Create a window with dedicated buffer
        ;; Also create that buffer.
        ;;
        (let ((window-pos (if (eq gmtree-dock-side 'left) 'left 'right)))
          ;; (display-buffer-in-side-window buffer `((side . ,window-pos)))
          ;;
          (setq buffer (generate-new-buffer gmtree-buffer-name))
          (setq window (display-buffer-in-side-window buffer `(
                                                               (side . ,window-pos)
                                                               (no-other-window . t)          ; Do not appear in window quick-pick.
                                                               (no-delete-other-windows . t)  ; Prevent GMTree from disappearing accidentally.
                                                               (preserve-size . (nil, nil))   ; When closed, reopen with the same size.
                                                               (dedicated . t)                ; Only allow this window to hold one buffer.
                                                               (allow-no-window . t)
                                                               (slot . -1)                    ; Place at the top of side frames.
                                                               )))
          )

        ;; (switch-to-buffer buffer)
        ;; (set-window-parameter window 'no-delete-other-windows t)
        ;; (set-frame-selected-window nil window)
        ;; (gmtree-mode)
        ;;
        ;; (gmtree-set-dock-side)
        (pop-to-buffer gmtree-buffer-name)
        (gmtree-mode)
        (set-window-dedicated-p window t) ; t has a unique effect in this case, strongly-dedicating the window.

        (message "Made Side Window.")
        )
      )
)
