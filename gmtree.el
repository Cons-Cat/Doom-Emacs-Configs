(define-derived-mode gmtree-mode special-mode "GMTree"
  "A major mode for displaying a Game Maker project tree."
  (setq indent-tabs-mode nil            ; only spaces
        buffer-read-only t              ; read only
        truncate-lines -1
        )
  )

;; ------------ Custom Variables -------------
(defgroup gmtree nil
  "Options for GMTree."
  :prefix "gmtree-"
  :group 'files
  )

(defconst gmtree-buffer-name "*GMTree*"
  "Name of the buffer where GMTree shows project contents.")

(defcustom gmtree-dock-side 'right
  "*The position of GMTree window."
  :group 'gmtree
  :type '(choice (const left)
                 (const right)))

(defcustom gmtree-window-width 25
  "*Specifies the width of the GMTree window."
  :type 'integer
  :group 'gmtree)

;; (defcustom gmtree-display-action '(gmtree-dock-side)
;;   "*Action to use for displaying GMTree window.
;; If you change the action so it doesn't use
;; `gmtree-dock-side', then other variables such as
;; `gmtree-window-position' won't be respected when opening NeoTree
;; window."
;;   :type 'sexp
;;   :group 'gmtree)

;; (defvar gmtree-global-buffer nil)
;; (defvar gmtree-global--window nil)

;; ------------ Global Functions -------------

(defun gmtree-open-tree()
  (interactive)
  ;; (set-buffer (gmtree-make-buffer))
  (set-buffer (get-buffer-create "foo"))
    )

(defun gmtree-make-buffer()
  (interactive)
  ;; (get-buffer-create gmtree-buffer-name)
  ;;
  (switch-to-buffer
   (generate-new-buffer-name gmtree-buffer-name))
  ;; (neotree-mode)
  ;; disable linum-mode
  (when (and (boundp 'linum-mode)
             (not (null linum-mode)))
    (linum-mode -1))
  (current-buffer)
  )

(defun gmtree-get-window()
    (interactive)
    (if (get-buffer gmtree-buffer-name)
        (progn
          ;; Return the window.
          ;; (message "Currently, do nothing.")
          ;; (switch-to-buffer-other-window gmtree-buffer-name)
          (display-buffer gmtree-buffer-name)
          (pop-to-buffer gmtree-buffer-name)
          (message (format "%s" (window-dedicated-p)))
          ;; (switch-to-buffer gmtree-buffer-name)
          )

      (progn
        ;; Create a window with dedicated buffer
        ;; Also create that buffer.
        ;;
        ;; (setq window (split-window nil nil 'right))
        ;; (setq buffer (switch-to-buffer (generate-new-buffer-name gmtree-buffer-name)))
        (setq buffer (generate-new-buffer gmtree-buffer-name))
        (setq window (display-buffer-in-side-window buffer '((side . right)
                                                                         (no-other-window . t)          ; Do not appear in window quick-pick.
                                                                         (no-delete-other-windows . t)  ; Prevent GMTree from disappearing accidentally.
                                                                         (preserve-size . (nil, nil))   ; When closed, reopen with the same size.
                                                                         (dedicated . t)                ; Only allow this window to hold one buffer.
                                                                         (mode . gmtree-mode)
                                                                         (allow-no-window . t)
                                                                         (slot . -1)                    ; Place at the top of side frames.
                                                                         )))
        ;; (switch-to-buffer buffer)
        ;; (set-window-parameter window 'no-delete-other-windows t)
        (set-frame-selected-window nil window)
        ;; (set-window-dedicated-p window t) ; t has a unique effect in this case, strongly-dedicating the window.
        ;; (gmtree-mode)
        ;;
        (message "Made Side Window.")
        )
      )
;; (set-window-dedicated-p (get-buffer-window gmtree-buffer-name))
)

;; (defun gmtree-dock-update (buffer _alist)
;;   "Display BUFFER to the left or right of the root window.
;; The side is decided according to `gmtree-dock-side'.
;; The root window is the root window of the selected frame.
;; _ALIST is ignored."
;;   (let ((window-pos (if (eq gmtree-dock-side 'left) 'left 'right)))
;;     (display-buffer-in-side-window buffer `((side . ,window-pos))))
;;   )

;; (defun gmtree-global--get-window (&optional auto-create-p)
;;   "Return the neotree window if it exists, else return nil.
;; But when the neotree window does not exist and AUTO-CREATE-P is non-nil,
;; it will create the neotree window and return it."
;;   (unless (gmtree-global--window-exists-p)
;;     (setf gmtree-global--window nil))
;;   (when (and (null gmtree-global--window)
;;              auto-create-p)
;;     (setq gmtree-global--window
;;           (gmtree-global--create-window)))
;;   gmtree-global--window)

;; (defun gmtree-global--select-window ()
;;   "Select the NeoTree window."
;;   (interactive)
;;   (let ((window (gmtree-global--get-window t)))
;;     (select-window window)))

;; (defun gmtree-global--create-window ()
;;   "Create global GMTree window."
;;   (let ((window nil)
;;         (buffer (gmtree-global--get-buffer t))
;;         )
;;     (setq window
;;           (select-window
;;            (display-buffer buffer gmtree-display-action)
;;            ))
;;     (gmtree-window--init window buffer)
;;     (gmtree-global--attach)
;;     (gmtree-global--reset-width)
;;     window
;;     )
;;   )

;; (defun gmtree-global--window-exists-p ()
;;   "Return non-nil if neotree window exists."
;;   (and (not (null (window-buffer gmtree-global--window)))
;;        (eql (window-buffer gmtree-global--window) (gmtree-global--get-buffer))
;;        )
;;   )

;; (defun gmtree-global--get-buffer (&optional init-p)
;;   "Return the global GMTree buffer if it exists.
;; If INIT-P is non-nil and global GMTree buffer not exists, then create it."
;;   (unless (equal (buffer-name gmtree-global--buffer)
;;                  gmtree-buffer-name)
;;     (setf gmtree-global--buffer nil))
;;   (when (and init-p
;;              (null gmtree-global--buffer))
;;     (save-window-excursion
;;       (setq gmtree-global--buffer
;;             (gmtree-buffer--create))))
;;   gmtree-global--buffer)

;; (defun gmtree-buffer--create ()
;;   "Create and switch to GMTree buffer."
;;   (switch-to-buffer
;;    (generate-new-buffer-name gmtree-buffer-name))
;;   (gmtree-mode)
;;   ;; disable linum-mode
;;   (when (and (boundp 'linum-mode)
;;              (not (null linum-mode)))
;;     (linum-mode -1))
;;   )
