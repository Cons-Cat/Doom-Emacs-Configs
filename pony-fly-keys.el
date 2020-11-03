(defun pony-binary-end-of-line()
  (interactive)
  (let (($p (point)))
    (goto-char (+
                (/
                 (- (line-end-position) $p)
                 2)
                (point))
               )
    )
  )

(defun pony-binary-beginning-of-line()
  (interactive)
  (let (($p (point)))
    (goto-char (+
                (/
                 (- $p (line-beginning-position))
                 2)
                (line-beginning-position))
               )
    )
  )

;; https://www.emacswiki.org/emacs/HalfScrolling
  (defun zz-scroll-half-page (direction)
    "Scrolls half page up if `direction' is non-nil, otherwise will scroll half page down."
    (let ((opos (cdr (nth 6 (posn-at-point)))))
      ;; opos = original position line relative to window
      (move-to-window-line nil)  ;; Move cursor to middle line
      (if direction
          (recenter-top-bottom -1)  ;; Current line becomes last
        (recenter-top-bottom 0))  ;; Current line becomes first
      (move-to-window-line opos)))  ;; Restore cursor/point position

  (defun zz-scroll-half-page-down ()
    "Scrolls exactly half page down keeping cursor/point position."
    (interactive)
    (zz-scroll-half-page nil))

  (defun zz-scroll-half-page-up ()
    "Scrolls exactly half page up keeping cursor/point position."
    (interactive)
    (zz-scroll-half-page t))

(defun pony-insert-region-pair()
  (interactive)
  (let ((open "#region\n")
        (close "#endregion\n"))      ; Default kind of region
        (cond ((equal major-mode 'emacs-lisp-mode) (setq open "; region\n") (setq close "; endregion\n")
               )
              ((equal major-mode 'c++-mode) (setq open "#pragma region\n") (setq close "#pragma endregion\n")
               )
              )
        (xah-insert-bracket-pair open close)
  )
  )
