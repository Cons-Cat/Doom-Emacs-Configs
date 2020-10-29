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
