(defun surround-bracket ()
  (interactive)
  (when (use-region-p)
    (let ((beg (region-beginning))
          (end (region-end)))
      (goto-char beg)
      (insert "(")
      (goto-char end)
      (forward-char)
      (insert ")"))))

(defun surround-quote ()
  (interactive)
  (when (use-region-p)
    (let ((beg (region-beginning))
          (end (region-end)))
      (goto-char beg)
      (insert "\"")
      (goto-char end)
      (forward-char)
      (insert "\""))))

(defun ar-format-r-file ()
  "Test."
  (interactive)
  (let (ar-buffer-file-name)
    (setq ar-buffer-file-name buffer-file-name)
    ;;(message "%s" ar-buffer-file-name)))
    (shell-command (format "format-r-file %s" ar-buffer-file-name))
    (revert-buffer :ignore-auto :noconfirm)))

(defun ar-quarto-render ()
  "Test."
  (interactive)
  (let (ar-buffer-file-name)
    (setq ar-buffer-file-name buffer-file-name)
    (call-process-shell-command (format "quarto render %s" ar-buffer-file-name))))

;;; helper.el ends here
