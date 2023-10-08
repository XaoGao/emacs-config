;;; package --- Custom helper functions
;;; Commentary:

;; Generate ruby objects

;;; Code:

(defun generate-ruby-module (module-name)
  (interactive "sModule name: ")
  (setq segments (split-string module-name "\\."))
  (setq segments-length (length segments))
  (while segments
    (insert (concat "module " (pop segments) "\n" )))
  (dotimes (i segments-length)
    (insert "end\n"))
  (indent-region 1 (point-max)))

(provide 'scripts)

;;; scripts.el ends here
