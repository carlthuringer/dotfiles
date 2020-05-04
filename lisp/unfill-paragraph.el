;;; unfill-paragraph --- Summary
;;; This package provides an unfill-paragraph method, helpful for folks like me who have been using auto-fill-mode too much.

;;; Suggested usage:
;;; (use-package unfill-paragraph :bind (("M-Q" . unfill-paragraph)))

;;; Commentary:
;;; Obtained freely from https://www.emacswiki.org/emacs/UnfillParagraph

;;; Code:

(defun unfill-paragraph (&optional region)
      "Takes a multi-line REGION and make it into a single line of text."
      (interactive (progn (barf-if-buffer-read-only) '(t)))
      (let ((fill-column (point-max))
            ;; This would override `fill-column' if it's an integer.
            (emacs-lisp-docstring-fill-column t))
        (fill-paragraph nil region)))

(provide 'unfill-paragraph)
;;; unfill-paragraph.el ends here
