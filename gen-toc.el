;;; gen-toc.el ---                                   -*- lexical-binding: t; -*-

;; Copyleft (C) 2014  kuanyui

;; Author: kuanyui <azazabc123@gmail.com>
;; License: WTFPL 1.0

(defun emacs-tw-gen-toc ()
  (interactive)
  (let* ((fin "")
	 pos)
    (save-excursion
      (goto-char (point-min))
      (replace-regexp "^#\\+BEGIN_QUOTE\n\\*\\{4\\} Table of Contents\n\\(?:.\\|\n\\)*+?#\\+END_QUOTE" "")
      (setq pos (point)))
    (goto-char (point-min))
    (while (re-search-forward "^\\(\\*+\\) \\(.+\\)" nil :no-error)
      (let* ((depth (length (match-string 1)))
	     (name (match-string 2)))
	(setq fin (concat fin (format "%s- [[#%s][%s]]\n"
				      (make-string (* 2 (1- depth)) 32)
				      (replace-regexp-in-string " " "-" (downcase name))
				      name
				      )))))
    (goto-char pos)
    (insert (format "#+BEGIN_QUOTE\n**** Table of Contents\n%s#+END_QUOTE" fin))))
