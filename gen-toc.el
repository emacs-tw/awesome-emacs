;;; gen-toc.el ---                                   -*- lexical-binding: t; -*-
;; Generate Table Of Contents for Awesome-Emacs.

;; Copyleft (C) 2014  kuanyui

;; Author: kuanyui <azazabc123@gmail.com>
;; License: WTFPL 1.0

;; Usage: M-x eval-buffer, then switch to awesome-emacs's README.org, M-x awesome-emacs-gen-toc


(defun awesome-emacs-gen-toc ()
  (interactive)
  (let* ((fin "")
	 pos)
    (save-excursion
      (goto-char (point-min))
      (replace-regexp "^#\\+BEGIN_QUOTE\n\\*Table of Contents\\*\n\\(?:.\\|\n\\)*+?#\\+END_QUOTE" "")
      (setq pos (point)))
    (goto-char (point-min))
    (while (re-search-forward "^\\(\\*+\\) \\(.+\\)" nil :no-error)
      (let* ((depth (length (match-string 1)))
	     (name (match-string 2)))
	(setq fin (concat fin (format "%s- [[#%s][%s]]\n"
				      (make-string (* 2 (1- depth)) 32)
				      (replace-regexp-in-string
				       " " "-"
				       (replace-regexp-in-string
					"[^A-z0-9- ]" "" (downcase name)))
				      name
				      )))))
    (goto-char pos)
    (insert (format "#+BEGIN_QUOTE\n*Table of Contents*\n%s#+END_QUOTE" fin))))
