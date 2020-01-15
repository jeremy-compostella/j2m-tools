;;; j2m-tools.el --- Tools function I use on a daily basis

;; Copyright (C) 2014 Jérémy Compostella

;; Author: Jérémy Compostella <jeremy.compostella@gmail.com>
;; Version: 0.1
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(defun increment-number-at-point (&optional n)
  "Increment the number forward from point by N.
Work for decimal and hexadecimal number.  Hexadecimal are
detected by the presence of the '0x' prefix."
  (interactive)
  (let ((n (or n 1))
	(saved-point (point)))
    (skip-chars-backward "[0-9][a-f][A-F]")
    (if (looking-back "0x")
	(progn
	  (or (looking-at "[0-9a-fA-F]+") (error "No number at point"))
	  (let ((value (+ n (string-to-number (match-string 0) 16))))
	    (replace-match (format (concat "%." (number-to-string (length (match-string 0))) "X") value) t)))
      (goto-char saved-point)
      (skip-chars-backward "-0-9")
      (or (looking-at "-?[0-9]+") (error "No number at point"))
      (let ((value (+ n (string-to-number (match-string 0))))
	    (length (if (string= (substring (match-string 0) 0 1) "-")
			(1- (length (match-string 0)))
		      (length (match-string 0)))))
	(replace-match (format (concat "%." (number-to-string length) "d") value))))))

(defun decrement-number-at-point (&optional n)
  "Decrement the number forward from point by N.
See `increment-number-at-point' for more details."
  (interactive)
  (let ((n (or n 1)))
    (increment-number-at-point (- n))))

(defun lines-boundaries ()
  "When no region is active, it returns the current line
beginning and line end points. Otherwise, it returns the active
region beginning and end points of the active region including
whole lines."
  (if (not (use-region-p))
      (list (line-beginning-position) (progn (forward-line 1) (point)))
    (let ((start (region-beginning))
	  (end (region-end)))
      (unless (eq start (progn (goto-char start) (line-beginning-position)))
	(setq start (line-beginning-position)))
      (unless (eq end (progn (goto-char end) (line-beginning-position)))
	(setq end (progn (forward-line 1) (line-beginning-position))))
      (list start end))))

(defun duplicate-lines ()
  "Duplicate the current lines.
When no region is active, it duplicates the current
line. Otherwise, it duplicates the whole lines included by the
active region."
   (interactive)
   (save-excursion (insert (apply 'buffer-substring (lines-boundaries)))))

(defun move-lines (n)
  "Move the current lines up or down by N lines. When no region
is active, it moves the current line. Otherwise, it moves the
whole lines included by the active region and keep it active."
  (interactive "p")
  (let ((column (current-column))
	(lines (apply 'delete-and-extract-region (lines-boundaries))))
    (forward-line n)
    (if (use-region-p)
	(progn (push-mark (point))
	       (insert lines)
	       (setq deactivate-mark nil))
      (save-excursion (insert lines))
      (move-to-column column))))

(defun kill-line-at-point ()
  "Kill the whole line at point."
  (interactive)
  (kill-region (line-beginning-position)
	       (progn (forward-line 1) (point))))

(defun copy-line-at-point ()
  "Put the line at point to the kill ring."
  (interactive)
  (save-excursion
    (apply 'kill-ring-save (lines-boundaries))))

(defun copy-symbol-at-point ()
  "Put the symbol at point string to the kill ring."
  (interactive)
  (if (symbol-at-point)
      (kill-new (format "%s" (symbol-at-point)))
    (error "No symbol at point.")))

(defun copy-word-at-point ()
  "Put the current sexp to the kill ring."
  (interactive)
  (if (word-at-point)
      (kill-new (format "%s" (word-at-point)))
    (error "No word at point.")))

(defvar ssh-history '()
  "History of `ssh' function argument.")

(defun ssh (&optional login-host)
  "Create a new ssh shell on HOST."
  (interactive (list (read-string "login@host: " nil 'ssh-history)))
  (with-current-buffer (get-buffer-create (concat "*" login-host "*"))
    (setq default-directory (concat "/ssh:" login-host ":"))
    (shell (current-buffer))
    ;; Input ring bash history
    (set (make-local-variable 'comint-input-ring-file-name)
	 (concat "/ssh:" login-host ":~/.bash_history"))
    (comint-read-input-ring)))

(defun org-insert-block (type &optional arg)
  (interactive)
  (let ((src (if (use-region-p)
		 (apply 'delete-and-extract-region (lines-boundaries))
	       ""))
	(start (point))
	(end))
    (insert (format "#+begin_%s%s" type (if arg (concat " " arg) "")))
    (save-excursion
      (insert "\n" src (format "#+end_%s" type))
      (setq end (point)))
    (org-indent-region start end)))

(defvar org-src-kill-ring (make-ring 16))

(defun untramp-path (path)
  (if (tramp-tramp-file-p path)
      (tramp-file-name-localname (tramp-dissect-file-name path))
    path))

(defun major-mode-to-name ()
  (let ((mode major-mode))
    (if (eq mode 'magit-status-mode)
	"diff"
      (replace-regexp-in-string "-mode" "" (symbol-name mode)))))

(defun org-src-kill ()
  (interactive)
  (let ((mode (major-mode-to-name))
	(boundaries (lines-boundaries)))
    (ring-insert org-src-kill-ring
		 (list mode
		       (line-number-at-pos (car boundaries))
		       (untramp-path (buffer-file-name))
		       (apply 'buffer-substring-no-properties boundaries)))))

(defun org-src-yank ()
  (interactive)
  (let* ((cur (ring-remove org-src-kill-ring))
	 (header (format "%s%s" (nth 0 cur)
			 (if (nth 1 cur)
			     (format " -n %d" (nth 1 cur) "")))))
    (org-insert-src-block header (nth 2 cur))
    (save-excursion
      (insert "\n")
      (insert (nth 3 cur)))))

(defun org-insert-quote-block ()
  (interactive)
  (org-insert-block "quote"))

(defun org-insert-note-block ()
  (interactive)
  (org-insert-block "note"))

(defun org-insert-src-block (&optional arg caption)
  (interactive)
  (org-insert-block "src" arg)
  (save-excursion
    (back-to-indentation)
    (insert "#+caption:")
    (when caption
      (insert " " caption))
    (indent-new-comment-line)))

(provide 'j2m-tools)

