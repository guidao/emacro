;;; emacro.el --- Commands in emacro -*- lexical-binding: t -*-

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;; Implementation for all commands in Meow.

;;; Code:

(defvar emacro-last-value "" "last value")
(defvar emacro-last-elisp-value nil "last elisp value")
(defvar emacro-join-sep "\n" "join sep when return array value")


(defun emacro-start ()
  (interactive)
  (let* ((w (split-window-below))
	 (buf (get-buffer-create "*Edit Eval*")))
    (select-window w)
    (switch-to-buffer buf)
    (erase-buffer)
    (lisp-interaction-mode)
    (emacro-mode)
    (if (string-empty-p emacro-last-value)
	(insert ";;; edit lisp C-c C-c save to emacro-last-value\n")
      	(insert emacro-last-value))))

(defun emacro-apply-to-region-lines (top bottom &optional emacro)
  (interactive "r")
  (save-excursion
    (let ((end-marker (copy-marker bottom))
	  next-line-marker
	  )
      (goto-char top)
      (if (not (bolp))
	  (forward-line 1))
      (setq next-line-marker (point-marker))
      (set-marker-insertion-type next-line-marker t)
      (while (< next-line-marker end-marker)
	(goto-char next-line-marker)
	(save-excursion
	  (forward-line 1)
	  (set-marker next-line-marker (point)))
	(save-excursion
	  (let ((mark-active nil))
          (emacro-execute (point) (marker-position next-line-marker) emacro-last-elisp-value))))
      (set-marker end-marker nil)
      (set-marker next-line-marker nil))
    ))

(defun emacro-execute (min max code)
  (let* ((str (buffer-substring-no-properties min max))
	 (lexical-binding nil)
	  (result (eval emacro-last-elisp-value `((str . ,str)))))
     (save-excursion
       (delete-region min max)
       (insert (cond ((stringp result) result)
		     ((sequencep result) (string-join result emacro-join-sep))
		     (t (format "%s" result)))))))


(defun emacro-save-command ()
  (interactive)
  (let ((buf (get-buffer "*Edit Eval*")))
    (switch-to-buffer buf)
    (setq emacro-last-value (buffer-string))
    (setq emacro-last-elisp-value (read (format "(--> str %s)" (buffer-string))))
    (delete-window)))



(defvar emacro-mode-map
  (let ((keymap (make-keymap)))
    (define-key keymap (kbd "C-c C-c") 'emacro-save-command)
    keymap))

(define-minor-mode emacro-mode
  "emacro edit mode"
  nil
  " [E]"
  emacro-mode-map
  )

(provide 'emacro)
;;; emacro.el ends here



