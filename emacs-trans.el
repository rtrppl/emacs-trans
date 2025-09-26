;; emacs-trans.el --- -*- lexical-binding: t -*-

;; Maintainer: René Trappel <rtrappel@gmail.com>
;; URL:
;; Version: 0.1.2
;; Package-Requires: emacs "26"
;; Keywords: search web

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; An Emacs wrapper for translate-shell.
;;
;;; News
;;
;; 0.1.3
;; - Added `emacs-trans-line'
;;
;; 0.1.2
;; -Improved design of translation buffer (q=to kill this buffer);
;; small fixes
;;
;; 0.1.1
;; - Added =emacs-trans-url= to open a URL at point (or otherwise
;;  provided) in Google Translate via `translate-shell'; minor fixes
;;
;; 0.1.
;; - Initial release


;;; Code:
(require 'org) ; for grabbing the link in an org-document
(require 'shr) ; for grabbing the link in an non-org-document

(defvar emacs-trans-show-alternatives "n")
(defvar emacs-trans-show-prompt-message "n")
(defvar emacs-trans-show-languages "n")
(defvar emacs-target-language ":en") ; if nil, default will be used

;;;###autoload
(defun emacs-trans-next-sentence ()
 "Translates the next sentence using translate-shell."
 (interactive)
 (set-mark (point))
 (forward-sentence)
 (let* ((sentence (buffer-substring-no-properties (region-beginning) (region-end)))
	(sentence (replace-regexp-in-string "\n" " " sentence))
	(trans-cmd (emacs-trans-create-trans-cmd))
	(translation (emacs-trans-translate trans-cmd sentence)))
   (deactivate-mark)
   (emacs-trans-open-results-buffer translation)))

;;;###autoload
(defun emacs-trans-line (&optional minimal)
  "Translates the current line using translate-shell."
  (interactive)
  (let* ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
	 (trans-cmd (emacs-trans-create-trans-cmd))
	 (translation))
    (when (not minimal)
      (setq translation (emacs-trans-translate trans-cmd line))
      (emacs-trans-open-results-buffer translation))
    (when minimal
      (setq trans-cmd (emacs-trans-create-trans-cmd t))
      (setq translation (emacs-trans-translate trans-cmd line t)))))

(defun emacs-trans-input ()
 "Translates an the next sentence using translate-shell."
 (interactive)
 (let* ((input (read-string "Input: "))
	(trans-cmd (emacs-trans-create-trans-cmd))
	(translation (emacs-trans-translate trans-cmd input)))
   (emacs-trans-open-results-buffer translation)))

(defun emacs-trans-region (&optional string)
 "Translates the active region."
 (interactive)
 (let* ((region (or
		 string
		 (buffer-substring-no-properties (region-beginning) (region-end))))
	(region (replace-regexp-in-string "\n" " " region))
	(trans-cmd (emacs-trans-create-trans-cmd))
	(translation))
   (when (not string)
     (setq translation (emacs-trans-translate trans-cmd region))
     (emacs-trans-open-results-buffer translation))
   (when string
     (setq trans-cmd (emacs-trans-create-trans-cmd t))
     (setq translation (emacs-trans-translate trans-cmd region)))
   translation))


	
;;;###autoload
(defun emacs-trans-create-trans-cmd (&optional minimal)
  "Creates the correct cmd for translate-shell."
  (let ((cmd "trans"))
    (when emacs-target-language
      (setq cmd (concat cmd " " emacs-target-language)))
    (when (string-equal emacs-trans-show-alternatives "n")
      (setq cmd (concat cmd " -show-alternatives n")))
    (when (string-equal emacs-trans-show-prompt-message "n")
      (setq cmd (concat cmd " -show-prompt-message n")))
    (when (string-equal emacs-trans-show-languages "n")
      (setq cmd (concat cmd " -show-languages n")))
    (when minimal
      (setq cmd (concat "trans " emacs-target-language " -b ")))
    cmd))

(defun emacs-trans-translate (cmd string)
  "Returns translation of STRING."
  (let* ((translation)
	(trans-cmd (concat cmd " " (shell-quote-argument string))))
    (with-temp-buffer
      (insert (shell-command-to-string trans-cmd))
      (setq translation (buffer-string)))
    translation))

(defun emacs-trans-open-results-buffer (translation)
  "Presents the `TRANSLATION' in a results buffer."
  (let ((buffer (get-buffer-create "*emacs-trans*")))
    (with-current-buffer buffer
      (erase-buffer)
      (insert "(q=kill this buffer)\n\n")
      (insert translation)
      (emacs-trans-results-buffer-mode 1))
    (display-buffer buffer)))

(define-minor-mode emacs-trans-results-buffer-mode
  "A minor mode for emacs-trans results buffers."
  :lighter " emacs-trans-results-buffer"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "q") 'emacs-trans-close-buffer)
            map))

(defun emacs-trans-close-buffer ()
   "A wrapper to close BUFFER. "
   (interactive)
   (kill-buffer))

;;;###autoload
(defun emacs-trans-url (&optional url dummy)
  "Opens url in Google Translate."
  (interactive)
  (let ((url (or url
		 (thing-at-point-url-at-point)
		 (get-text-property (point) 'shr-url)
		 (when (derived-mode-p 'org-mode)
		   (org-element-property :raw-link (org-element-context)))
		 (read-string "Please enter a URL: ")))
	(cmd "trans"))
    (when emacs-target-language
      (setq cmd (concat cmd " " emacs-target-language)))
    (shell-command (concat cmd " " url))))

    

;;; emacs-trans.el ends here
