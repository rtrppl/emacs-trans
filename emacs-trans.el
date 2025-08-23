(defvar emacs-trans-show-alternatives "n")
(defvar emacs-trans-show-prompt-message "n")
(defvar emacs-trans-show-languages "n")
(defvar emacs-target-language ":en") ; if nil, default will be used

(defun emacs-trans-next-sentence ()
 "Translates the next sentence using translate-shell."
 (interactive)
 (set-mark (point))
 (forward-sentence)
 (let* ((sentence (buffer-substring-no-properties (region-beginning) (region-end)))
	(trans-cmd (emacs-trans-create-trans-cmd))
	(translation (emacs-trans-translate trans-cmd sentence)))
   (deactivate-mark)
   (emacs-trans-open-results-buffer translation)))

(defun emacs-trans-input ()
 "Translates an the next sentence using translate-shell."
 (interactive)
 (let* ((input (read-string "Input: "))
	(trans-cmd (emacs-trans-create-trans-cmd))
	(translation (emacs-trans-translate trans-cmd input)))
   (emacs-trans-open-results-buffer translation)))

(defun emacs-trans-region ()
 "Translates the active region."
 (interactive)
 (let* ((region (buffer-substring-no-properties (region-beginning) (region-end)))
	(trans-cmd (emacs-trans-create-trans-cmd))
	(translation (emacs-trans-translate trans-cmd region)))
   (emacs-trans-open-results-buffer translation)))
	
(defun emacs-trans-create-trans-cmd ()
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
      (insert translation))
    (next-window)
    (display-buffer buffer)))
