;;; skratch.el --- Manage persistent scratch buffers -*- lexical-binding: t; -*-

;; Author: ZeStig <zestig@duck.com>
;; Maintainer: ZeStig <zestig@duck.com>
;; Version: 0.1
;; Package-Requires: ((emacs "27.1"))
;; Keywords: convenience, scratch, sessions
;; URL: https://github.com/zstg/skratch

;;; Commentary:

;; `skratch' allows you to manage persistent Emacs scratch buffers that are
;; automatically saved and can be recovered or switched between.
;; It supports:
;; - Automatic saving of scratch buffers
;; - Creating new numbered scratch buffers
;; - Opening the latest or a previously saved scratch buffer

;;; Code:

(defvar skratch-save-dir (expand-file-name "~/.config/emacs/.scratch/")
  "Directory to save scratch buffers.")

(defun skratch--save-scratch-buffers-silently ()
  "Save all scratch buffers silently to files in `skratch-save-dir'.
Create the directory if it does not exist."
  (unless (file-directory-p skratch-save-dir)
    (make-directory skratch-save-dir t))
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and (string-match-p "^\\*scratch[0-9]*\\*$" (buffer-name buf))
                 (not (buffer-file-name buf)))
        (let* ((filename (concat skratch-save-dir
                                 (replace-regexp-in-string "[*]" "" (buffer-name buf)))))
          (with-temp-file filename
            (insert-buffer-substring buf)))))))

;; Auto-save every 2 seconds of idle time
(run-with-idle-timer 2 2 #'skratch--save-scratch-buffers-silently)

;;;###autoload
(defun skratch-create ()
  "Create a new numbered scratch buffer."
  (interactive)
  (let* ((max-num (or (skratch--get-highest-number) 0))
         (new-num (1+ max-num))
         (bufname (format "*scratch%d*" new-num)))
    (switch-to-buffer (get-buffer-create bufname))
    (when (= new-num 0)
      (funcall initial-buffer-mode))))

(defun skratch--get-highest-number ()
  "Return the highest scratch number found in `skratch-save-dir`."
  (when (file-directory-p skratch-save-dir)
    (let* ((files (directory-files skratch-save-dir nil "^scratch[0-9]*$"))
           (numbers (mapcar (lambda (f)
                              (if (string= f "scratch")
                                  0
                                (string-to-number (substring f 7))))
                            files)))
      (if numbers (apply #'max numbers) 0))))

;;;###autoload
(defun skratch-open ()
  "Prompt to open a saved scratch buffer."
  (interactive)
  (unless (file-directory-p skratch-save-dir)
    (user-error "Scratch save directory does not exist: %s" skratch-save-dir))
  (let* ((files (directory-files skratch-save-dir nil "^scratch[0-9]*$"))
         (choice (completing-read "Open saved scratch: " files nil t)))
    (when choice
      (let* ((filepath (expand-file-name choice skratch-save-dir))
             (bufname (format "*%s*" choice)))
        (if (get-buffer bufname)
            (switch-to-buffer bufname)
          (switch-to-buffer (get-buffer-create bufname))
          (erase-buffer)
          (insert-file-contents filepath)
          (goto-char (point-min)))))))

;;;###autoload
(defun skratch-open-latest ()
  "Open the scratch buffer with the highest number from saved files."
  (interactive)
  (let* ((max-num (skratch--get-highest-number))
         (bufname (format "*scratch%d*" max-num)))
    (if (get-buffer bufname)
        (switch-to-buffer bufname)
      (let ((filepath (expand-file-name (format "scratch%d" max-num) skratch-save-dir)))
        (if (file-exists-p filepath)
            (progn
              (switch-to-buffer (get-buffer-create bufname))
              (erase-buffer)
              (insert-file-contents filepath))
          (message "No scratch buffer or file found with number %d" max-num))))))

;;;###autoload
(defun skratch-open-recent ()
  "Open the most recently used scratch buffer."
  (interactive)
  (let ((buffers (seq-filter (lambda (buf)
                               (string-match-p "^\\*scratch[0-9]*\\*$" (buffer-name buf)))
                             (buffer-list))))
    (if buffers
        (switch-to-buffer (car buffers))
      (scratch-buffer))))

(provide 'skratch)

;;; skratch.el ends here
