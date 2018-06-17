;;; cycle-dir1.el --- Cycle the files of a directory sorted by some customizable predicate (default: cycle-dir-file-older-than-file-p)  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Tobias Zawada

;; Author: Tobias Zawada <naehring@smtp.1und1.de>
;; Keywords: files

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

;;; Commentary:

;; Add `(require 'cycle-dir)` to your init file.
;; Call emacs with the option --oldest-in-dir with a directory name.
;; The oldest file in that directory is found at startup and `cycle-dir-global-mode' is entered.
;; That mode adds two buttons Cycle Dir Next and Cycle Dir Prev to the toolbar.
;; You can change the sorting predicate with the option `--cycle-dir-predicate`
;; and filter the file names considered as Prev and Next by a regexp as value of the option `--cycle-dir-filter`.

;;; Code:

(require 'cl-macs)

(defconst cycle-dir-option-name "-oldest-in-dir"
  "Command line option for finding the oldest regular file
in the given directory.")

(defconst cycle-dir-predicate-option-name "-cycle-dir-predicate"
  "Command line option for setting the `cycle-dir-predicate'.
Expects a function with two file name arguments.
See `cycle-dir-predicate'.")

(defconst cycle-dir-filter-option-name "-cycle-dir-filter"
  "Command line option for setting the `cycle-dir-filter'.
The option value is a regular expression for filtering the file names.
See `cycle-dir-filter'.")

(defun cycle-dir-file-older-than-file-p (file1 file2)
  "Return t if file FILE1 is older than file FILE2.
If FILE2 does not exist, the answer is nil;
otherwise, if FILE1 does not exist, the answer is t."
  (file-newer-than-file-p file2 file1))

(defcustom cycle-dir-predicate #'cycle-dir-file-older-than-file-p
  "Function that takes two file name arguments.
It should return t if the first file should come before the second
when cycling."
  :type 'function
  :group 'cycle-dir)

(defcustom cycle-dir-filter nil
  "Regular expression for filtering the file names for cycling
or nil if all regular files should be cycled."
  :type 'regexp
  :group 'cycle-dir)

(setq inhibit-startup-screen t)

(defun cycle-dir-files (&optional dir)
  "Return list of files in directory DIR.
DIR defaults to `default-directory'.
The list is sorted by modification time from the newest to the oldest."
  (unless dir
    (setq dir default-directory))
  (when (and (stringp dir)
	     (file-directory-p dir))
    (sort (cl-remove-if-not
	   #'file-regular-p
	   (directory-files dir t cycle-dir-filter t))
	  cycle-dir-predicate)))

(defun cycle-dir-next-name (inc)
  "Get the next INC-th file name for `cycle-dir-next'.
See `cycle-dir-next' for the meaning of INC.
Return nil if there is no next file."
    (let* ((file-name (buffer-file-name))
	   (files (cycle-dir-files))
	   (n (cl-position file-name files :test #'string-equal))
	   (n+inc (and n (+ n inc))))
      (and n (> n+inc 0) (nth n+inc files))))

(defun cycle-dir-next (&optional inc)
  "Replace the current buffer visiting a file with one visiting INC-th next file.
INC is an integer with default value 1.
For positive INC the next file is the INC-th next younger file.
For negative INC the next file is the INC-th next older file.
Kill current buffer if it is not modified."
  (interactive "p")
  (unless inc (setq inc 1))
  (let ((next (cycle-dir-next-name inc))
	(buf (current-buffer)))
    (when next
      (find-file next)
      (unless (buffer-modified-p buf)
	(kill-buffer buf)))))

(defun cycle-dir-prev ()
  "Call `cycle-dir-next' with inc value -1."
  (interactive)
  (cycle-dir-next -1))

(define-minor-mode cycle-dir-mode
  "Show buttons to get to the next newer or older file." nil nil nil
  (setq-local tool-bar-map tool-bar-map)
  (tool-bar-local-item "prev-node" 'cycle-dir-prev 'cycle-dir-prev tool-bar-map :enable '(cycle-dir-next-name -1) :visible 'cycle-dir-mode)
  (tool-bar-local-item "next-node" 'cycle-dir-next 'cycle-dir-next tool-bar-map :enable '(cycle-dir-next-name 1) :visible 'cycle-dir-mode))

(defun cycle-dir-mode-on ()
  "Turn `cycle-dir-mode' on for buffers associated with files."
  (when (buffer-file-name)
    (cycle-dir-mode)))

(define-global-minor-mode cycle-dir-global-mode cycle-dir-mode cycle-dir-mode-on)

(defvar cycle-dir-cmd-line-option-hook #'cycle-dir-global-mode)

(defun cycle-dir-cmd-line-option (switch)
  "Fetch arg of command line options -cycle-dir-predicate and -oldest-in-dir from `command-switch-alist'."
  (cond
   ((string-equal switch cycle-dir-predicate-option-name)
    (cl-assert command-line-args-left nil "Missing predicate for option -%s" cycle-dir-predicate-option-name)
    (setq cycle-dir-predicate (read (car command-line-args-left))
	  command-line-args-left (cdr command-line-args-left))
    t)
   ((string-equal switch cycle-dir-filter-option-name)
    (cl-assert command-line-args-left nil "Missing regular expression for option -%s" cycle-dir-filter-option-name)
    (setq cycle-dir-filter (car command-line-args-left)
	  command-line-args-left (cdr command-line-args-left))
    t)
   ((string-equal switch cycle-dir-option-name)
    (cl-assert command-line-args-left nil "Missing directory name for option -%s" cycle-dir-option-name)
    (let ((dir-name (car command-line-args-left)))
      (setq command-line-args-left (cdr command-line-args-left))
      (cl-assert (file-directory-p dir-name) nil "Value \"%s\" for option -%s is not a directory." dir-name cycle-dir-option-name)
      (let ((files (cycle-dir-files dir-name)))
	(cl-assert files nil "Directory \"%s\" for option -%s does not contain any regular files." dir-name cycle-dir-option-name)
	(find-file (car files))))
    (run-hooks 'cycle-dir-cmd-line-option-hook)
    t)))

(add-to-list 'command-switch-alist (cons cycle-dir-option-name #'cycle-dir-cmd-line-option))
(add-to-list 'command-switch-alist (cons cycle-dir-predicate-option-name #'cycle-dir-cmd-line-option))
(add-to-list 'command-switch-alist (cons cycle-dir-filter-option-name #'cycle-dir-cmd-line-option))

(provide 'cycle-dir)
;;; cycle-dir.el ends here
