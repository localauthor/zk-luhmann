;;; zk-luhmann.el --- Support for Luhmann-style IDs in zk  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Grant Rosson

;; Author: Grant Rosson <https://github.com/localauthor>
;; Created: January 25, 2022
;; License: GPL-3.0-or-later
;; Version: 0.3
;; Homepage: https://github.com/localauthor/zk
;; Package-Requires: ((emacs "24.4")(zk "0.2")(zk-index "0.4"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Adds support for files with Luhmann-style IDs in zk and zk-index.

;; Luhmann-style IDs are alphanumeric sequences that immediately follow the
;; zk-id in a note's filename. By default, the Luhmann-ID is surrounded by
;; parentheses, with each character in the ID delimited by a comma. A note
;; with such a Luhmann-ID will have a file name that looks something like:

;;       "202012101215 (1,1,a,3,c) The origin of species.md"

;; Because all files with Luhmann-IDs have normal zk-ids, they are normal
;; zk-files. As a result, the naming and ID scheme supported by this package
;; simply offers a different organizing scheme within a zk. It is both fully
;; integrated with zk while being, nevertheless, completely distinct --- a
;; system within a system.

;;; Code:

(require 'zk)
(require 'zk-index)

;;; Variables

(defgroup zk-luhmann nil
  "Luhmann-ID support of zk."
  :group 'text
  :group 'files
  :prefix "zk-luhmann")

(defcustom zk-luhmann-id-prefix "("
  "Character denoting the start of a Luhmann ID."
  :type 'string)

(defcustom zk-luhmann-id-postfix ")"
  "Character denoting the end of a Luhmann ID."
  :type 'string)

(defcustom zk-luhmann-id-delimiter ","
  "Character delimiting a Luhmann ID."
  :type 'string)

(defvar zk-luhmann-id-regexp (concat zk-luhmann-id-prefix
                                    "\\([0-9a-zA-Z"
                                    zk-luhmann-id-delimiter
                                    "]*\\)"
                                    zk-luhmann-id-postfix)
  "Regexp to match Luhmann-IDs.")

;;; Luhmann ID Support

;;;###autoload
(defun zk-luhmann ()
  "Find note with Luhmann-IDs."
  (interactive)
  (let* ((list (zk-luhmann-files))
         (file
          (completing-read
           "Select File: "
           (lambda (string predicate action)
             (if (eq action 'metadata)
                 `(metadata
                   (category . zk-file)
                   (group-function . zk-luhmann-group-function)
                   (display-sort-function . zk-luhmann-sort))
               (complete-with-action action list string predicate))))))
    (find-file file)))

(defun zk-luhmann-group-function (file transform)
  "TRANSFORM each FILE for 'zk-luhmann'."
  (if transform
      (progn
        (string-match zk-file-name-regexp file)
        (match-string 2 file))
    "Luhmann Notes"))

(defun zk-luhmann-sort (list)
  "Sort LIST of 'zk-luhmann' candidates or files."
  (sort list
        (lambda (a b)
          (let* ((a-list
                  (when (string-match zk-luhmann-id-regexp a)
                         (split-string (match-string 1 a)
                                       zk-luhmann-id-delimiter)))
                 (b-list
                  (when (string-match zk-luhmann-id-regexp b)
                         (split-string (match-string 1 b)
                                       zk-luhmann-id-delimiter)))
                 (count 0)
                 (exit)
                 (return))
            (while (and (eq exit nil)
                        (nth count a-list)
                        (nth count b-list))
              (let* ((alpha (nth count a-list))
                     (beta (nth count b-list))
                     (one (if (string-match "\\`[-+]?[0-9]+\\'" alpha)
                              (string-to-number alpha)
                            alpha))
                     (two (if (string-match "\\`[-+]?[0-9]+\\'" beta)
                              (string-to-number beta)
                            beta)))
                (cond ((and (integerp one)
                            (integerp two)
                            (not (eq one two)))
                       (progn
                         (setq exit t)
                         (if (< one two)
                             (setq return t)
                           (setq return nil))))
                      ((and (stringp one)
                            (stringp two)
                            (not (string= one two)))
                       (progn
                         (setq exit t)
                         (if (string< one two)
                             (setq return t)
                           (setq return nil))))
                      ((and (integerp one)
                            (stringp two))
                       (progn
                         (setq exit t)
                         (setq return t)))
                      (t (setq count (+ 1 count))))))
            (cond ((not a-list)
                   nil)
                  ((not b-list)
                   t)
                  ((and (not (nth count a-list))
                        (nth count b-list))
                   t)
                  ((and (not (nth count b-list))
                        (nth count a-list))
                   nil)
                  (t return))))))

(defun zk-luhmann-completion-at-point ()
  "Completion at point function for notes with Luhmann-IDs."
  (let ((case-fold-search t)
        (pt (point)))
    (save-excursion
      (save-match-data
 	(when (re-search-backward zk-luhmann-id-prefix nil t)
          (list (match-beginning 0)
                pt
                (zk-luhmann-format-candidates)
                :exclusive 'no))))))

(defun zk-luhmann-files ()
  "List notes with Luhmann-IDs."
  (zk--directory-files t (concat zk-id-regexp " " zk-luhmann-id-prefix)))

(defun zk-luhmann-format-candidates (&optional files)
  "Format completions candidates for FILES with Luhmann-IDs."
  (let ((files (if files files
                 (zk-luhmann-files))))
    (zk--format-candidates files "%t [[%i]]")))

;;; Luhmann Index

;;;###autoload
(defun zk-luhmann-index ()
  "Open index for Luhmann-ID notes."
  (interactive)
  (zk-index--clear-query-mode-line)
  (zk-index (zk-luhmann-files) nil 'zk-luhmann-sort))

(defun zk-luhmann-index-sort ()
  "Sort index according to Luhmann-IDs."
  (interactive)
  (let ((file-list (zk-index--current-file-list)))
    (when (listp file-list)
      (zk-index-refresh file-list
                        zk-index-last-format-function
                        #'zk-luhmann-sort))))

(defun zk-luhmann-index-top ()
  "Focus on top level Luhmann-ID notes."
  (interactive)
  (let ((buffer-string (buffer-string)))
    (zk-index (zk--directory-files
               t
               (concat zk-luhmann-id-prefix
                       "[^"
                       zk-luhmann-id-delimiter
                       "]*"
                       zk-luhmann-id-postfix))
	      zk-index-last-format-function
	      #'zk-luhmann-sort)
    (when (string= buffer-string (buffer-string))
      (zk-luhmann-index))))

(defun zk-luhmann-index-forward ()
  "Narrow focus to Luhmann notes 'below' note at point."
  (interactive)
  (zk-index--clear-query-mode-line)
  (let* ((buffer-string (buffer-string))
	 (regexp (concat zk-luhmann-id-prefix
                         ".[^"
                         zk-luhmann-id-postfix
                         "]*" ))
	 (line (buffer-substring
		(line-beginning-position)
		(line-end-position)))
	 (id (unless (string= "" line)
	       (unless (string-match regexp line)
                 (error "Not a Luhmann note"))
	       (match-string-no-properties 0 line)))
	 (str
	  (cond ((eq this-command 'zk-luhmann-index-forward)
		 (concat
                  id zk-luhmann-id-postfix "\\|"
                  id zk-luhmann-id-delimiter "..?" zk-luhmann-id-postfix))
		((eq this-command 'zk-luhmann-index-unfold)
                 id))))
    (when id
      (progn
	(zk-index (zk--directory-files t str)
		  zk-index-last-format-function
		  #'zk-luhmann-sort)
	(goto-char (point-min))
	(re-search-forward id nil t)
	(beginning-of-line)))
    (cond ((and (eq this-command 'zk-luhmann-index-unfold)
		(string= buffer-string (buffer-string)))
           (pulse-momentary-highlight-one-line nil 'highlight))
	  ((and (eq this-command 'zk-luhmann-index-forward)
		(string= buffer-string (buffer-string)))
	   (progn
	     (setq this-command 'zk-luhmann-index-unfold)
	     (zk-luhmann-index-unfold))))))

(defun zk-luhmann-index-back ()
  "Expand focus to Luhmann notes 'above' note at point."
  (interactive)
  (unless (re-search-forward zk-luhmann-id-regexp
                             (line-end-position) t)
    (error "Not a Luhmann note"))
  (zk-index--clear-query-mode-line)
  (zk-luhmann-index-sort)
  (let* ((buffer-string (buffer-string))
	 (backward-rx (concat zk-luhmann-id-prefix
                              ".*"
                              zk-luhmann-id-postfix))
	 (line (buffer-substring (goto-char (point-min))
				 (line-end-position)))
	 (id (progn
	       (string-match backward-rx line)
	       (match-string 0 line)))
	 (sub-id (string-trim-right id (concat zk-luhmann-id-delimiter
                                               ".[^"
                                               zk-luhmann-id-delimiter
                                               "]*"
                                               zk-luhmann-id-postfix))))
    (cond ((eq 2 (length id))
	   (zk-index (zk--directory-files t id)
		     zk-index-last-format-function
		     #'zk-luhmann-sort))
	  (t (progn (zk-index (zk--directory-files
                               t
                               (concat sub-id zk-luhmann-id-postfix
                                       "\\|"
                                       sub-id zk-luhmann-id-delimiter
                                       "..?" zk-luhmann-id-postfix))
		              zk-index-last-format-function
		              #'zk-luhmann-sort)
                    (re-search-forward id nil t)
                    (beginning-of-line)
                    (pulse-momentary-highlight-one-line nil 'highlight))))
    (when (string= buffer-string (buffer-string))
      (zk-luhmann-index-top))))

(defun zk-luhmann-index-unfold ()
  "Expand focus to all Luhmann notes, with point on current note."
  (interactive)
  (zk-luhmann-index-forward)
  (recenter-top-bottom))

(defun zk-luhmann-index-level ()
  "Set number of sub-branch levels to view."
  (interactive)
  (zk-index--clear-query-mode-line)
  (let* ((char (if (integerp last-command-event)
                   last-command-event
                 (get last-command-event 'ascii-character)))
         (reps (- (- (logand char ?\177) ?0) 1))
         (base-rx (concat zk-luhmann-id-prefix "[0-9]*"))
         (slug (concat zk-luhmann-id-delimiter "."))
         (new-slug "")
         (regexp
          (progn
            (when reps
              (dotimes (_ reps)
                (setq new-slug (concat new-slug slug))))
            (concat base-rx new-slug zk-luhmann-id-postfix)))
         (current-files (zk--parse-id 'file-path (zk-index--current-id-list)))
         (files (remq nil
                      (mapcar
                       (lambda (x)
                         (when (member x (zk--directory-files t regexp))
                           x))
                       current-files))))
    (zk-index files
              zk-index-last-format-function
              #'zk-luhmann-sort)))

;;;###autoload
(defun zk-luhmann-index-go-to-current ()
  "Open index with current note at point."
  (interactive)
  "Open ZK-Index buffer and to line of current note."
  (zk-index--clear-query-mode-line)
  (let ((id (zk--current-id)))
    (zk-index (zk-luhmann-files)
              zk-index-last-format-function
              #'zk-luhmann-sort)
    (re-search-forward id nil t)
    (beginning-of-line)
    (pulse-momentary-highlight-one-line nil 'highlight)))

(provide 'zk-luhmann)
;;; zk-luhmann.el ends here
