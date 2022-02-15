;;; zk-luhmann.el --- Support for Luhmann-style IDs in zk  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Grant Rosson

;; Author: Grant Rosson <https://github.com/localauthor>
;; Created: January 25, 2022
;; License: GPL-3.0-or-later
;; Version: 0.3
;; Homepage: https://github.com/localauthor/zk
;; Package-Requires: ((emacs "24.1")(zk "0.2"))

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

(defvar zk-luhmann-id-regex (concat zk-luhmann-id-prefix
                                    "\\([0-9a-zA-Z,]*\\)"
                                    zk-luhmann-id-postfix)
  "Regexp to match Luhmann-IDs.")

;;; Luhmann ID Support

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

(defun zk-luhmann-group-function (cand transform)
  "TRANSFORM each CAND for 'zk-luhmann'."
  (if transform
      (progn
        (string-match (concat "\\(?1:"
                              zk-id-regexp
                              "\\).\\(?2:.*?\\."
                              zk-file-extension
                              ".*\\)")
                      cand)
        (match-string 2 cand))
    "Luhmann Notes"))

(defun zk-luhmann-sort (list)
  "Sort LIST of 'zk-luhmann' candidates or files."
  (sort list
        (lambda (a b)
          (let ((one
		 (when (string-match zk-luhmann-id-regex a)
                   (match-string 1 a)))
                (two
		 (when (string-match zk-luhmann-id-regex b)
                   (match-string 1 b))))
            (string< one two)))))

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
  (interactive)
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
                  id zk-luhmann-id-delimiter "." zk-luhmann-id-postfix))
		((eq this-command 'zk-luhmann-index-unfold)
		 (substring id 0 2)))))
    (when id
      (progn
	(zk-index (zk--directory-files t str)
		  zk-index-last-format-function
		  #'zk-luhmann-sort)
	(goto-char (point-min))
	(re-search-forward id nil t)
	(beginning-of-line)
        (when (eq this-command 'zk-luhmann-index-unfold)
          (pulse-momentary-highlight-one-line nil 'highlight))))
    (cond ((and (eq this-command 'zk-luhmann-index-unfold)
		(string= buffer-string (buffer-string)))
	   (zk-luhmann-index-top))
	  ((and (eq this-command 'zk-luhmann-index-forward)
		(string= buffer-string (buffer-string)))
	   (progn
	     (setq this-command 'zk-luhmann-index-unfold)
	     (zk-luhmann-index-unfold))))))

(defun zk-luhmann-index-back ()
  (interactive)
  (zk-luhmann-index-sort)
  (let* ((buffer-string (buffer-string))
	 (backward-rx (concat zk-luhmann-id-prefix
                              ".[^"
                              zk-luhmann-id-postfix
                              "]*"))
	 (line (buffer-substring (goto-char (point-min))
				 (line-end-position)))
	 (id (progn
	       (string-match backward-rx line)
	       (match-string 0 line)))
	 (sub-id (substring (match-string 0 line) 0 -2)))
    (cond ((eq 2 (length id))
	   (zk-index (zk--directory-files t id)
		     zk-index-last-format-function
		     #'zk-luhmann-sort))
	  (t (progn (zk-index (zk--directory-files
                               t
                               (concat sub-id zk-luhmann-id-postfix
                                       "\\|"
                                       sub-id zk-luhmann-id-delimiter
                                       "." zk-luhmann-id-postfix))
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
  (let* ((char (if (integerp last-command-event)
                   last-command-event
                 (get last-command-event 'ascii-character)))
         (reps (- (- (logand char ?\177) ?0) 1))
         (base-rx (concat zk-luhmann-id-prefix "[0-9]*"))
         (slug ",.")
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

(defun zk-luhmann-index-go-to-current ()
  "Open index with current note at point."
  (interactive)
  "Open ZK-Index buffer and to line of current note."
  (let ((id (zk--current-id)))
    (zk-index (zk-luhmann-files)
              zk-index-last-format-function
              #'zk-luhmann-sort)
    (other-window 1)
    (re-search-forward id nil t)
    (beginning-of-line)
    (pulse-momentary-highlight-one-line nil 'highlight)))

(defun zk-luhmann--lmid-from-tid(id)
  "Retrieve luhmann-id from a given zk ID."
  (let ((title (zk--parse-id 'title id)))
   (when (string-match zk-luhmann-id-regexp title)
                   (match-string 1 title))))

(defun zk-luhmann--inc-num-or-char (c)
  "Increment a sequence of numbers or a character C."
  (let ((nump  nil))
      (if (string-match "[0123456789]+" c)
        (number-to-string (1+ (string-to-number c)))
        ;; handle chars
            (let ((chr (1+ (string-to-char c))))
              ;; Check if a(97)-z(122)->A(65)->Z(90)->Error
              (if (> chr 122)
                  (char-to-string 65) ;; start with capital A again
                (if (and (> chr 90)(< chr 97))
                    (error "Currently implementing only a-zA-Z - Consider branching")
                   (char-to-string chr)))))))

(defun zk-luhmann--lmid-next (lm-id)
"Increments the Luhmann-ID LM-ID."
 (let ((elems (split-string lm-id zk-luhmann-id-delimiter))
       c1 c2)
   (setq c1 (car (last elems)))
   (setq c2 (zk-luhmann--inc-num-or-char c1))
   (setcar (nthcdr (1- (length elems)) elems) c2)
   (concat zk-luhmann-id-prefix
           (string-join elems zk-luhmann-id-delimiter)
           zk-luhmann-id-postfix)))


(defun zk-luhmann--parse-file (target files)
  "Return TARGET, either 'id, 'luhmann-id or 'title, from FILES.
Takes a single file-path, as a string, or a list of file-paths.  A
note's title is understood to be the portion of its filename following
the zk ID in the format 'zk-id-regexp', the Luhmann ID in
'zk-luhmann-id-regexp' and preceding the file extension."
  (let* ((target (pcase target
                   ('id '1)
                   ('luhmann-id '2)
                   ('title '3)))
         (files (if (listp files)
                    files
                  (list files)))
         (return
          (mapcar
           (lambda (file)
       (if (string-match (concat "\\(?1:"
                             zk-id-regexp
                             "\\).\\(?2:"
                             zk-luhmann-id-regexp
                             "\\).\\(?3:.*?\\)\\."
                             zk-file-extension
                             ".*")
                         file)
           (match-string target file)
         nil))
           files)))
    (if (eq 1 (length return))
        (car return)
      return)))

(defun zk-luhmann--lmid-list (&optional str)
  "Return a list of zk Luhmann IDs for notes in 'zk-directory'.
Optional search for regexp STR in note title."
  (let ((files (if str (zk--directory-files t str)
                 (zk--directory-files t))))
    (zk-luhmann--parse-file 'luhmann-id files)))


(defun zk-luhmann--lmid-next-from-tid (tid)
  "Construct the follow-up Luhmann ID following the note with time-derived ID TID."
  (let ((next-lm-id (zk-luhmann--lmid-next
     (zk-luhmann--lmid-from-tid tid))))
  (if (not (zk-luhmann--lmid-list next-lm-id))
      (progn next-lm-id)
    nil)))

(defun zk-luhmann-new-note-header (lm-id title new-id &optional orig-id)
  "Include the Luhmann ID LM-ID in the header of the note.
This is a wrapper to enricht the note header containing the TITLE, the
NEW-ID and optionally the ORIG-ID of the parent note."
  (funcall zk-new-note-header-function (concat (format "%s %s" lm-id title)) new-id orig-id)
)

(defun zk-luhmann-new-follow-note ()
  "Create a new note, insert link at point of creation."
  (interactive)
  (let* ((pref-arg current-prefix-arg)
         (new-id (zk--generate-id))
         (orig-id (ignore-errors (zk--current-id)))
         (text (when (use-region-p)
                 (buffer-substring
                  (region-beginning)
                  (region-end))))
         (title (if (use-region-p)
                    (with-temp-buffer
                      (insert text)
                      (goto-char (point-min))
                      (buffer-substring
                       (point)
                       (line-end-position)))
                  (read-string "Note title (without Luhmann ID): ")))
         (body (when (use-region-p)
                 (with-temp-buffer
                   (insert text)
                   (goto-char (point-min))
                   (forward-line 2)
                   (buffer-substring
                    (point)
                    (point-max)))))
         lm-id)
    ;; Ask for parent note, if it was not called from within a ZK note
    (unless orig-id
      (setq orig-id (zk--id-list (read-string "Luhmann ID of parent note (n,c,..): "))))

    ;; Returns nil, if there is already a follow-up note
    (setq lm-id (zk-luhmann--lmid-next-from-tid orig-id))

    (if (not lm-id)
        ;; Warn if there is already a following note
        (message "%s %s" (propertize "There is already a following note." 'face '(:foreground "red"))
                 (propertize "Consider to branch!" 'face '(:foreground "yellow")))

    (when (use-region-p)
      (kill-region (region-beginning) (region-end)))

    (when (or pref-arg
              (eq zk-new-note-link-insert 't)
              (and (eq zk-new-note-link-insert 'zk)
                   (zk-file-p))
              (and (eq zk-new-note-link-insert 'ask)
                   (y-or-n-p "Insert link at point? ")))
      (zk-insert-link new-id (concat lm-id title)))

    (save-buffer)
    (find-file (concat (format "%s/%s %s %s.%s"
                               zk-directory
                               new-id
                               lm-id
                               title
                               zk-file-extension)))
    (zk-luhmann-new-note-header lm-id title new-id orig-id)
    (when body (insert body))
    (when zk-enable-link-buttons (zk-make-link-buttons))
    (save-buffer))))

(provide 'zk-luhmann)
;;; zk-luhmann.el ends here
