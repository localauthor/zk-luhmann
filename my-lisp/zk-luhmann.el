;;; zk-luhmann.el --- Support for Luhmann-style IDs in zk  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Grant Rosson

;; Author: Grant Rosson <https://github.com/localauthor>
;; Created: January 25, 2022
;; License: GPL-3.0-or-later
;; Version: 0.1
;; Homepage: https://github.com/localauthor/zk
;; Package-Requires: ((emacs "24.1"))

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

;; Luhmann-style IDs are alphanumeric sequences between curly braces.
;; They look like this: {1,1,b,3 }
;; Note the space after the final character. This is necessary for proper sorting.

;; The ID is part of the file-name, positioned between the zk-id and the
;; title.

;; Because all files with Luhmann-IDs have normal zk-ids, they are normal
;; zk-files. This naming and ID scheme therefore simply offers a different
;; organizing scheme within a zk. It is both fully integrated with zk while
;; being, nevertheless, completely distinct --- a system within a system.

;;; Code:

(require 'zk)
(require 'zk-index)

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
                 (when (string-match "{\\([^ ]*\\)" a)
                   (match-string 1 a)))
                (two
                 (when (string-match "{\\([^ ]*\\)" b)
                   (match-string 1 b))))
            (string< one two)))))

(defun zk-luhmann-completion-at-point ()
  "Completion at point function for notes with Luhmann-IDs."
  (let ((case-fold-search t)
        (pt (point)))
    (save-excursion
      (save-match-data
        (when (re-search-backward "{" nil t)
          (list (match-beginning 0)
                pt
                (zk-luhmann-format-candidates)
                :exclusive 'no))))))

(defun zk-luhmann-files ()
  "List notes with Luhmann-IDs."
  (zk--directory-files t "{"))

(defun zk-luhmann-format-candidates (&optional files)
  "Format completions candidates for FILES with Luhmann-IDs."
  (let* ((files (if files files
                  (zk-luhmann-files)))
         (output))
    (dolist (file files)
      (progn
        (string-match (concat "\\(?1:"
                              zk-id-regexp
                              "\\).\\(?2:.*?\\)\\."
                              zk-file-extension
                              ".*")
                      file)
        (let ((id (match-string 1 file))
              (title (match-string 2 file)))
          (when id
            (push (format-spec "%t [[%i]]"
                               `((?i . ,id)(?t . ,title)))
                  output)))))
    output))

(add-hook 'completion-at-point-functions #'zk-luhmann-completion-at-point 'append)

;;; Luhmann Index

(define-key zk-index-map (kbd "L") #'zk-luhmann-index-sort)
(define-key zk-index-map (kbd "l") #'zk-luhmann-index)
(define-key zk-index-map (kbd "F") #'zk-luhmann-index-forward)
(define-key zk-index-map (kbd "B") #'zk-luhmann-index-back)
(define-key zk-index-map (kbd "T") #'zk-luhmann-index-top)

;;;###autoload
(defun zk-luhmann-index ()
  "Open index for Luhmann-ID notes."
  (interactive)
  (zk-index (zk-luhmann-files) nil 'zk-luhmann-sort))

(defun zk-luhmann-index-sort ()
  "Sort index according to Luhmann-IDs."
  (interactive)
  (zk-index-refresh (zk-index--current-file-list)
                    zk-index-last-format-function
                    #'zk-luhmann-sort))

(defun zk-luhmann-index-top ()
  "Focus on top level Luhmann-ID notes."
  (interactive)
  (zk-index (zk--directory-files t "{[^,] }")
            zk-index-last-format-function
            #'zk-luhmann-sort))

(defun zk-luhmann-index-forward ()
  "Focus on notes that share a particular ID string.
Calling this function on note \"{4,3 }\" will present a listing
of all notes whose Luhmann-IDs begin with \"4,3\", such as
\"{4,3,a }\" and \"{4,3,c,1,d }\". The effect is like descending
\"into\" the archive.

Keep in mind that this function will exclude notes that might be
considered \"adjacent\", in the sense that \"{4,4 }\" is
\"adjacent\" to \"{4,3 }\". This function is therefore a
convenience feature, not a substitute for thinking.

See 'zk-luhmann-index-back' for complementary behavior."
  (interactive)
  (let* ((regexp
          (cond ((eq this-command 'zk-luhmann-index-forward)
                 "{.[^ }]*")
                ((eq this-command 'zk-luhmann-index-back)
                 "{.[^, }]*")))
          (line (buffer-substring
                 (line-beginning-position)
                 (line-end-position)))
          (id (progn
                (string-match regexp line)
                (match-string-no-properties 0 line))))
    (when id
      (zk-index (zk--directory-files t id)
                zk-index-last-format-function
                #'zk-luhmann-sort))))

(defun zk-luhmann-index-back ()
  "Focus on notes that share all but the last ID element.
Calling this function on \"{4,3,a }\ will present a listing of
all notes whose Luhmann IDs begin with \"4,3\". The effect is
like returning back toward the \"top\" of the archive.

See 'zk-luhmann-index-forward' for complementary behavior."
  (interactive)
  (zk-luhmann-index-forward))

(provide 'zk-luhmann)
;;; zk-luhmann.el ends here
