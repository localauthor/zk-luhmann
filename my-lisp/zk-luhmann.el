;;; zk-luhmann.el --- Support for Luhmann-style IDs in zk  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Grant Rosson

;; Author: Grant Rosson <https://github.com/localauthor>
;; Created: January 25, 2022
;; License: GPL-3.0-or-later
;; Version: 0.2
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
  (let ((files (if files files
                 (zk-luhmann-files))))
    (zk--format-candidates files "%t [[%i]]")))

(add-hook 'completion-at-point-functions #'zk-luhmann-completion-at-point 'append)


;;; Luhmann Index

(define-key zk-index-map (kbd "L") #'zk-luhmann-index-sort)
(define-key zk-index-map (kbd "l") #'zk-luhmann-index)
(define-key zk-index-map (kbd "C-f") #'zk-luhmann-index-forward)
(define-key zk-index-map (kbd "C-b") #'zk-luhmann-index-back)
(define-key zk-index-map (kbd "C-t") #'zk-luhmann-index-unfold)
(define-key zk-index-map (kbd "t") #'zk-luhmann-index-top)
(define-key zk-index-map (kbd "t") #'zk-luhmann-index-level)
(define-key zk-index-map (kbd "1") #'zk-luhmann-index-level)
(define-key zk-index-map (kbd "2") #'zk-luhmann-index-level)
(define-key zk-index-map (kbd "3") #'zk-luhmann-index-level)
(define-key zk-index-map (kbd "4") #'zk-luhmann-index-level)
(define-key zk-index-map (kbd "5") #'zk-luhmann-index-level)
(define-key zk-index-map (kbd "6") #'zk-luhmann-index-level)
(define-key zk-index-map (kbd "7") #'zk-luhmann-index-level)
(define-key zk-index-map (kbd "8") #'zk-luhmann-index-level)
(define-key zk-index-map (kbd "9") #'zk-luhmann-index-level)


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
    (zk-index (zk--directory-files t "{[^,] }")
              zk-index-last-format-function
              #'zk-luhmann-sort)
    (when (string= buffer-string (buffer-string))
      (zk-luhmann-index))))

(defun zk-luhmann-index-forward ()
  (interactive)
  (let* ((buffer-string (buffer-string))
         (regexp "{.[^ }]*")
         (line (buffer-substring
                (line-beginning-position)
                (line-end-position)))
         (id (unless (string= "" line)
               (progn
                 (string-match regexp line)
                 (match-string-no-properties 0 line))))
         (str
          (cond ((eq this-command 'zk-luhmann-index-forward)
                 (concat id " \\|" id ",. [^ }]*"))
                ((eq this-command 'zk-luhmann-index-unfold)
                 (substring id 0 2)))))
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
         (line (buffer-substring (goto-char (point-min))
                                 (line-end-position)))
         (id (progn
               (string-match "{.[^ ]*" line)
               (match-string 0 line)))
         (sub-id (substring (match-string 0 line) 0 -2)))
    (cond ((eq 2 (length id))
            (zk-index (zk--directory-files t id)
                      zk-index-last-format-function
                      #'zk-luhmann-sort))
          (t (zk-index (zk--directory-files t (concat sub-id " \\|" sub-id ",. [^ }]*"))
                       zk-index-last-format-function
                       #'zk-luhmann-sort)))
    (when (string= buffer-string (buffer-string))
      (zk-luhmann-index-top))))

(defun zk-luhmann-index-unfold ()
  (interactive)
  (zk-luhmann-index-forward)
  (recenter-top-bottom 0))

(defun zk-luhmann-index-level ()
  (interactive)
  (let* ((char (if (integerp last-command-event)
                   last-command-event
                 (get last-command-event 'ascii-character)))
         (reps (- (- (logand char ?\177) ?0) 1))
         (base-rx "{[0-9]*")
         (slug ",.")
         (new-slug "")
         (regexp
          (progn
            (when reps
              (dotimes (_ reps)
                (setq new-slug (concat new-slug slug))))
            (concat base-rx new-slug " ")))
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

;; (defun zk-luhmann-index-level (arg)
;;   (interactive)
;;   (let* ((arg (if current-prefix-arg
;;                    (- current-prefix-arg 1)
;;                 arg))
;;          (base-rx "{[0-9]*")
;;          (slug ",.")
;;          (new-slug "")
;;          (regexp
;;           (progn
;;             (when arg
;;               (dotimes (_ arg)
;;                 (setq new-slug (concat new-slug slug))))
;;             (concat base-rx new-slug " ")))
;;          (current-files (zk--parse-id 'file-path (zk-index--current-id-list)))
;;          (files (remq nil
;;                       (mapcar
;;                        (lambda (x)
;;                          (when (member x (zk--directory-files t regexp))
;;                            x))
;;                        current-files))))
;;     (zk-index files
;;               zk-index-last-format-function
;;               #'zk-luhmann-sort)))

;; old forward and back
;; (defun zk-luhmann-index-forward ()
;;   "Focus on notes that share a particular ID string.
;; Calling this function on note \"{4,3 }\" will present a listing
;; of all notes whose Luhmann-IDs begin with \"4,3\", such as
;; \"{4,3,a }\" and \"{4,3,c,1,d }\". The effect is like descending
;; \"into\" the archive.

;; Keep in mind that this function will exclude notes that might be
;; considered \"adjacent\", in the sense that \"{4,4 }\" is
;; \"adjacent\" to \"{4,3 }\". This function is therefore a
;; convenience feature, not a substitute for thinking.

;; See 'zk-luhmann-index-back' for complementary behavior."
;;   (interactive)
;;   (let* ((line-count (count-lines (point-min)
;;                                   (point-max)))
;;          (regexp
;;           (cond ((eq this-command 'zk-luhmann-index-forward)
;;                  "{.[^ }]*")
;;                 ((eq this-command 'zk-luhmann-index-back)
;;                  "{.[^, }]*")))
;;           (line (buffer-substring
;;                  (line-beginning-position)
;;                  (line-end-position)))
;;           (id (unless (string= "" line)
;;                 (progn
;;                   (string-match regexp line)
;;                   (match-string-no-properties 0 line)))))
;;     (when id
;;       (zk-index (zk--directory-files t id)
;;                 zk-index-last-format-function
;;                 #'zk-luhmann-sort))
;;     (when (and (eq this-command 'zk-luhmann-index-back)
;;                (eq line-count (count-lines (point-min)
;;                                            (point-max))))
;;       (zk-luhmann-index-top))))

;; (defun zk-luhmann-index-back ()
;;   "Focus on notes that share all but the last ID element.
;; Calling this function on \"{4,3,a }\ will present a listing of
;; all notes whose Luhmann IDs begin with \"4,3\". The effect is
;; like returning back toward the \"top\" of the archive.

;; See 'zk-luhmann-index-forward' for complementary behavior."
;;   (interactive)
;;   (zk-luhmann-index-forward))

(provide 'zk-luhmann)
;;; zk-luhmann.el ends here
