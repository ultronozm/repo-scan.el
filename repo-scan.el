;;; repo-scan.el --- Display status of some collection of GIT repositories  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Paul D. Nelson

;; Author: Paul D. Nelson <nelson.paul.david@gmail.com>
;; Version: 0.0
;; URL: https://github.com/ultronozm/czm-tex-fold.el
;; Package-Requires: ((emacs "29.1") (magit "3.0"))
;; Keywords: convenience, git

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

;; Display status of some collection of GIT repositories in a buffer
;; with clickable links.  Useful for checking if you've committed and
;; pushed all changes to my package repos.
;;
;; Usage: navigate to a directory containing subdirectories that are
;; git repos, and run M-x repo-scan.  A buffer will pop up with a list
;; of repos that have uncommitted changes, and a list of repos that
;; have unpushed changes.  Click on the links to open magit status
;; buffers for the repos.
;;
;; Sample use-package declaration:
;;
;; (use-package repo-scan
;;   :vc (:url "https://github.com/ultronozm/repo-scan.el.git"
;;             :rev :newest))

;;; Code:

(require 'magit)

;;;###autoload
(defun repo-scan (&optional dir)
  "Display buffer explaining git status for subfolders of DIR.
Default is to scan subfolders of current directory."
  (interactive)
  (repo-scan-core
   (directory-files (or dir default-directory) t)))

(defun repo-scan-core (repos)
  "Display buffer explaining git status of REPOS.
Show which repos have uncommitted changes, and which have
unpushed commits.  Ignores anything that is not a git repo."
  (interactive)
  (let ((buffer (get-buffer-create "*repo-scan*"))
        (counter 0))
    (with-current-buffer buffer
      (erase-buffer)
      (dolist (repo repos)
        (when (and (file-directory-p repo)
                   (not (member (file-name-nondirectory repo) '("." "..")))
                   (magit-git-repo-p repo t))
          (let ((default-directory repo))
            (unless (string-empty-p (shell-command-to-string "git status --porcelain"))
              (insert (concat
                       (number-to-string (cl-incf counter))
                       ". ["
                       "[elisp:(magit-status " "\"" repo "\")]"
                       "[" repo "]] (uncommitted changes)\n")))
            (unless (string-empty-p (shell-command-to-string
                                     (concat "git log "
                                             (magit-get-upstream-branch)
                                             "..HEAD")))
              (insert (concat
                       (number-to-string (cl-incf counter))
                       ". ["
                       "[elisp:(magit-status " "\"" repo "\")]"
                       "[" repo "]] (unpushed changes)\n"))))))
      (switch-to-buffer-other-window buffer)
      (org-mode)
      (goto-char (point-min))
      counter)))

;;;###autoload
(defun repo-scan-elpaca ()
  "Scan repos in my elpaca directory."
  (interactive)
  (repo-scan (concat user-emacs-directory "elpaca/repos")))

(provide 'repo-scan)
;;; repo-scan.el ends here
