;;; repo-scan.el --- Display status of some collection of GIT repositories  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Paul D. Nelson

;; Author: Paul D. Nelson <nelson.paul.david@gmail.com>
;; Version: 0.0
;; URL: https://github.com/ultronozm/repo-scan.el
;; Package-Requires: ((emacs "29.1") (magit "3.0") (consult "1.0"))
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
;; pushed all changes to your package repos.
;;
;; Usage:
;; 1. Basic scanning: Navigate to a directory containing subdirectories that are
;;    git repos, and run M-x repo-scan.
;; 2. For maintained repositories: Customize `repo-scan-repos' and use
;;    `repo-scan-pull-all', `repo-scan-check-compiled', or `repo-scan-search'.
;;

;;; Code:

(require 'magit)
(require 'consult)

(defgroup repo-scan nil
  "Display status of GIT repositories."
  :group 'tools)

(defcustom repo-scan-repos nil
  "List of repository names to scan.
These should be the base names of repositories in your elpaca/repos directory."
  :type '(repeat string)
  :group 'repo-scan)

(defcustom repo-scan-use-absolute-names nil
  "If non-nil, use absolute names in repo-scan output."
  :type 'boolean
  :group 'repo-scan)

(defcustom repo-scan-base-directory
  (expand-file-name "elpaca/repos/" user-emacs-directory)
  "Base directory containing the repositories to scan."
  :type 'directory
  :group 'repo-scan)

(defun repo-scan--get-repo-path (name)
  "Convert repository NAME to full path using `repo-scan-base-directory'."
  (expand-file-name name repo-scan-base-directory))

(defun repo-scan-get-status (repo)
  "Get status of REPO."
  (let ((default-directory repo))
    (if (not (magit-git-repo-p repo))
        '(not-a-repo)
      (append
       (if (string-empty-p (shell-command-to-string "git status --porcelain"))
           nil
         '(uncommitted))
       (if (string-empty-p (shell-command-to-string
                           (concat "git log "
                                   (magit-get-upstream-branch)
                                   "..HEAD")))
           nil
         '(unpushed))))))

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
        (counter 0)
        (unflagged-repos nil))
    (with-current-buffer buffer
      (erase-buffer)
      (dolist (repo repos)
        (when (and (file-directory-p repo)
                   (not (member (file-name-nondirectory repo) '("." "..")))
                   (magit-git-repo-p repo t))
          (let* ((default-directory repo)
                 (flag nil)
                 (name (if repo-scan-use-absolute-names
                           repo
                         (file-name-nondirectory repo)))
                 (status (repo-scan-get-status repo)))
            (when (member 'uncommitted status)
              (setq flag t)
              (insert (concat
                       (number-to-string (cl-incf counter))
                       ". ["
                       "[elisp:(magit-status " "\"" repo "\")]"
                       "[" name "]] (uncommitted changes)\n")))
            (when (member 'unpushed status)
              (setq flag t)
              (insert (concat
                       (number-to-string (cl-incf counter))
                       ". ["
                       "[elisp:(magit-status " "\"" repo "\")]"
                       "[" name "]] (unpushed changes)\n")))
            (unless flag
              (push repo unflagged-repos)))))
      (switch-to-buffer-other-window buffer)
      (org-mode)
      (goto-char (point-max))
      (insert "\n\nUnflagged repos:\n")
      (dolist (repo (reverse unflagged-repos))
        (insert (concat
                 (number-to-string (cl-incf counter))
                 ". ["
                 "[elisp:(magit-status " "\"" repo "\")]"
                 "["
                 (if repo-scan-use-absolute-names
                     repo
                   (file-name-nondirectory repo))
                 "]]  ")))
      (goto-char (point-min))
      counter)))

;;;###autoload
(defun repo-scan-elpaca ()
  "Scan repos in elpaca directory specified by `repo-scan-base-directory'."
  (interactive)
  (repo-scan repo-scan-base-directory))

;;;###autoload
(defun repo-scan-pull (repos)
  "Pull changes for REPOS that have no uncommitted changes.
Display status for repos that do have uncommitted changes."
  (let ((buffer (get-buffer-create "*repo-scan-pull*"))
        flagged-repos)
    (with-current-buffer buffer
      (erase-buffer))
    (dolist (repo repos)
      (when (magit-git-repo-p repo)
        (let ((status (repo-scan-get-status repo)))
          (if status
              (push repo flagged-repos)
            (let ((default-directory repo))
              (with-current-buffer buffer
                (insert (format "%s: " repo) (shell-command-to-string "git pull"))))))))
    (when flagged-repos
      (repo-scan-core flagged-repos))
    (display-buffer buffer)))

;;;###autoload
(defun repo-scan-pull-all ()
  "Pull all repositories listed in `repo-scan-repos'."
  (interactive)
  (let ((repos (mapcar #'repo-scan--get-repo-path repo-scan-repos)))
    (repo-scan-pull repos)))

;;;###autoload
(defun repo-scan-check-compiled ()
  "Check which repos have uncompiled .el files.
Specifically, checks for missing .elc files in the elpaca builds directory."
  (interactive)
  (dolist (name repo-scan-repos)
    (let ((elc-file
           (locate-user-emacs-file
            (expand-file-name (concat name ".elc")
                              (expand-file-name name "elpaca/builds")))))
      (unless (file-exists-p elc-file)
        (message "%s.elc not found" name)))))

;;;###autoload
(defun repo-scan-search ()
  "Search through all Elisp files in repositories listed in `repo-scan-repos'."
  (interactive)
  (let ((files (mapcan
                (lambda (name)
                  (directory-files-recursively
                   (repo-scan--get-repo-path name)
                   "\\.el\\'"))
                repo-scan-repos)))
    (consult--grep "Ripgrep" #'consult--ripgrep-make-builder files nil)))

(provide 'repo-scan)
;;; repo-scan.el ends here
