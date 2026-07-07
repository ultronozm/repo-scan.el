;;; repo-scan.el --- Dashboard for a collection of Git repos  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Paul D. Nelson

;; Author: Paul D. Nelson <nelson.paul.david@gmail.com>
;; Version: 0.1
;; URL: https://github.com/ultronozm/repo-scan.el
;; Package-Requires: ((emacs "29.1"))
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

;; `repo-scan' is a Dired-like dashboard for a personal set of Git
;; repositories.  It reads configured repository sources, displays Git
;; state in a `tabulated-list-mode' buffer, and provides light wrappers
;; around VC commands for pull/push operations.
;;
;; The default manifest format matches lines such as:
;;
;;   ~/work/project git@github.com:user/project.git
;;
;; Comments and blank lines are ignored.  A third field may name an
;; extra remote as NAME=URL; fields of the form sync=POLICY and
;; expected-local=BRANCH[,BRANCH...] set the repository's sync policy
;; and expected local-only branches; other extra fields are ignored.
;;
;; The dashboard is opened with M-x repo-scan.  Sync policies make
;; the usual dashboard flow double as an end-of-session sync: the
;; state column shows each repository's pending action (fast-forward
;; pull, push, or wip snapshot-and-push), o shows the outgoing log
;; that an action would push, and x / X execute the safe actions for
;; the marked or all repositories.

;;; Code:

(eval-when-compile (require 'cl-lib))

(require 'diff-mode)
(require 'seq)
(require 'subr-x)
(require 'tabulated-list)
(require 'vc-dir)
(require 'xref)

(declare-function consult-ripgrep "consult" (&optional dir initial))
(declare-function magit-status "magit" (&optional directory))
(declare-function vc-pull "vc" (&optional prompt))
(declare-function vc-push "vc" (&optional prompt))

(defgroup repo-scan nil
  "Dired-like dashboard for managing a manifest of Git repositories."
  :group 'tools
  :prefix "repo-scan-")

(defcustom repo-scan-manifest-files
  (list (expand-file-name "dotfiles/repos.manifest" "~"))
  "Manifest files read by `repo-scan-source-manifests'."
  :type '(repeat file)
  :group 'repo-scan)

(defcustom repo-scan-repos nil
  "Repository base names read by `repo-scan-source-registered'.
Each name is expanded relative to `repo-scan-base-directory'."
  :type '(repeat string)
  :group 'repo-scan)

(defcustom repo-scan-base-directory
  (expand-file-name "elpaca/repos/" user-emacs-directory)
  "Base directory for repositories listed in `repo-scan-repos'."
  :type 'directory
  :group 'repo-scan)

(defcustom repo-scan-sources
  '(repo-scan-source-manifests repo-scan-source-registered)
  "Functions that return repository descriptors.
Each function is called with no arguments and should return a list of
plists.  A descriptor must contain at least :path and may also contain
:name, :group, :origin, and :source."
  :type '(repeat function)
  :group 'repo-scan)

(defcustom repo-scan-extra-roots nil
  "Directories whose immediate Git children are shown by the extra roots source.
Enable this by adding `repo-scan-source-extra-roots' to
`repo-scan-sources'."
  :type '(repeat directory)
  :group 'repo-scan)

(defcustom repo-scan-emacs-package-roots nil
  "Directories whose immediate Git children are Emacs package repos.
Enable this by adding `repo-scan-source-emacs-package-roots' to
`repo-scan-sources'."
  :type '(repeat directory)
  :group 'repo-scan)

(defcustom repo-scan-buffer-name "*repo-scan*"
  "Buffer name used by `repo-scan'."
  :type 'string
  :group 'repo-scan)

(defcustom repo-scan-refresh-concurrency 6
  "Maximum number of repository scan subprocesses to run at once."
  :type 'natnum
  :group 'repo-scan)

(defcustom repo-scan-git-program "git"
  "Git executable used by `repo-scan'."
  :type 'string
  :group 'repo-scan)

(defcustom repo-scan-fetch-arguments '("fetch" "--all" "--prune")
  "Git arguments used by `repo-scan-fetch'."
  :type '(repeat string)
  :group 'repo-scan)

(defcustom repo-scan-command-buffer-name "*repo-scan command*"
  "Buffer used for shell command output."
  :type 'string
  :group 'repo-scan)

(defcustom repo-scan-diff-buffer-name "*repo-scan diff*"
  "Buffer used for `repo-scan-diff'."
  :type 'string
  :group 'repo-scan)

(defcustom repo-scan-log-buffer-name "*repo-scan log*"
  "Buffer used for `repo-scan-log'."
  :type 'string
  :group 'repo-scan)

(defcustom repo-scan-info-buffer-name "*Repo Dashboard Info*"
  "Buffer used for `repo-scan-info'."
  :type 'string
  :group 'repo-scan)

(defface repo-scan-ok-face
  '((t :inherit success))
  "Face used for clean repositories."
  :group 'repo-scan)

(defface repo-scan-warning-face
  '((t :inherit warning))
  "Face used for dirty, ahead, or behind repositories."
  :group 'repo-scan)

(defface repo-scan-error-face
  '((t :inherit error))
  "Face used for missing or non-Git repositories."
  :group 'repo-scan)

(defface repo-scan-muted-face
  '((t :inherit shadow))
  "Face used for quiet dashboard details."
  :group 'repo-scan)

(defvar-local repo-scan--records nil
  "Repository records shown in the current dashboard buffer.")

(defvar-local repo-scan--source-functions nil
  "Repository source functions used by the current dashboard buffer.
When nil, use `repo-scan-sources'.")

(defvar-local repo-scan--marked nil
  "Hash table of marked repository paths.")

(defvar-local repo-scan--preserve-row nil
  "Non-nil means refresh should keep the same visual row.")

(defvar-local repo-scan--scan-generation 0
  "Generation counter for asynchronous dashboard scans.")

(defvar-local repo-scan--scan-queue nil
  "Pending descriptors for the current asynchronous scan.")

(defvar-local repo-scan--scan-active nil
  "Processes active in the current asynchronous scan.")

(defvar-local repo-scan--scan-total 0
  "Number of records in the current asynchronous scan.")

(defvar-local repo-scan--scan-completed 0
  "Number of records completed in the current asynchronous scan.")

(defconst repo-scan--library-file
  (or load-file-name
      buffer-file-name
      (locate-library "repo-scan"))
  "File used by async scan subprocesses to load `repo-scan'.")

(defconst repo-scan--scan-result-marker
  "\n;;; repo-scan-scan-result ;;;\n"
  "Marker preceding the readable async scan result on stdout.")

;;; Paths and descriptors

(defun repo-scan--expand-path (path)
  "Return PATH expanded with a leading tilde interpreted relative to HOME."
  (expand-file-name (substitute-in-file-name path)))

(defun repo-scan--short-path (path)
  "Return PATH with HOME abbreviated."
  (abbreviate-file-name (repo-scan--expand-path path)))

(defun repo-scan--repo-name (path)
  "Return a display name for repository PATH."
  (file-name-nondirectory
   (directory-file-name (repo-scan--expand-path path))))

(defun repo-scan--descriptor (path &rest props)
  "Return a repository descriptor for PATH with PROPS."
  (let ((expanded (repo-scan--expand-path path)))
    (append
     (list :path expanded
           :display-path (repo-scan--short-path expanded)
           :name (repo-scan--repo-name expanded))
     props)))

(defun repo-scan--normalize-descriptor (descriptor)
  "Return DESCRIPTOR with the display fields required by the dashboard."
  (let ((path (plist-get descriptor :path)))
    (unless (stringp path)
      (error "Repository descriptor missing string :path: %S" descriptor))
    (let* ((expanded (repo-scan--expand-path path))
           (normalized (copy-sequence descriptor)))
      (setq normalized (plist-put normalized :path expanded))
      (unless (plist-get normalized :display-path)
        (setq normalized
              (plist-put normalized :display-path
                         (repo-scan--short-path expanded))))
      (unless (plist-get normalized :name)
        (setq normalized
              (plist-put normalized :name
                         (repo-scan--repo-name expanded))))
      normalized)))

(defun repo-scan--parse-manifest-line (line source)
  "Parse manifest LINE from SOURCE.
Return nil for comments and blank lines."
  (let ((trimmed (string-trim line)))
    (unless (or (string-empty-p trimmed)
                (string-prefix-p "#" trimmed))
      (let* ((fields (split-string trimmed "[ \t]+" t))
             (path (nth 0 fields))
             (origin (nth 1 fields))
             (rest (cddr fields))
             (sync (seq-find (lambda (field)
                               (string-prefix-p "sync=" field))
                             rest))
             (expected-local
              (apply #'append
                     (mapcar (lambda (field)
                               (if (string-prefix-p "expected-local=" field)
                                   (repo-scan--split-comma-list
                                    (substring field
                                               (length "expected-local=")))
                                 nil))
                             rest)))
             (extra (seq-find (lambda (field)
                                (not (or (string-prefix-p "sync=" field)
                                         (string-prefix-p
                                          "expected-local=" field))))
                              rest)))
        (when path
          (repo-scan--descriptor
           path
           :origin origin
           :extra-remote (unless (or (null extra) (string= extra "-")) extra)
           :sync-policy (and sync
                             (repo-scan--parse-sync-token
                              (substring sync (length "sync="))))
           :expected-local-branches expected-local
           :group "manifest"
           :source source))))))

(defun repo-scan--read-manifest (file)
  "Return repository descriptors from manifest FILE."
  (when (file-readable-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (let (repos)
        (while (not (eobp))
          (when-let* ((repo (repo-scan--parse-manifest-line
                             (buffer-substring (line-beginning-position)
                                               (line-end-position))
                             file)))
            (push repo repos))
          (forward-line 1))
        (nreverse repos)))))

;;;###autoload
(defun repo-scan-source-manifests ()
  "Return repository descriptors from `repo-scan-manifest-files'."
  (mapcan (lambda (file)
            (repo-scan--read-manifest
             (repo-scan--expand-path file)))
          repo-scan-manifest-files))

;;;###autoload
(defun repo-scan-source-registered ()
  "Repository descriptors for the repos listed in `repo-scan-repos'."
  (mapcar (lambda (name)
            (list :path (expand-file-name name repo-scan-base-directory)
                  :group "elisp packages"
                  :source 'registered))
          repo-scan-repos))

(defun repo-scan--direct-git-children (root group)
  "Return descriptors for Git repositories immediately under ROOT in GROUP."
  (let ((expanded (repo-scan--expand-path root))
        repos)
    (when (file-directory-p expanded)
      (dolist (child (directory-files expanded t directory-files-no-dot-files-regexp))
        (when (and (file-directory-p child)
                   (file-exists-p (expand-file-name ".git" child)))
          (push (repo-scan--descriptor
                 child
                 :group group
                 :source expanded)
                repos))))
    (nreverse repos)))

;;;###autoload
(defun repo-scan-source-extra-roots ()
  "Return Git repositories below `repo-scan-extra-roots'."
  (mapcan (lambda (root)
            (repo-scan--direct-git-children root "extra"))
          repo-scan-extra-roots))

;;;###autoload
(defun repo-scan-source-emacs-package-roots ()
  "Return Emacs package repositories below `repo-scan-emacs-package-roots'."
  (mapcan (lambda (root)
            (repo-scan--direct-git-children root "emacs-package"))
          repo-scan-emacs-package-roots))

(defun repo-scan--collect-descriptors ()
  "Collect and de-duplicate repository descriptors from dashboard sources."
  (let ((seen (make-hash-table :test #'equal))
        (sources (or repo-scan--source-functions repo-scan-sources))
        repos)
    (dolist (source sources)
      (dolist (raw (funcall source))
        (let* ((repo (repo-scan--normalize-descriptor raw))
               (key (file-truename (plist-get repo :path))))
          (unless (gethash key seen)
            (puthash key t seen)
            (push repo repos)))))
    (nreverse repos)))

(defun repo-scan--scan-key (descriptor)
  "Return a stable scan key for DESCRIPTOR."
  (or (plist-get descriptor :repo-scan-scan-key)
      (file-truename (repo-scan--expand-path
                      (plist-get descriptor :path)))))

(defun repo-scan--descriptor-with-scan-key (descriptor)
  "Return a copy of DESCRIPTOR with a stable async scan key."
  (plist-put (copy-sequence descriptor)
             :repo-scan-scan-key
             (repo-scan--scan-key descriptor)))

(defun repo-scan--pending-record (descriptor)
  "Return a placeholder dashboard record for DESCRIPTOR."
  (append (copy-sequence descriptor)
          (list :kind 'pending
                :state "scanning"
                :branch ""
                :dirty 0
                :unpushed 0
                :ahead 0
                :behind 0
                :branches nil)))

(defun repo-scan--scan-error-record (descriptor message)
  "Return an error dashboard record for DESCRIPTOR with MESSAGE."
  (append (copy-sequence descriptor)
          (list :kind 'error
                :state "scan error"
                :branch ""
                :remote-problems (list message)
                :dirty 0
                :unpushed 0
                :ahead 0
                :behind 0
                :branches nil)))

(defun repo-scan--emacs-program ()
  "Return the Emacs executable used for async scan subprocesses."
  (let ((program (expand-file-name invocation-name invocation-directory)))
    (if (file-executable-p program)
        program
      invocation-name)))

(defun repo-scan--async-scan-form (descriptor)
  "Return an Emacs Lisp form string to scan DESCRIPTOR in a subprocess."
  (format
   (concat
    "(let ((repo-scan-git-program %S))"
    "  (condition-case err"
    "      (let ((record (repo-scan--scan '%S)))"
    "        (princ repo-scan--scan-result-marker)"
    "        (prin1 (list :ok record)))"
    "    (error"
    "     (princ repo-scan--scan-result-marker)"
    "     (prin1 (list :error (format \"%%S\" err))))))")
   repo-scan-git-program
   descriptor))

;;; Git helpers

(defun repo-scan--git-lines (dir &rest args)
  "Run Git in DIR with ARGS and return output lines, or nil on failure."
  (when (file-directory-p dir)
    (with-temp-buffer
      (let ((status (apply #'process-file
                           repo-scan-git-program nil t nil
                           "-C" dir args)))
        (when (and (integerp status) (zerop status))
          (split-string (buffer-string) "\n" t))))))

(defun repo-scan--git-string (dir &rest args)
  "Run Git in DIR with ARGS and return trimmed output, or nil on failure."
  (when-let* ((lines (apply #'repo-scan--git-lines dir args)))
    (string-trim (string-join lines "\n"))))

(defun repo-scan--git-success-p (dir &rest args)
  "Return non-nil if Git in DIR with ARGS exits successfully."
  (and (file-directory-p dir)
       (with-temp-buffer
         (let ((status (apply #'process-file
                              repo-scan-git-program nil t nil
                              "-C" dir args)))
           (and (integerp status) (zerop status))))))

(defun repo-scan--git-root (dir)
  "Return Git top-level for DIR, or nil if DIR is not inside a work tree."
  (repo-scan--git-string dir "rev-parse" "--show-toplevel"))

(defun repo-scan--normalize-url (url)
  "Normalize Git remote URL for comparison."
  (when url
    (let ((value (string-remove-suffix ".git" url)))
      (dolist (prefix '("git@" "https://" "http://" "ssh://" "git://"))
        (setq value (string-remove-prefix prefix value)))
      (when (string-match "\\`[^@]+@\\(.+\\)\\'" value)
        (setq value (match-string 1 value)))
      (replace-regexp-in-string ":" "/" value t t))))

(defun repo-scan--remote-url (dir name)
  "Return URL for remote NAME in DIR, or nil."
  (repo-scan--git-string dir "remote" "get-url" name))

(defun repo-scan--remote-matches-p (actual expected)
  "Return non-nil when ACTUAL and EXPECTED denote the same remote."
  (equal (repo-scan--normalize-url actual)
         (repo-scan--normalize-url expected)))

(defun repo-scan--split-comma-list (text)
  "Split comma-separated TEXT into nonempty trimmed strings."
  (seq-filter (lambda (item) (not (string-empty-p item)))
              (mapcar #'string-trim (split-string text ","))))

(defun repo-scan--record-key-matches-p (key record)
  "Return non-nil when KEY names RECORD.
KEY may be a bare repository name or an existing path."
  (or (equal key (plist-get record :name))
      (and (string-match-p "/" key)
           (file-exists-p (repo-scan--expand-path key))
           (file-equal-p (repo-scan--expand-path key)
                         (plist-get record :path)))))

(defun repo-scan--extra-remote-parts (extra)
  "Return (NAME URL) parsed from EXTRA, or nil."
  (when (and extra (string-match "\\`\\([^=]+\\)=\\(.+\\)\\'" extra))
    (list (match-string 1 extra)
          (match-string 2 extra))))

(defun repo-scan--remote-problems (dir descriptor)
  "Return remote problems for DIR according to DESCRIPTOR."
  (let ((origin (plist-get descriptor :origin))
        (extra (plist-get descriptor :extra-remote))
        problems)
    (when (and origin (not (string-empty-p origin)))
      (let ((actual (repo-scan--remote-url dir "origin")))
        (cond
         ((null actual)
          (push "missing origin" problems))
         ((not (repo-scan--remote-matches-p actual origin))
          (push "origin mismatch" problems)))))
    (when extra
      (if-let* ((parts (repo-scan--extra-remote-parts extra))
                (name (nth 0 parts))
                (expected (nth 1 parts))
                (actual (repo-scan--remote-url dir name)))
          (unless (repo-scan--remote-matches-p actual expected)
            (push (format "%s mismatch" name) problems))
        (if-let* ((parts (repo-scan--extra-remote-parts extra))
                  (name (nth 0 parts)))
            (push (format "missing %s" name) problems)
          (push "bad extra remote" problems))))
    (nreverse problems)))

(defun repo-scan--current-branch (dir)
  "Return current branch name for DIR, or nil when detached."
  (repo-scan--git-string dir "symbolic-ref" "--quiet" "--short" "HEAD"))

(defun repo-scan--short-head (dir)
  "Return short HEAD hash for DIR, or nil."
  (repo-scan--git-string dir "rev-parse" "--short" "HEAD"))

(defun repo-scan--count-lines (text)
  "Return the number of non-empty lines in TEXT."
  (if (or (null text) (string-empty-p text))
      0
    (length (split-string text "\n" t))))

(defun repo-scan--dirty-count (dir)
  "Return count of dirty status lines in DIR."
  (repo-scan--count-lines
   (repo-scan--git-string dir "status" "--porcelain")))

(defun repo-scan--unpushed-count (dir)
  "Return count of commits in DIR on local branches but no remote."
  (repo-scan--count-lines
   (repo-scan--git-string dir "log" "--branches" "--not" "--remotes" "--oneline")))

(defun repo-scan--unpushed-count-for-branches (dir branches)
  "Return count of unique local-only commits reachable from BRANCHES in DIR."
  (if branches
      (string-to-number
       (or (apply #'repo-scan--git-string
                  dir "rev-list" "--count"
                  (append branches (list "--not" "--remotes")))
           "0"))
    0))

(defun repo-scan--unpushed-branch-count (dir branch)
  "Return count of commits on BRANCH in DIR but on no remote."
  (string-to-number
   (or (repo-scan--git-string
        dir "rev-list" "--count" branch "--not" "--remotes")
       "0")))

(defun repo-scan--local-branch-names (dir)
  "Return local branch names in DIR."
  (mapcar (lambda (line) (car (split-string line "\t")))
          (repo-scan--local-branch-lines dir)))

(defun repo-scan--included-local-branches (dir expected-local-branches)
  "Return local branches in DIR except EXPECTED-LOCAL-BRANCHES."
  (seq-remove (lambda (branch)
                (member branch expected-local-branches))
              (repo-scan--local-branch-names dir)))

(defun repo-scan--unpushed-branches
    (dir current-branch &optional expected-local-branches)
  "Return local branches in DIR with commits not on any remote.
EXPECTED-LOCAL-BRANCHES are omitted from the result."
  (let (branches)
    (dolist (line (repo-scan--local-branch-lines dir))
      (pcase-let* ((`(,branch ,upstream) (split-string line "\t"))
                   (count (repo-scan--unpushed-branch-count dir branch)))
        (when (and (> count 0)
                   (not (member branch expected-local-branches)))
          (push (list :branch branch
                      :upstream (and upstream
                                     (not (string-empty-p upstream))
                                     upstream)
                      :count count
                      :current (equal branch current-branch))
                branches))))
    (nreverse branches)))

(defun repo-scan--ahead-behind (dir branch upstream)
  "Return cons cell (AHEAD . BEHIND) for BRANCH and UPSTREAM in DIR."
  (when-let* ((counts (repo-scan--git-string
                       dir "rev-list" "--left-right" "--count"
                       (format "%s...%s" branch upstream)))
              (parts (split-string counts "[ \t]+" t))
              (ahead (string-to-number (or (nth 0 parts) "0")))
              (behind (string-to-number (or (nth 1 parts) "0"))))
    (cons ahead behind)))

(defun repo-scan--candidate-upstream (dir branch)
  "Return a same-named remote branch candidate for BRANCH in DIR."
  (seq-find
   (lambda (remote)
     (and (not (string-suffix-p "/HEAD" remote))
          (string-suffix-p (concat "/" branch) remote)))
   (repo-scan--git-lines
    dir "for-each-ref" "--format=%(refname:short)"
    "refs/remotes")))

(defun repo-scan--local-branch-lines (dir)
  "Return local branch/upstream lines for DIR."
  (repo-scan--git-lines
   dir "for-each-ref"
   "--format=%(refname:short)%09%(upstream:short)"
   "refs/heads"))

(defun repo-scan--branch-statuses (dir current-branch)
  "Return branch drift statuses for DIR.
CURRENT-BRANCH is used to mark the current row."
  (let (statuses)
    (dolist (line (repo-scan--local-branch-lines dir))
      (pcase-let* ((`(,branch ,upstream)
                    (split-string line "\t"))
                   (upstream (or (and upstream
                                      (not (string-empty-p upstream))
                                      upstream)
                                 (repo-scan--candidate-upstream dir branch)))
                   (counts (and upstream
                                (repo-scan--ahead-behind
                                 dir branch upstream))))
        (when upstream
          (push (append
                 (list :branch branch
                       :upstream upstream
                       :ahead (or (car counts) 0)
                       :behind (or (cdr counts) 0)
                       :current (equal branch current-branch))
                 (unless counts
                   (list :problem
                         (format "cannot compare %s with %s"
                                 branch upstream))))
                statuses))))
    (nreverse statuses)))

(defun repo-scan--branch-problems (branches)
  "Return remote problem strings from BRANCHES."
  (delq nil (mapcar (lambda (status)
                      (plist-get status :problem))
                    branches)))

(defun repo-scan--current-branch-status (record)
  "Return current branch status plist from RECORD, or nil."
  (seq-find (lambda (status)
              (plist-get status :current))
            (plist-get record :branches)))

(defun repo-scan--problem-branches (record)
  "Return branch status plists from RECORD with non-zero ahead/behind."
  (seq-filter
   (lambda (status)
     (or (> (plist-get status :ahead) 0)
         (> (plist-get status :behind) 0)))
   (plist-get record :branches)))

(defun repo-scan--record-ahead (record)
  "Return aggregate ahead count for RECORD."
  (cl-loop for status in (plist-get record :branches)
           sum (plist-get status :ahead)))

(defun repo-scan--record-behind (record)
  "Return aggregate behind count for RECORD."
  (cl-loop for status in (plist-get record :branches)
           sum (plist-get status :behind)))

(defun repo-scan--safe-pull-p (record)
  "Return non-nil when RECORD can be pulled safely by default."
  (let ((current (repo-scan--current-branch-status record)))
    (and (eq (plist-get record :kind) 'git)
         current
         (zerop (plist-get record :dirty))
         (zerop (plist-get current :ahead))
         (> (plist-get current :behind) 0))))

(defun repo-scan--safe-push-p (record)
  "Return non-nil when RECORD can be pushed without force."
  (let ((current (repo-scan--current-branch-status record)))
    (and (eq (plist-get record :kind) 'git)
         current
         (zerop (plist-get record :dirty))
         (> (plist-get current :ahead) 0)
         (zerop (plist-get current :behind)))))

(defun repo-scan--record-state (record)
  "Return a short state string for RECORD."
  (pcase (plist-get record :kind)
    ('missing "missing")
    ('non-git "not git")
    ('git
     (let* ((dirty (plist-get record :dirty))
            (unpushed (plist-get record :unpushed))
            (remote-problems (plist-get record :remote-problems))
            (current (repo-scan--current-branch-status record))
            (branches (repo-scan--problem-branches record))
            (current-ahead (or (and current (plist-get current :ahead)) 0))
            (current-behind (or (and current (plist-get current :behind)) 0)))
       (cond
        ((and (eq (car (repo-scan--sync-policy record)) 'ignore)
              (or remote-problems (> dirty 0) (> unpushed 0)
                  branches (> current-ahead 0) (> current-behind 0)))
         "expected")
        (remote-problems
         "remote problem")
        ((eq (car (repo-scan--sync-policy record)) 'wip)
         (cond
          ((> dirty 0) "wip snapshot")
          ((plist-get record :wip-push) "wip push")
          (t "ok")))
        ((> dirty 0)
         (if branches "dirty+drift" "dirty"))
        ((and (> current-ahead 0) (> current-behind 0))
         "diverged")
        ((> current-behind 0)
         "pullable")
        ((> current-ahead 0)
         "pushable")
        ((> unpushed 0)
         "local-only")
        (branches "branch drift")
        (t "ok"))))
    (_ "unknown")))

(defun repo-scan--plist-merge (base &rest props)
  "Return a copy of BASE with PROPS merged in, overwriting existing keys."
  (let ((result (copy-sequence base)))
    (while props
      (setq result (plist-put result (pop props) (pop props))))
    result))

(defun repo-scan--scan (descriptor)
  "Return dashboard status record for repository DESCRIPTOR."
  (let* ((path (plist-get descriptor :path))
         (base (copy-sequence descriptor))
         (root (and (file-exists-p path)
                    (repo-scan--git-root path))))
    (cond
     ((not (file-exists-p path))
      (repo-scan--plist-merge base
                                   :kind 'missing
                                   :state "missing"
                                   :branch ""
                                   :dirty 0
                                   :unpushed 0
                                   :unpushed-branches nil
                                   :ahead 0
                                   :behind 0
                                   :branches nil))
     ((not root)
      (repo-scan--plist-merge base
                                   :kind 'non-git
                                   :state "not git"
                                   :branch ""
                                   :dirty 0
                                   :unpushed 0
                                   :unpushed-branches nil
                                   :ahead 0
                                   :behind 0
                                   :branches nil))
     (t
      (let* ((branch (repo-scan--current-branch root))
             (branches (repo-scan--branch-statuses root branch))
             (expected-local-branches
              (repo-scan--expected-local-branches base))
             (included-local-branches
              (repo-scan--included-local-branches
               root expected-local-branches))
             (unpushed-branches
              (repo-scan--unpushed-branches
               root branch expected-local-branches))
             (remote-problems
              (append (repo-scan--remote-problems root descriptor)
                      (repo-scan--branch-problems branches)))
             (record (repo-scan--plist-merge
                      base
                      :kind 'git
                      :path root
                      :display-path (repo-scan--short-path root)
                      :branch (or branch
                                  (format "(detached %s)"
                                          (or (repo-scan--short-head root)
                                              "?")))
                      :remote-problems remote-problems
                      :dirty (repo-scan--dirty-count root)
                      :unpushed
                      (repo-scan--unpushed-count-for-branches
                       root included-local-branches)
                      :unpushed-branches unpushed-branches
                      :expected-local-branches
                      expected-local-branches
                      :branches branches)))
        (setq record (plist-put record :ahead
                                (repo-scan--record-ahead record)))
        (setq record (plist-put record :behind
                                (repo-scan--record-behind record)))
        (pcase-let ((`(,policy . ,remote)
                     (repo-scan--sync-policy record)))
          (when (and (eq policy 'wip) branch)
            (let ((extras (repo-scan--wip-scan-extras
                           root branch remote)))
              (setq record (plist-put record :wip-push
                                      (plist-get extras :wip-push)))
              (setq record (plist-put record :siblings
                                      (plist-get extras :siblings))))))
        (plist-put record :state (repo-scan--record-state record)))))))

;;; Display formatting

(defun repo-scan--state-face (record)
  "Return face for RECORD state."
  (pcase (plist-get record :kind)
    ('pending 'repo-scan-muted-face)
    ('error 'repo-scan-error-face)
    ('missing 'repo-scan-error-face)
    ('non-git 'repo-scan-error-face)
    ('git
     (pcase (plist-get record :state)
       ("ok" 'repo-scan-ok-face)
       ("expected" 'repo-scan-muted-face)
       (_ 'repo-scan-warning-face)))
    (_ 'repo-scan-muted-face)))

(defun repo-scan--format-count (n)
  "Return N as a dashboard count, hiding zeroes."
  (if (and (integerp n) (> n 0))
      (number-to-string n)
    ""))

(defun repo-scan--branch-drift-string (status)
  "Return compact drift text for branch STATUS."
  (let ((ahead (plist-get status :ahead))
        (behind (plist-get status :behind))
        (branch (plist-get status :branch)))
    (concat branch
            (when (> ahead 0)
              (format " +%d" ahead))
            (when (> behind 0)
              (format " -%d" behind)))))

(defun repo-scan--commit-word (count)
  "Return a short commit count string for COUNT."
  (format "%d commit%s" count (if (= count 1) "" "s")))

(defun repo-scan--status-line-word (count)
  "Return a short status-line count string for COUNT."
  (format "%d status line%s" count (if (= count 1) "" "s")))

(defun repo-scan--local-only-branch-string (status)
  "Return compact local-only text for branch STATUS."
  (format "%s +%d"
          (plist-get status :branch)
          (plist-get status :count)))

(defun repo-scan--local-only-text (record)
  "Return compact local-only branch detail text for RECORD."
  (let* ((branches (plist-get record :unpushed-branches))
         (shown (seq-take branches 2))
         (parts (mapcar #'repo-scan--local-only-branch-string shown))
         (total (or (plist-get record :unpushed) 0)))
    (when (> (length branches) (length shown))
      (setq parts (append parts (list (format "+%d more"
                                              (- (length branches)
                                                 (length shown)))))))
    (cond
     (parts
      (concat "local-only: " (string-join parts ", ")))
     ((> total 0)
      (format "local-only: %s" (repo-scan--commit-word total))))))

(defun repo-scan--drift-text (record)
  "Return compact branch drift detail text for RECORD."
  (let* ((branches (repo-scan--problem-branches record))
         (shown (seq-take branches 2))
         (parts (mapcar #'repo-scan--branch-drift-string shown)))
    (when (> (length branches) (length shown))
      (setq parts (append parts (list (format "+%d more"
                                              (- (length branches)
                                                 (length shown)))))))
    (when parts
      (concat "drift: " (string-join parts ", ")))))

(defun repo-scan--branches-text (record)
  "Return compact branch and remote detail text for RECORD."
  (let ((parts (append (plist-get record :remote-problems)
                       (plist-get record :siblings)
                       (delq nil
                             (list (repo-scan--local-only-text record)
                                   (repo-scan--drift-text record))))))
    (string-join parts "; ")))

(defun repo-scan--entry (record)
  "Return tabulated list entry for RECORD."
  (let* ((path (plist-get record :path))
         (marked (and repo-scan--marked
                      (gethash path repo-scan--marked)))
         (face (repo-scan--state-face record))
         (state (propertize (plist-get record :state) 'face face)))
    (list path
          (vector
           (if marked "*" " ")
           (propertize (plist-get record :name) 'face face)
           (or (plist-get record :group) "")
           (or (plist-get record :branch) "")
           (repo-scan--format-count (plist-get record :dirty))
           (repo-scan--format-count (plist-get record :ahead))
           (repo-scan--format-count (plist-get record :behind))
           state
           (propertize (plist-get record :display-path)
                       'face 'repo-scan-muted-face)
           (repo-scan--branches-text record)))))

(defun repo-scan--state-rank (record)
  "Return sort rank for RECORD."
  (pcase (plist-get record :state)
    ("scan error" 0)
    ("missing" 0)
    ("not git" 1)
    ("remote problem" 2)
    ("dirty+drift" 3)
    ("dirty" 4)
    ("diverged" 5)
    ("wip snapshot" 5)
    ("pullable" 6)
    ("pushable" 7)
    ("wip push" 7)
    ("branch drift" 8)
    ("local-only" 9)
    ("scanning" 98)
    ("expected" 97)
    ("ok" 99)
    (_ 50)))

(defun repo-scan--record< (a b)
  "Return non-nil if record A should sort before record B."
  (let ((rank-a (repo-scan--state-rank a))
        (rank-b (repo-scan--state-rank b)))
    (cond
     ((/= rank-a rank-b) (< rank-a rank-b))
     ((not (string= (or (plist-get a :group) "")
                    (or (plist-get b :group) "")))
      (string-lessp (or (plist-get a :group) "")
                    (or (plist-get b :group) "")))
     (t
      (string-lessp (plist-get a :name) (plist-get b :name))))))

(defun repo-scan--header-line (records)
  "Return header line text for RECORDS."
  (let ((missing 0)
        (dirty 0)
        (remote-problems 0)
        (pullable 0)
        (pushable 0)
        (local-only 0)
        (drift 0)
        (scanning 0)
        (errors 0))
    (dolist (record records)
      (pcase (plist-get record :kind)
        ('pending (setq scanning (1+ scanning)))
        ('error (setq errors (1+ errors)))
        ('missing (setq missing (1+ missing)))
        ('git
         (when (plist-get record :remote-problems)
           (setq remote-problems (1+ remote-problems)))
         (when (> (plist-get record :dirty) 0)
           (setq dirty (1+ dirty)))
         (when (repo-scan--safe-pull-p record)
           (setq pullable (1+ pullable)))
         (when (repo-scan--safe-push-p record)
           (setq pushable (1+ pushable)))
         (when (> (plist-get record :unpushed) 0)
           (setq local-only (1+ local-only)))
         (when (repo-scan--problem-branches record)
           (setq drift (1+ drift))))))
    (concat
     (format
      "Repos:%d%s%s  Missing:%d  Remote:%d  Dirty:%d  Pullable:%d  Pushable:%d  Local-only:%d  Drift:%d"
      (length records)
      (if (> scanning 0) (format "  Scanning:%d" scanning) "")
      (if (> errors 0) (format "  Errors:%d" errors) "")
      missing remote-problems dirty pullable pushable local-only drift)
     "   RET:vc-dir  i:info  j:dired  =:diff  .:rescan  +:pull  P:push  x/X:safe pull  ?:help")))

(defun repo-scan--current-position-state (&optional position)
  "Return row state at POSITION, or point when POSITION is nil."
  (save-excursion
    (when position
      (goto-char position))
    (when-let* ((id (tabulated-list-get-id)))
      (list :id id
            :row (count-lines (point-min) (line-beginning-position))
            :column (current-column)))))

(defun repo-scan--goto-id (id)
  "Move to dashboard row ID and return non-nil if found."
  (goto-char (point-min))
  (catch 'found
    (while (not (eobp))
      (when (equal (tabulated-list-get-id) id)
        (throw 'found t))
      (forward-line 1))
    nil))

(defun repo-scan--goto-row (row column)
  "Move to ROW and COLUMN."
  (goto-char (point-min))
  (forward-line (max 0 row))
  (while (and (not (bobp)) (not (tabulated-list-get-id)))
    (forward-line -1))
  (move-to-column column))

(defun repo-scan--restore-position (state entries)
  "Restore point from STATE after rendering ENTRIES."
  (when state
    (let ((id (plist-get state :id))
          (row (plist-get state :row))
          (column (plist-get state :column)))
      (cond
       ((and id
             (not repo-scan--preserve-row)
             (assoc id entries)
             (repo-scan--goto-id id))
        (move-to-column column))
       ((and row entries)
        (repo-scan--goto-row (min row (1- (length entries))) column))))))

;;; Selection and marking

(defun repo-scan--ensure-mark-table ()
  "Ensure the current buffer has a mark table."
  (unless (hash-table-p repo-scan--marked)
    (setq repo-scan--marked (make-hash-table :test #'equal))))

(defun repo-scan--record-at-point ()
  "Return dashboard record at point, or nil."
  (when-let* ((id (tabulated-list-get-id)))
    (seq-find (lambda (record)
                (equal id (plist-get record :path)))
              repo-scan--records)))

(defun repo-scan--require-record ()
  "Return dashboard record at point, or signal a user error."
  (or (repo-scan--record-at-point)
      (user-error "No repository on this line")))

(defun repo-scan--marked-records ()
  "Return marked records in display order."
  (repo-scan--ensure-mark-table)
  (seq-filter (lambda (record)
                (gethash (plist-get record :path) repo-scan--marked))
              repo-scan--records))

(defun repo-scan--records-for-action ()
  "Return marked records, or the current record if none are marked."
  (or (repo-scan--marked-records)
      (list (repo-scan--require-record))))

(defun repo-scan--set-mark (record marked)
  "Set RECORD mark state to MARKED."
  (repo-scan--ensure-mark-table)
  (let ((path (plist-get record :path)))
    (if marked
        (puthash path t repo-scan--marked)
      (remhash path repo-scan--marked))))

(defun repo-scan-mark ()
  "Mark the repository at point and move to the next row."
  (interactive)
  (repo-scan--set-mark (repo-scan--require-record) t)
  (let ((repo-scan--preserve-row t))
    (repo-scan--render))
  (forward-line 1))

(defun repo-scan-unmark ()
  "Unmark the repository at point and move to the next row."
  (interactive)
  (repo-scan--set-mark (repo-scan--require-record) nil)
  (let ((repo-scan--preserve-row t))
    (repo-scan--render))
  (forward-line 1))

(defun repo-scan-toggle-mark ()
  "Toggle the mark on the repository at point."
  (interactive)
  (repo-scan--ensure-mark-table)
  (let* ((record (repo-scan--require-record))
         (path (plist-get record :path))
         (marked (gethash path repo-scan--marked)))
    (repo-scan--set-mark record (not marked)))
  (let ((repo-scan--preserve-row t))
    (repo-scan--render)))

(defun repo-scan-unmark-all ()
  "Clear all dashboard marks."
  (interactive)
  (repo-scan--ensure-mark-table)
  (clrhash repo-scan--marked)
  (repo-scan--render))

(defun repo-scan-mark-if (predicate message)
  "Mark all records satisfying PREDICATE and show MESSAGE."
  (repo-scan--ensure-mark-table)
  (let ((count 0))
    (dolist (record repo-scan--records)
      (when (funcall predicate record)
        (puthash (plist-get record :path) t repo-scan--marked)
        (setq count (1+ count))))
    (repo-scan--render)
    (message "%s: %d" message count)))

(defun repo-scan-mark-safe-pullable ()
  "Mark clean repositories whose current branch can be pulled safely."
  (interactive)
  (repo-scan-mark-if #'repo-scan--safe-pull-p "Marked safe pullable repos"))

(defun repo-scan-mark-safe-pushable ()
  "Mark clean repositories whose current branch can be pushed safely."
  (interactive)
  (repo-scan-mark-if #'repo-scan--safe-push-p "Marked safe pushable repos"))

(defun repo-scan-mark-dirty ()
  "Mark repositories with dirty working trees."
  (interactive)
  (repo-scan-mark-if
   (lambda (record)
     (and (eq (plist-get record :kind) 'git)
          (> (plist-get record :dirty) 0)))
   "Marked dirty repos"))

(defun repo-scan-mark-behind ()
  "Mark repositories with any local branch behind an upstream."
  (interactive)
  (repo-scan-mark-if
   (lambda (record)
     (and (eq (plist-get record :kind) 'git)
          (> (plist-get record :behind) 0)))
   "Marked repos behind upstreams"))

(defun repo-scan-mark-ahead ()
  "Mark repositories with any local branch ahead of an upstream."
  (interactive)
  (repo-scan-mark-if
   (lambda (record)
     (and (eq (plist-get record :kind) 'git)
          (> (plist-get record :ahead) 0)))
   "Marked repos ahead of upstreams"))

(defun repo-scan-mark-stale ()
  "Mark repositories with non-current stale or diverged local branches."
  (interactive)
  (repo-scan-mark-if
   (lambda (record)
     (seq-some (lambda (status)
                 (and (not (plist-get status :current))
                      (or (> (plist-get status :ahead) 0)
                          (> (plist-get status :behind) 0))))
               (plist-get record :branches)))
   "Marked repos with stale local branches"))

;;; Dashboard refresh

(defun repo-scan--save-window-starts ()
  "Return an alist of (WINDOW . START) for windows showing this buffer."
  (let (result)
    (walk-windows
     (lambda (w)
       (when (eq (window-buffer w) (current-buffer))
         (push (cons w (window-start w)) result)))
     nil t)
    result))

(defun repo-scan--restore-window-starts (saved)
  "Restore window-start positions from SAVED."
  (dolist (entry saved)
    (when (window-live-p (car entry))
      (set-window-start (car entry) (cdr entry) t))))

(defun repo-scan--render (&optional old-state skip-sort)
  "Render `repo-scan--records', restoring OLD-STATE when non-nil.
When SKIP-SORT is non-nil, preserve the current record order and
window scroll positions."
  (repo-scan--ensure-mark-table)
  (let* ((old-state (or old-state (repo-scan--current-position-state)))
         (window-starts (when skip-sort
                          (repo-scan--save-window-starts)))
         (records (if skip-sort
                      (copy-sequence repo-scan--records)
                    (sort (copy-sequence repo-scan--records)
                          #'repo-scan--record<)))
         (entries (mapcar #'repo-scan--entry records)))
    (setq repo-scan--records records)
    (setq header-line-format (repo-scan--header-line records))
    (setq tabulated-list-entries entries)
    (tabulated-list-print (not repo-scan--preserve-row))
    (repo-scan--restore-position old-state entries)
    (when window-starts
      (repo-scan--restore-window-starts window-starts))))

(defun repo-scan--async-refresh-available-p ()
  "Return non-nil when async refresh can start subprocesses."
  (and repo-scan--library-file
       (file-readable-p repo-scan--library-file)
       (let ((program (repo-scan--emacs-program)))
         (or (file-executable-p program)
             (executable-find program)))))

(defun repo-scan--cancel-async-scans ()
  "Cancel active async scan subprocesses for the current dashboard."
  (dolist (process (copy-sequence repo-scan--scan-active))
    (when (process-live-p process)
      (delete-process process))
    (when-let* ((buffer (process-buffer process))
                ((buffer-live-p buffer)))
      (kill-buffer buffer)))
  (setq repo-scan--scan-queue nil
        repo-scan--scan-active nil))

(defun repo-scan--replace-record (key record)
  "Replace the record identified by KEY with RECORD."
  (setq repo-scan--records
        (mapcar (lambda (old-record)
                  (if (equal key (repo-scan--scan-key old-record))
                      record
                    old-record))
                repo-scan--records)))

(defun repo-scan--read-scan-result (text)
  "Read an async scan result from process output TEXT."
  (when (string-match (regexp-quote repo-scan--scan-result-marker) text)
    (condition-case nil
        (car (read-from-string (substring text (match-end 0))))
      (error nil))))

(defun repo-scan--scan-process-record (process descriptor)
  "Return the dashboard record produced by PROCESS for DESCRIPTOR."
  (let* ((buffer (process-buffer process))
         (text (if (buffer-live-p buffer)
                   (with-current-buffer buffer
                     (buffer-substring-no-properties (point-min) (point-max)))
                 ""))
         (result (repo-scan--read-scan-result text)))
    (cond
     ((plist-member result :ok)
      (plist-get result :ok))
     ((plist-member result :error)
      (repo-scan--scan-error-record
       descriptor
       (format "scan failed: %s" (plist-get result :error))))
     (t
      (repo-scan--scan-error-record
       descriptor
       (format "scan process exited %s without a readable result"
               (process-exit-status process)))))))

(defun repo-scan--scan-sentinel (process _event)
  "Handle completion of asynchronous scan PROCESS."
  (when (memq (process-status process) '(exit signal))
    (let ((dashboard (process-get process :repo-scan-buffer))
          (generation (process-get process :repo-scan-generation))
          (descriptor (process-get process :repo-scan-descriptor)))
      (when (buffer-live-p dashboard)
        (with-current-buffer dashboard
          (when (= generation repo-scan--scan-generation)
            (let ((old-state (repo-scan--current-position-state))
                  (key (repo-scan--scan-key descriptor))
                  (record (repo-scan--scan-process-record process descriptor)))
              (setq repo-scan--scan-active
                    (delq process repo-scan--scan-active)
                    repo-scan--scan-completed
                    (1+ repo-scan--scan-completed))
              (repo-scan--replace-record key record)
              (repo-scan--start-next-scans)
              (let ((scanning-p (or repo-scan--scan-queue
                                    repo-scan--scan-active)))
                (repo-scan--render old-state scanning-p)
                (unless scanning-p
                  (message "Repo Scan scanned %d repo(s)"
                           repo-scan--scan-completed))))))))
      (when-let* ((buffer (process-buffer process))
                  ((buffer-live-p buffer)))
        (kill-buffer buffer))))

(defun repo-scan--start-next-scans ()
  "Start queued async scans up to `repo-scan-refresh-concurrency'."
  (let ((limit (max 1 repo-scan-refresh-concurrency)))
    (while (and repo-scan--scan-queue
                (< (length repo-scan--scan-active) limit))
      (let* ((descriptor (pop repo-scan--scan-queue))
             (name (plist-get descriptor :name))
             (buffer (generate-new-buffer
                      (format " *repo-scan-scan:%s*" name)))
             (process
              (make-process
               :name (format "repo-scan-scan:%s" name)
               :buffer buffer
               :connection-type 'pipe
               :command (list (repo-scan--emacs-program)
                              "-Q" "--batch"
                              "-l" repo-scan--library-file
                              "--eval"
                              (repo-scan--async-scan-form descriptor))
               :noquery t
               :sentinel #'repo-scan--scan-sentinel)))
        (process-put process :repo-scan-buffer (current-buffer))
        (process-put process :repo-scan-generation
                     repo-scan--scan-generation)
        (process-put process :repo-scan-descriptor descriptor)
        (push process repo-scan--scan-active)))))

(defun repo-scan-refresh-sync ()
  "Refresh the current repository dashboard synchronously."
  (repo-scan--ensure-mark-table)
  (setq repo-scan--scan-generation (1+ repo-scan--scan-generation))
  (repo-scan--cancel-async-scans)
  (let* ((old-state (repo-scan--current-position-state))
         (descriptors (mapcar #'repo-scan--descriptor-with-scan-key
                              (repo-scan--collect-descriptors))))
    (setq repo-scan--scan-total 0
          repo-scan--scan-completed 0
          repo-scan--records (mapcar #'repo-scan--scan descriptors))
    (repo-scan--render old-state)))

(defun repo-scan-refresh-async ()
  "Refresh the current repository dashboard asynchronously."
  (repo-scan--ensure-mark-table)
  (setq repo-scan--scan-generation (1+ repo-scan--scan-generation))
  (repo-scan--cancel-async-scans)
  (let* ((old-state (repo-scan--current-position-state))
         (descriptors (mapcar #'repo-scan--descriptor-with-scan-key
                              (repo-scan--collect-descriptors))))
    (setq repo-scan--scan-total (length descriptors)
          repo-scan--scan-completed 0
          repo-scan--scan-queue descriptors
          repo-scan--records (mapcar #'repo-scan--pending-record
                                          descriptors))
    (repo-scan--render old-state)
    (if descriptors
        (progn
          (repo-scan--start-next-scans)
          (message "Repo Scan scanning %d repo(s)"
                   repo-scan--scan-total))
      (message "Repo Scan has no repositories"))))

(defun repo-scan-refresh ()
  "Refresh the current repository dashboard."
  (interactive)
  (if (repo-scan--async-refresh-available-p)
      (repo-scan-refresh-async)
    (repo-scan-refresh-sync)))

(defun repo-scan-refresh-at-point ()
  "Rescan the repository at point, or all marked repositories."
  (interactive)
  (let* ((records (repo-scan--records-for-action))
         (old-state (repo-scan--current-position-state)))
    (dolist (record records)
      (let* ((key (repo-scan--scan-key record))
             (new-record (repo-scan--scan record)))
        (repo-scan--replace-record key new-record)))
    (repo-scan--render old-state)
    (message "Rescanned %d repo(s)" (length records))))

(defun repo-scan-next-line ()
  "Move to the next dashboard row."
  (interactive)
  (forward-line 1)
  (when (eobp)
    (forward-line -1)))

(defun repo-scan-previous-line ()
  "Move to the previous dashboard row."
  (interactive)
  (forward-line -1)
  (when (bobp)
    (forward-line 1)))

;;; Actions

(defun repo-scan--record-directory (record)
  "Return existing directory for RECORD, or signal an error."
  (let ((path (plist-get record :path)))
    (unless (file-directory-p path)
      (user-error "No directory for %s" (plist-get record :display-path)))
    path))

(defun repo-scan-vc-dir ()
  "Open `vc-dir' for the repository at point."
  (interactive)
  (vc-dir (repo-scan--record-directory (repo-scan--require-record))))

(defun repo-scan-dired ()
  "Open Dired for the repository at point."
  (interactive)
  (dired (repo-scan--record-directory (repo-scan--require-record))))

(defun repo-scan-magit-status ()
  "Open Magit status for the repository at point."
  (interactive)
  (unless (require 'magit nil t)
    (user-error "Magit is not available"))
  (magit-status (repo-scan--record-directory (repo-scan--require-record))))

(defun repo-scan--find-vc-dir-buffer (dir)
  "Return an existing `vc-dir' buffer for DIR, or nil."
  (let ((target (file-name-as-directory (expand-file-name dir))))
    (seq-find
     (lambda (buf)
       (with-current-buffer buf
         (and (derived-mode-p 'vc-dir-mode)
              (equal (file-name-as-directory
                      (expand-file-name default-directory))
                     target))))
     (buffer-list))))

(defun repo-scan--call-vc-command (record command fallback &optional prefix)
  "Call VC COMMAND for RECORD via an existing `vc-dir' buffer.
If no `vc-dir' buffer exists for the repository, call FALLBACK instead.
PREFIX is passed through as `current-prefix-arg'."
  (let ((dir (repo-scan--record-directory record)))
    (if-let* (((fboundp command))
              (vc-buf (repo-scan--find-vc-dir-buffer dir)))
        (let ((current-prefix-arg prefix))
          (pop-to-buffer vc-buf)
          (call-interactively command)
          'vc)
      (funcall fallback record)
      'fallback)))

(defun repo-scan--fallback-pull (record)
  "Fallback pull implementation for RECORD."
  (repo-scan--display-git-command record "*repo-scan pull*"
                                       "pull" "--ff-only"))

(defun repo-scan--fallback-push (record)
  "Fallback push implementation for RECORD."
  (repo-scan--display-git-command record "*repo-scan push*"
                                       "push"))

(defun repo-scan--rescan-record (record dashboard)
  "Rescan RECORD and update DASHBOARD buffer."
  (let* ((key (repo-scan--scan-key record))
         (new-record (repo-scan--scan record)))
    (when (buffer-live-p dashboard)
      (with-current-buffer dashboard
        (repo-scan--replace-record key new-record)
        (repo-scan--render)))))

(defun repo-scan-pull (&optional prefix)
  "Run `vc-pull' in the repository at point.
PREFIX is forwarded to `vc-pull'."
  (interactive "P")
  (let ((record (repo-scan--require-record))
        (dashboard (current-buffer)))
    (pcase (repo-scan--call-vc-command
            record 'vc-pull #'repo-scan--fallback-pull prefix)
      ('vc (message "VC pull started; press . to rescan"))
      ('fallback (repo-scan--rescan-record record dashboard)))))

(defun repo-scan-push (&optional prefix)
  "Run `vc-push' in the repository at point.
PREFIX is forwarded to `vc-push'."
  (interactive "P")
  (let ((record (repo-scan--require-record))
        (dashboard (current-buffer)))
    (pcase (repo-scan--call-vc-command
            record 'vc-push #'repo-scan--fallback-push prefix)
      ('vc (message "VC push started; press . to rescan"))
      ('fallback (repo-scan--rescan-record record dashboard)))))

(defun repo-scan--insert-git-output (dir args)
  "Insert Git output from DIR using ARGS at point.
Return the process exit status."
  (insert "$ git")
  (dolist (arg args)
    (insert " " (shell-quote-argument arg)))
  (insert "\n\n")
  (let ((status (apply #'process-file
                       repo-scan-git-program nil t nil
                       "-C" dir args)))
    (insert "\n")
    status))

(defun repo-scan--display-git-command (record buffer-name &rest args)
  "Run Git ARGS for RECORD and display output in BUFFER-NAME."
  (let ((dir (repo-scan--record-directory record))
        (buffer (get-buffer-create buffer-name)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "%s\n%s\n\n"
                        (plist-get record :name)
                        (plist-get record :display-path)))
        (repo-scan--insert-git-output dir args)
        (special-mode)))
    (pop-to-buffer buffer)))

(defun repo-scan-fetch ()
  "Fetch all remotes for the repository at point."
  (interactive)
  (let ((record (repo-scan--require-record)))
    (apply #'repo-scan--display-git-command
           record "*repo-scan fetch*"
           repo-scan-fetch-arguments)
    (repo-scan-refresh)))

(defun repo-scan-fetch-all ()
  "Fetch all remotes asynchronously for all known Git repositories."
  (interactive)
  (let ((records (seq-filter (lambda (record)
                               (eq (plist-get record :kind) 'git))
                             repo-scan--records)))
    (repo-scan--run-shell-command records
                                       (string-join
                                        (cons repo-scan-git-program
                                              repo-scan-fetch-arguments)
                                        " ")
                                       t)
    (message "Fetch started for %d repo(s); press g after it finishes to refresh"
             (length records))))

(defun repo-scan--choose-local-only-branch (record prompt)
  "Choose a local-only branch from RECORD using PROMPT."
  (let* ((branches (plist-get record :unpushed-branches))
         (default (car branches)))
    (cond
     ((null branches) nil)
     ((= (length branches) 1) default)
     (t
      (let* ((candidates
              (mapcar (lambda (branch)
                        (cons (format "%s (%s)"
                                      (plist-get branch :branch)
                                      (repo-scan--commit-word
                                       (plist-get branch :count)))
                              branch))
                      branches))
             (choice (completing-read prompt candidates nil t
                                      nil nil
                                      (caar candidates))))
        (cdr (assoc choice candidates)))))))

(defun repo-scan--local-only-primary-p (record)
  "Return non-nil when local-only commits are RECORD's main problem."
  (and (> (or (plist-get record :unpushed) 0) 0)
       (or (string= (plist-get record :state) "local-only")
           (not (repo-scan--problem-branches record)))))

(defun repo-scan--insert-local-only-log (record branch &optional patch)
  "Insert local-only commit log for RECORD and BRANCH.
When PATCH is non-nil, include patches."
  (let* ((dir (repo-scan--record-directory record))
         (branch-name (plist-get branch :branch))
         (branch-arg (or branch-name "--branches"))
         (count (or (plist-get branch :count)
                    (plist-get record :unpushed))))
    (insert (format "%s local-only %s\n\n"
                    (or branch-name "all branches")
                    (repo-scan--commit-word count)))
    (insert
     "These commits are reachable from a local branch but from no remote-tracking ref.\n\n")
    (repo-scan--insert-git-output
     dir
     (append (list "log" "--decorate" "--oneline")
             (unless patch '("--graph"))
             (when patch '("--stat" "--patch"))
             (list branch-arg "--not" "--remotes")))))

(defun repo-scan--choose-branch-status (record prompt)
  "Choose a branch status from RECORD using PROMPT."
  (let* ((statuses (repo-scan--problem-branches record))
         (current (repo-scan--current-branch-status record))
         (default (or (and current
                           (or (> (plist-get current :ahead) 0)
                               (> (plist-get current :behind) 0))
                           current)
                      (car statuses))))
    (cond
     ((null statuses) current)
     ((= (length statuses) 1) (car statuses))
     (t
      (let* ((candidates
              (mapcar (lambda (status)
                        (cons (format "%s -> %s (+%d/-%d)"
                                      (plist-get status :branch)
                                      (plist-get status :upstream)
                                      (plist-get status :ahead)
                                      (plist-get status :behind))
                              status))
                      statuses))
             (choice (completing-read prompt candidates nil t
                                      nil nil
                                      (car (rassoc default candidates)))))
        (cdr (assoc choice candidates)))))))

(defun repo-scan--status-for-branch (record branch)
  "Return branch drift status from RECORD for BRANCH, or nil."
  (seq-find (lambda (status)
              (equal branch (plist-get status :branch)))
            (plist-get record :branches)))

(defun repo-scan--branch-advice (status)
  "Return short next-step advice for branch drift STATUS."
  (let ((ahead (plist-get status :ahead))
        (behind (plist-get status :behind))
        (current (plist-get status :current)))
    (cond
     ((and current (= ahead 0) (> behind 0))
      "Next: press x or + to fast-forward pull if the worktree is clean.")
     ((and current (> ahead 0) (= behind 0))
      "Next: press P to push the current branch.")
     ((and (> ahead 0) (> behind 0))
      "Next: inspect the log/diff, then rebase, merge, push, or delete the stale local branch manually.")
     ((> behind 0)
      "Next: if this branch is still useful, check it out and update it; otherwise delete the local branch.")
     ((> ahead 0)
      "Next: push or merge it if the commit matters; otherwise delete the local branch.")
     (t "Next: no action needed for this branch."))))

(defun repo-scan--insert-local-only-info (record)
  "Insert local-only commit details for RECORD."
  (let ((branches (plist-get record :unpushed-branches))
        (total (or (plist-get record :unpushed) 0)))
    (when (> total 0)
      (insert "Local-only commits\n")
      (insert
       "Meaning: commits reachable from local branches but from no remote-tracking ref.\n")
      (insert "Reproduce: git log --branches --not --remotes --oneline --decorate\n")
      (insert "Inspect: press l here, or run the command above in the repo.\n")
      (insert
       "Resolve: push/merge/cherry-pick commits you still need, or delete obsolete local/backup branches.\n\n")
      (if branches
          (dolist (branch branches)
            (let* ((name (plist-get branch :branch))
                   (status (repo-scan--status-for-branch record name))
                   (upstream (or (plist-get branch :upstream)
                                 (and status
                                      (plist-get status :upstream)))))
              (insert (format "- %s: %s"
                              name
                              (repo-scan--commit-word
                               (plist-get branch :count))))
              (when upstream
                (insert (format ", upstream %s" upstream)))
              (when status
                (insert (format ", drift +%d/-%d"
                                (plist-get status :ahead)
                                (plist-get status :behind))))
              (insert "\n")))
        (insert (format "- %s across local branches\n"
                        (repo-scan--commit-word total))))
      (insert "\n"))))

(defun repo-scan--insert-drift-info (record)
  "Insert branch drift details for RECORD."
  (let ((branches (repo-scan--problem-branches record)))
    (when branches
      (insert "Branch drift\n")
      (insert
       "Meaning: local branches differ from their configured or same-named remote branch.\n")
      (dolist (status branches)
        (let ((branch (plist-get status :branch))
              (upstream (plist-get status :upstream))
              (ahead (plist-get status :ahead))
              (behind (plist-get status :behind)))
          (insert (format "- %s -> %s: +%d/-%d%s\n"
                          branch upstream ahead behind
                          (if (plist-get status :current)
                              " (current)"
                            "")))
          (insert (format "  Inspect: git log --left-right --graph --decorate --oneline %s...%s\n"
                          branch upstream))
          (insert "  " (repo-scan--branch-advice status) "\n")))
      (insert "\n"))))

(defun repo-scan-info ()
  "Explain the repository state at point and useful next steps."
  (interactive)
  (let* ((record (repo-scan--require-record))
         (buffer (get-buffer-create repo-scan-info-buffer-name))
         (kind (plist-get record :kind))
         (dirty (or (plist-get record :dirty) 0))
         (remote-problems (plist-get record :remote-problems))
         (current (repo-scan--current-branch-status record)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "%s\n%s\nState: %s\n\n"
                        (plist-get record :name)
                        (plist-get record :display-path)
                        (plist-get record :state)))
        (pcase kind
          ('missing
           (insert "The manifest path does not exist.\n"))
          ('non-git
           (insert "The path exists, but it is not inside a Git repository.\n"))
          ('git
           (when remote-problems
             (insert "Remote problems\n")
             (dolist (problem remote-problems)
               (insert "- " problem "\n"))
             (insert "\n"))
           (when (> dirty 0)
             (insert (format "Working tree\n- %s from git status --porcelain\n"
                             (repo-scan--status-line-word dirty)))
             (insert "Inspect: press = here, or run git status --short.\n\n"))
           (when current
             (insert (format "Current branch\n- %s -> %s: +%d/-%d\n\n"
                             (plist-get current :branch)
                             (plist-get current :upstream)
                             (plist-get current :ahead)
                             (plist-get current :behind))))
           (repo-scan--insert-local-only-info record)
           (repo-scan--insert-drift-info record)
           (when (and (= dirty 0)
                      (not remote-problems)
                      (= (or (plist-get record :unpushed) 0) 0)
                      (not (repo-scan--problem-branches record)))
             (insert "No dashboard problems are recorded for this repository.\n")))
          (_
           (insert "No additional information is available for this row.\n")))
        (special-mode)))
    (pop-to-buffer buffer)))

(defun repo-scan--insert-branch-diff (record status)
  "Insert diff for branch STATUS in RECORD."
  (let* ((dir (repo-scan--record-directory record))
         (branch (plist-get status :branch))
         (upstream (plist-get status :upstream))
         (ahead (plist-get status :ahead))
         (behind (plist-get status :behind)))
    (insert (format "%s -> %s (+%d/-%d)\n\n" branch upstream ahead behind))
    (cond
     ((and (> ahead 0) (> behind 0))
      (repo-scan--insert-git-output
       dir (list "log" "--left-right" "--graph" "--decorate" "--oneline"
                 (format "%s...%s" branch upstream)))
      (insert "\nIncoming diff:\n\n")
      (repo-scan--insert-git-output
       dir (list "diff" (format "%s...%s" branch upstream)))
      (insert "\nOutgoing diff:\n\n")
      (repo-scan--insert-git-output
       dir (list "diff" (format "%s...%s" upstream branch))))
     ((> behind 0)
      (repo-scan--insert-git-output
       dir (list "diff" (format "%s..%s" branch upstream))))
     ((> ahead 0)
      (repo-scan--insert-git-output
       dir (list "diff" (format "%s..%s" upstream branch))))
     (t
      (insert "No branch diff.\n")))))

(defun repo-scan-diff ()
  "Show a useful Git diff for the repository at point."
  (interactive)
  (let* ((record (repo-scan--require-record))
         (dir (repo-scan--record-directory record))
         (buffer (get-buffer-create repo-scan-diff-buffer-name)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "%s\n%s\n\n"
                        (plist-get record :name)
                        (plist-get record :display-path)))
        (cond
         ((> (plist-get record :dirty) 0)
          (repo-scan--insert-git-output dir (list "status" "--short"))
          (insert "\n")
          (repo-scan--insert-git-output dir (list "diff" "HEAD" "--")))
         ((repo-scan--local-only-primary-p record)
          (repo-scan--insert-local-only-log
           record
           (repo-scan--choose-local-only-branch
            record "Local-only branch: ")
           t))
         ((repo-scan--problem-branches record)
          (repo-scan--insert-branch-diff
           record
           (repo-scan--choose-branch-status record "Diff branch: ")))
         (t
          (insert "No dirty files, local-only commits, or branch drift.\n")))
        (diff-mode)))
    (pop-to-buffer buffer)))

(defun repo-scan-log ()
  "Show a branch log for the repository at point."
  (interactive)
  (let* ((record (repo-scan--require-record))
         (dir (repo-scan--record-directory record))
         (local-only (and (repo-scan--local-only-primary-p record)
                          (repo-scan--choose-local-only-branch
                           record "Local-only branch: ")))
         (status (unless local-only
                   (repo-scan--choose-branch-status record "Log branch: ")))
         (branch (or (plist-get status :branch)
                     (and (repo-scan--current-branch-status record)
                          (plist-get (repo-scan--current-branch-status record)
                                     :branch))
                     "HEAD"))
         (upstream (plist-get status :upstream))
         (buffer (get-buffer-create repo-scan-log-buffer-name)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "%s\n%s\n\n"
                        (plist-get record :name)
                        (plist-get record :display-path)))
        (cond
         (local-only
          (repo-scan--insert-local-only-log record local-only))
         (upstream
          (repo-scan--insert-git-output
           dir (list "log" "--left-right" "--graph" "--decorate" "--oneline"
                     (format "%s...%s" branch upstream))))
         (t
          (repo-scan--insert-git-output
           dir (list "log" "--graph" "--decorate" "--oneline" "-n" "80"
                     branch))))
        (special-mode)))
    (pop-to-buffer buffer)))

;;; Search

(defun repo-scan--all-records ()
  "Return normalized records for all configured repository sources."
  (mapcar #'repo-scan--descriptor-with-scan-key
          (repo-scan--collect-descriptors)))

(defun repo-scan--search-records (all)
  "Return records targeted by a search command.
When ALL is non-nil, or when the current buffer is not a
`repo-scan-mode' buffer, return all configured repositories.  Otherwise
return marked repositories, or the repository at point."
  (if (or all (not (derived-mode-p 'repo-scan-mode)))
      (repo-scan--all-records)
    (repo-scan--records-for-action)))

(defun repo-scan--tracked-files (dir)
  "Return tracked files in repository DIR, as absolute file names."
  (with-temp-buffer
    (let ((status (process-file repo-scan-git-program nil t nil
                                "-C" dir "ls-files" "-z")))
      (unless (zerop status)
        (user-error "git ls-files failed in %s" dir))
      (seq-keep (lambda (file)
                  (let ((expanded (expand-file-name file dir)))
                    (when (file-exists-p expanded)
                      expanded)))
                (split-string (buffer-string) "\0" t)))))

(defun repo-scan--search-files (records)
  "Return tracked files for RECORDS."
  (let (files)
    (dolist (record records)
      (let ((dir (repo-scan--record-directory record)))
        (when (file-directory-p dir)
          (setq files (nconc files (repo-scan--tracked-files dir))))))
    files))

;;;###autoload
(defun repo-scan-search (regexp &optional all)
  "Search for REGEXP in tracked files of selected repositories.
In a dashboard buffer, search marked repositories, or the repository at
point when none are marked.  With prefix argument ALL, or when called
outside a dashboard buffer, search all repositories from configured
sources."
  (interactive
   (list (read-regexp "Search tracked files for regexp")
         current-prefix-arg))
  (let* ((records (repo-scan--search-records all))
         (dirs (seq-uniq (mapcar #'repo-scan--record-directory records)
                         #'file-equal-p)))
    (unless records
      (user-error "No repositories to search"))
    (if (and (require 'consult nil t)
             (executable-find "rg"))
        (consult-ripgrep dirs regexp)
      (let* ((files (repo-scan--search-files records))
             (xrefs (xref-matches-in-files regexp files)))
        (unless xrefs
          (user-error "No matches for %s" regexp))
        (xref-show-xrefs (lambda () xrefs) nil)))))

;;; Bulk safe action

(defun repo-scan-execute-safe ()
  "Execute the pending safe sync action on marked repos, or current repo.
Depending on each repository's sync policy (see
`repo-scan-sync-policies'), the safe action is a fast-forward
pull, a push of a cleanly-ahead current branch, or a wip
snapshot-and-push."
  (interactive)
  (repo-scan--execute-sync-actions (repo-scan--records-for-action)))

(defun repo-scan-execute-safe-all ()
  "Execute the pending safe sync action on all eligible repositories."
  (interactive)
  (repo-scan--execute-sync-actions repo-scan--records))

;;; Sync policies (wrap-up)

(defcustom repo-scan-sync-policies nil
  "Alist mapping repositories to sync policies.
Each key is a repository path (tilde allowed) or a bare repository
name.  Each value is one of:

- `origin' -- a normal repository: the dashboard expects it clean and
  `repo-scan-execute-safe' pushes branches that are ahead of
  their upstream and cleanly fast-forwardable.
- `wip', or (wip . REMOTE) -- a working repository:
  `repo-scan-execute-safe' snapshot-commits any uncommitted
  changes and force-pushes the current branch to
  NAMESPACE/BRANCH on REMOTE (default \"wip\"); see
  `repo-scan-wip-namespace'.
- `ignore' -- expected to be dirty or diverged: shown as \"expected\"
  and never acted on.

Repositories not listed default to `origin'.  Manifest lines can also
set the policy with a field of the form sync=origin, sync=wip,
sync=wip:REMOTE, or sync=ignore."
  :type '(alist :key-type string :value-type sexp)
  :group 'repo-scan)

(defcustom repo-scan-expected-local-branches nil
  "Alist mapping repositories to expected local-only branches.
Each key is a repository path (tilde allowed) or a bare repository
name.  Each value is a list of branch names whose local-only commits
should not make the repository show as \"local-only\".

Use this for generated or deliberately machine-local branches.  It
does not hide dirty working trees, branch drift, or local-only commits
on other branches.  Manifest lines can also set this with a field of
the form expected-local=BRANCH[,BRANCH...]."
  :type '(alist :key-type string :value-type (repeat string))
  :group 'repo-scan)

(defcustom repo-scan-wip-namespace 'system-name
  "Namespace for branches pushed by wip-policy sync.
When non-nil, wip-policy sync pushes branch BRANCH to NAMESPACE/BRANCH
on the wip remote instead of BRANCH, so that machines sharing a remote
never write to each other's refs and the forced push can only
overwrite this machine's own previous snapshot.  The value is a
string, or the symbol `system-name' to use a sanitized `system-name'.
Set to nil to push to the unqualified branch name (only safe when a
single machine pushes to the remote)."
  :type '(choice (const :tag "Machine name" system-name)
                 (string :tag "Fixed namespace")
                 (const :tag "None (single machine only)" nil))
  :group 'repo-scan)

(defcustom repo-scan-snapshot-message-function
  #'repo-scan-default-snapshot-message
  "Function returning the commit message for wip snapshot commits.
Called with the repository record."
  :type 'function
  :group 'repo-scan)

(defun repo-scan-default-snapshot-message (_record)
  "Return the default wip snapshot commit message."
  (format-time-string "; wip snapshot %F %R"))

(defun repo-scan--parse-sync-token (token)
  "Return a sync policy for manifest TOKEN, or nil if unrecognized."
  (pcase token
    ("origin" 'origin)
    ("ignore" 'ignore)
    ("wip" 'wip)
    ((pred (string-prefix-p "wip:"))
     (cons 'wip (substring token (length "wip:"))))
    (_ nil)))

(defun repo-scan--sync-policy (record)
  "Return normalized sync policy (SYMBOL . REMOTE) for RECORD."
  (let ((raw (or (plist-get record :sync-policy)
                 (cdr (seq-find
                       (lambda (cell)
                         (let ((key (car cell)))
                           (or (equal key (plist-get record :name))
                               (and (string-match-p "/" key)
                                    (file-exists-p
                                     (repo-scan--expand-path key))
                                    (file-equal-p
                                     (repo-scan--expand-path key)
                                     (plist-get record :path))))))
                       repo-scan-sync-policies)))))
    (pcase raw
      ('ignore '(ignore . nil))
      ('wip '(wip . "wip"))
      (`(wip . ,remote) (cons 'wip remote))
      (_ '(origin . nil)))))

(defun repo-scan--expected-local-branches (record)
  "Return branch names expected to be local-only for RECORD."
  (let ((configured
         (cdr (seq-find
               (lambda (cell)
                 (repo-scan--record-key-matches-p (car cell) record))
               repo-scan-expected-local-branches))))
    (delete-dups
     (copy-sequence
      (append (plist-get record :expected-local-branches)
              configured)))))

(defun repo-scan--wip-namespace ()
  "Return the effective wip namespace string, or nil."
  (pcase repo-scan-wip-namespace
    ('system-name
     (let ((name (downcase (car (split-string (system-name) "\\.")))))
       (replace-regexp-in-string "[^a-z0-9-]" "-" name)))
    ((and (pred stringp) name) name)
    (_ nil)))

(defun repo-scan--wip-target (branch)
  "Return the namespaced wip target ref name for BRANCH."
  (let ((namespace (repo-scan--wip-namespace)))
    (if namespace (format "%s/%s" namespace branch) branch)))

(defun repo-scan--git-status-output (dir &rest args)
  "Run Git in DIR with ARGS; return (EXIT-STATUS . OUTPUT)."
  (with-temp-buffer
    (let ((status (apply #'process-file
                         repo-scan-git-program nil t nil
                         "-C" dir args)))
      (cons status (string-trim (buffer-string))))))

(defun repo-scan--wip-scan-extras (dir branch remote)
  "Return wip scan info for DIR's BRANCH against REMOTE.
Returns a plist with :wip-push (non-nil when the namespaced remote ref
is missing or behind BRANCH) and :siblings (strings describing other
namespaces' refs holding commits absent from BRANCH).  Uses existing
remote-tracking refs; fetch (\\[repo-scan-fetch-all]) to refresh
them."
  (let* ((target (repo-scan--wip-target branch))
         (remote-ref (format "%s/%s" remote target))
         (has-ref (repo-scan--git-success-p
                   dir "rev-parse" "--verify" "--quiet" remote-ref))
         (ahead (and has-ref
                     (car (repo-scan--ahead-behind
                           dir branch remote-ref))))
         (namespace (repo-scan--wip-namespace))
         siblings)
    (dolist (ref (repo-scan--git-lines
                  dir "for-each-ref" "--format=%(refname:short)"
                  (format "refs/remotes/%s/*/%s" remote branch)))
      (let ((sibling-ns
             (and (string-prefix-p (concat remote "/") ref)
                  (string-suffix-p (concat "/" branch) ref)
                  (substring ref (1+ (length remote))
                             (- (length ref) (1+ (length branch)))))))
        (when (and sibling-ns
                   (not (string-empty-p sibling-ns))
                   (not (equal sibling-ns namespace)))
          (let ((missing (cdr (repo-scan--ahead-behind
                               dir branch ref))))
            (when (and missing (> missing 0))
              (push (format "%s +%d" sibling-ns missing) siblings))))))
    (list :wip-push (or (not has-ref) (> (or ahead 0) 0))
          :siblings (nreverse siblings))))

(defun repo-scan--snapshot-commit (record)
  "Snapshot-commit uncommitted changes in RECORD.
Return a result string, or nil when the commit failed."
  (let ((dir (plist-get record :path))
        (message (funcall repo-scan-snapshot-message-function record)))
    (and (repo-scan--git-success-p dir "add" "-A")
         (repo-scan--git-success-p dir "commit" "-m" message)
         (format "snapshot committed (%s)" message))))

(defun repo-scan--wip-sync (record remote)
  "Fetch, snapshot and push RECORD's current branch to REMOTE.
Return (t . MESSAGE) on success, (nil . MESSAGE) on failure."
  (let* ((dir (plist-get record :path))
         (branch (plist-get record :branch))
         (target (repo-scan--wip-target branch)))
    (cond
     ((not (repo-scan--git-success-p dir "fetch" remote))
      (cons nil (format "fetch from %s failed" remote)))
     ((and (> (repo-scan--dirty-count dir) 0)
           (not (repo-scan--snapshot-commit record)))
      (cons nil "snapshot commit failed"))
     (t
      (let ((result (repo-scan--git-status-output
                     dir "push"
                     (format "--force-with-lease=refs/heads/%s" target)
                     remote
                     (format "%s:refs/heads/%s" branch target))))
        (if (zerop (car result))
            (cons t (format "pushed %s to %s/%s" branch remote target))
          (cons nil (cdr result))))))))

(defun repo-scan--origin-push (record)
  "Push RECORD's current branch to its upstream.
Return (t . MESSAGE) on success, (nil . MESSAGE) on failure."
  (let* ((dir (plist-get record :path))
         (current (repo-scan--current-branch-status record))
         (branch (plist-get current :branch))
         (upstream (plist-get current :upstream))
         (remote (car (split-string upstream "/")))
         (result (repo-scan--git-status-output
                  dir "push" remote branch)))
    (if (zerop (car result))
        (cons t (format "pushed %s to %s" branch upstream))
      (cons nil (cdr result)))))

(defun repo-scan--sync-action (record)
  "Return the pending sync action symbol for RECORD, or nil.
One of `pull', `push' or `wip-sync'."
  (when (eq (plist-get record :kind) 'git)
    (pcase-let ((`(,policy . ,_remote) (repo-scan--sync-policy record)))
      (pcase policy
        ('ignore nil)
        ('wip (and (or (> (plist-get record :dirty) 0)
                       (plist-get record :wip-push))
                   'wip-sync))
        (_ (cond
            ((repo-scan--safe-pull-p record) 'pull)
            ((repo-scan--safe-push-p record) 'push)))))))

(defun repo-scan--execute-sync-actions (records)
  "Execute pending sync actions for RECORDS, reporting a receipt."
  (let* ((dashboard (current-buffer))
         (actionable (seq-filter #'repo-scan--sync-action records))
         (summary (mapconcat
                   (lambda (kind)
                     (let ((n (seq-count
                               (lambda (r)
                                 (eq (repo-scan--sync-action r) kind))
                               actionable)))
                       (and (> n 0) (format "%s %d" kind n))))
                   '(pull push wip-sync) " ")))
    (unless actionable
      (user-error "No safe sync actions available"))
    (when (yes-or-no-p (format "Sync (%s repo(s): %s)? "
                               (length actionable) (string-trim summary)))
      (let (failures)
        (dolist (record actionable)
          (let* ((action (repo-scan--sync-action record))
                 (result
                  (pcase action
                    ('pull (repo-scan--git-status-output
                            (plist-get record :path) "pull" "--ff-only"))
                    ('push (repo-scan--origin-push record))
                    ('wip-sync
                     (repo-scan--wip-sync
                      record
                      (cdr (repo-scan--sync-policy record))))))
                 (ok (pcase action
                       ('pull (zerop (car result)))
                       (_ (car result)))))
            (unless ok
              (push (format "%s: %s" (plist-get record :name) (cdr result))
                    failures))
            (repo-scan--rescan-record record dashboard)))
        (if failures
            (message "Sync finished with failures: %s"
                     (string-join (nreverse failures) " | "))
          (message "Sync finished: %s" (string-trim summary)))))))

(defun repo-scan-outgoing-log ()
  "Show the log of commits that a sync would push for the repo at point."
  (interactive)
  (let* ((record (repo-scan--require-record))
         (branch (plist-get record :branch))
         (range
          (pcase-let ((`(,policy . ,remote)
                       (repo-scan--sync-policy record)))
            (pcase policy
              ('wip
               (let ((remote-ref (format "%s/%s" remote
                                         (repo-scan--wip-target branch))))
                 (if (repo-scan--git-success-p
                      (plist-get record :path)
                      "rev-parse" "--verify" "--quiet" remote-ref)
                     (format "%s..%s" remote-ref branch)
                   branch)))
              (_
               (let ((current (repo-scan--current-branch-status record)))
                 (if (and current (plist-get current :upstream))
                     (format "%s..%s" (plist-get current :upstream) branch)
                   branch)))))))
    (repo-scan--display-git-command
     record repo-scan-log-buffer-name
     "log" "--stat" range)))

;;; Shell commands

(defun repo-scan--shell-buffer ()
  "Return the command output buffer."
  (get-buffer-create repo-scan-command-buffer-name))

(defun repo-scan--insert-command-header (record command)
  "Insert a command header for RECORD and COMMAND."
  (insert (format "\n\n== %s (%s)\n$ %s\n\n"
                  (plist-get record :name)
                  (plist-get record :display-path)
                  command)))

(defun repo-scan--insert-prefixed-lines (buffer prefix chunk pending)
  "Insert CHUNK into BUFFER with PREFIX before each complete line.
PENDING is the previous incomplete line.  Return the new incomplete
line, or nil."
  (let* ((text (concat (or pending "") chunk))
         (complete (string-suffix-p "\n" text))
         (parts (split-string text "\n"))
         (tail (unless complete (car (last parts))))
         (lines (if complete parts (butlast parts))))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (dolist (line lines)
          (unless (string-empty-p line)
            (insert prefix line "\n")))))
    tail))

(defun repo-scan--run-shell-command (records command async)
  "Run shell COMMAND in RECORDS.
When ASYNC is non-nil, start one process per repository and prefix
output lines with the repository name."
  (let ((buffer (repo-scan--shell-buffer)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "repo-scan command: %s\n" command))))
    (if async
        (dolist (record records)
          (let* ((default-directory (repo-scan--record-directory record))
                 (repo-name (plist-get record :name))
                 (prefix (format "[%s] " repo-name))
                 (pending nil))
            (with-current-buffer buffer
              (let ((inhibit-read-only t))
                (insert (format "\n== %s (%s)\n"
                                repo-name
                                (plist-get record :display-path)))))
            (make-process
             :name (format "repo-scan:%s" (plist-get record :name))
             :buffer buffer
             :command (list shell-file-name shell-command-switch command)
             :noquery t
             :filter (lambda (_process chunk)
                       (setq pending
                             (repo-scan--insert-prefixed-lines
                              buffer prefix chunk pending)))
             :sentinel (lambda (process _event)
                         (when (memq (process-status process) '(exit signal))
                           (with-current-buffer (process-buffer process)
                             (let ((inhibit-read-only t))
                               (goto-char (point-max))
                               (when (and pending
                                          (not (string-empty-p pending)))
                                 (insert prefix pending "\n")
                                 (setq pending nil))
                               (insert (format "\n[%s exited %s]\n"
                                               (process-name process)
                                               (process-exit-status process))))))))))
      (dolist (record records)
        (let ((default-directory (repo-scan--record-directory record)))
          (with-current-buffer buffer
            (let ((inhibit-read-only t))
              (repo-scan--insert-command-header record command)
              (call-process shell-file-name nil buffer nil
                            shell-command-switch command))))))
    (with-current-buffer buffer
      (special-mode))
    (pop-to-buffer buffer)))

(defun repo-scan-shell-command (command)
  "Run shell COMMAND in marked repos, or the repo at point."
  (interactive
   (list (read-shell-command "Shell command in repos: ")))
  (repo-scan--run-shell-command (repo-scan--records-for-action)
                                     command nil))

(defun repo-scan-async-shell-command (command)
  "Run shell COMMAND asynchronously in marked repos, or the repo at point."
  (interactive
   (list (read-shell-command "Async shell command in repos: ")))
  (repo-scan--run-shell-command (repo-scan--records-for-action)
                                     command t))

;;; Keymap and mode

(defvar repo-scan-mark-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "p") #'repo-scan-mark-safe-pullable)
    (define-key map (kbd "P") #'repo-scan-mark-safe-pushable)
    (define-key map (kbd "d") #'repo-scan-mark-dirty)
    (define-key map (kbd "b") #'repo-scan-mark-behind)
    (define-key map (kbd "a") #'repo-scan-mark-ahead)
    (define-key map (kbd "s") #'repo-scan-mark-stale)
    map)
  "Prefix keymap for repository mark commands.")

(defvar repo-scan-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "n") #'repo-scan-next-line)
    (define-key map (kbd "p") #'repo-scan-previous-line)
    (define-key map (kbd "g") #'repo-scan-refresh)
    (define-key map (kbd ".") #'repo-scan-refresh-at-point)
    (define-key map (kbd "RET") #'repo-scan-vc-dir)
    (define-key map (kbd "i") #'repo-scan-info)
    (define-key map (kbd "j") #'repo-scan-dired)
    (define-key map (kbd "A") #'repo-scan-search)
    (define-key map (kbd "C-x g") #'repo-scan-magit-status)
    (define-key map (kbd "=") #'repo-scan-diff)
    (define-key map (kbd "l") #'repo-scan-log)
    (define-key map (kbd "f") #'repo-scan-fetch)
    (define-key map (kbd "F") #'repo-scan-fetch-all)
    (define-key map (kbd "+") #'repo-scan-pull)
    (define-key map (kbd "P") #'repo-scan-push)
    (define-key map (kbd "m") #'repo-scan-mark)
    (define-key map (kbd "u") #'repo-scan-unmark)
    (define-key map (kbd "t") #'repo-scan-toggle-mark)
    (define-key map (kbd "U") #'repo-scan-unmark-all)
    (define-key map (kbd "*") repo-scan-mark-map)
    (define-key map (kbd "x") #'repo-scan-execute-safe)
    (define-key map (kbd "X") #'repo-scan-execute-safe-all)
    (define-key map (kbd "o") #'repo-scan-outgoing-log)
    (define-key map (kbd "!") #'repo-scan-shell-command)
    (define-key map (kbd "&") #'repo-scan-async-shell-command)
    (define-key map (kbd "?") #'describe-mode)
    map)
  "Keymap used in `repo-scan-mode'.")

;;;###autoload
(define-derived-mode repo-scan-mode tabulated-list-mode "Repo Scan"
  "Major mode for a Dired-like dashboard of Git repositories."
  (repo-scan--ensure-mark-table)
  (setq tabulated-list-format
        [("M" 1 nil)
         ("Repo" 22 t)
         ("Group" 14 t)
         ("Branch" 24 t)
         ("D" 4 tabulated-list-entry-size->)
         ("A" 4 tabulated-list-entry-size->)
         ("B" 4 tabulated-list-entry-size->)
         ("State" 16 t)
         ("Path" 30 t)
         ("Details" 0 t)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key nil)
  (add-hook 'tabulated-list-revert-hook #'repo-scan-refresh nil t)
  (tabulated-list-init-header))

;;;###autoload
(defun repo-scan (&optional sources buffer-name)
  "Open a repository dashboard.
When SOURCES is non-nil, use it as the source function list for the
created dashboard buffer.  BUFFER-NAME overrides
`repo-scan-buffer-name'."
  (interactive)
  (let ((buffer (get-buffer-create (or buffer-name repo-scan-buffer-name))))
    (with-current-buffer buffer
      (repo-scan-mode)
      (setq-local repo-scan--source-functions sources)
      (repo-scan-refresh))
    (pop-to-buffer buffer)))

;;;###autoload
(defun repo-scan-emacs-packages ()
  "Open a repository dashboard for Emacs package repositories."
  (interactive)
  (repo-scan '(repo-scan-source-emacs-package-roots)
                  "*repo-scan: emacs packages*"))

(provide 'repo-scan)

;;; repo-scan.el ends here
