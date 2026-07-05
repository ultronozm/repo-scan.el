;;; repo-dashboard.el --- Dashboard for a manifest of Git repos -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Paul D. Nelson

;; Author: Paul D. Nelson <ultrono@gmail.com>
;; Version: 0.0.1
;; URL: https://github.com/ultronozm/repo-dashboard.el
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, vc

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

;; `repo-dashboard' is a Dired-like dashboard for a personal set of Git
;; repositories.  It reads one or more simple manifest files, displays
;; Git state in a `tabulated-list-mode' buffer, and provides light
;; wrappers around VC commands for pull/push operations.
;;
;; The default manifest format matches lines such as:
;;
;;   ~/work/project git@github.com:user/project.git
;;
;; Comments and blank lines are ignored.  A third field may name an
;; extra remote as NAME=URL; a field of the form sync=POLICY sets the
;; repository's sync policy (see `repo-dashboard-sync-policies');
;; other extra fields are ignored.
;;
;; The dashboard is opened with `repo-dashboard'.  Sync policies make
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

(declare-function magit-status "magit" (&optional directory))
(declare-function vc-pull "vc" (&optional prompt))
(declare-function vc-push "vc" (&optional prompt))

(defgroup repo-dashboard nil
  "Dired-like dashboard for managing a manifest of Git repositories."
  :group 'tools
  :prefix "repo-dashboard-")

(defcustom repo-dashboard-manifest-files
  (list (expand-file-name "dotfiles/repos.manifest" "~"))
  "Manifest files read by `repo-dashboard-source-manifests'."
  :type '(repeat file)
  :group 'repo-dashboard)

(defcustom repo-dashboard-sources
  '(repo-dashboard-source-manifests)
  "Functions that return repository descriptors.
Each function is called with no arguments and should return a list of
plists.  A descriptor must contain at least :path and may also contain
:name, :group, :origin, and :source."
  :type '(repeat function)
  :group 'repo-dashboard)

(defcustom repo-dashboard-extra-roots nil
  "Directories whose immediate Git children are shown by the extra roots source.
Enable this by adding `repo-dashboard-source-extra-roots' to
`repo-dashboard-sources'."
  :type '(repeat directory)
  :group 'repo-dashboard)

(defcustom repo-dashboard-emacs-package-roots nil
  "Directories whose immediate Git children are Emacs package repos.
Enable this by adding `repo-dashboard-source-emacs-package-roots' to
`repo-dashboard-sources'."
  :type '(repeat directory)
  :group 'repo-dashboard)

(defcustom repo-dashboard-buffer-name "*Repo Dashboard*"
  "Buffer name used by `repo-dashboard'."
  :type 'string
  :group 'repo-dashboard)

(defcustom repo-dashboard-refresh-concurrency 6
  "Maximum number of repository scan subprocesses to run at once."
  :type 'natnum
  :group 'repo-dashboard)

(defcustom repo-dashboard-git-program "git"
  "Git executable used by `repo-dashboard'."
  :type 'string
  :group 'repo-dashboard)

(defcustom repo-dashboard-fetch-arguments '("fetch" "--all" "--prune")
  "Git arguments used by `repo-dashboard-fetch'."
  :type '(repeat string)
  :group 'repo-dashboard)

(defcustom repo-dashboard-command-buffer-name "*Repo Dashboard Command*"
  "Buffer used for shell command output."
  :type 'string
  :group 'repo-dashboard)

(defcustom repo-dashboard-diff-buffer-name "*Repo Dashboard Diff*"
  "Buffer used for `repo-dashboard-diff'."
  :type 'string
  :group 'repo-dashboard)

(defcustom repo-dashboard-log-buffer-name "*Repo Dashboard Log*"
  "Buffer used for `repo-dashboard-log'."
  :type 'string
  :group 'repo-dashboard)

(defface repo-dashboard-ok-face
  '((t :inherit success))
  "Face used for clean repositories."
  :group 'repo-dashboard)

(defface repo-dashboard-warning-face
  '((t :inherit warning))
  "Face used for dirty, ahead, or behind repositories."
  :group 'repo-dashboard)

(defface repo-dashboard-error-face
  '((t :inherit error))
  "Face used for missing or non-Git repositories."
  :group 'repo-dashboard)

(defface repo-dashboard-muted-face
  '((t :inherit shadow))
  "Face used for quiet dashboard details."
  :group 'repo-dashboard)

(defvar-local repo-dashboard--records nil
  "Repository records shown in the current dashboard buffer.")

(defvar-local repo-dashboard--source-functions nil
  "Repository source functions used by the current dashboard buffer.
When nil, use `repo-dashboard-sources'.")

(defvar-local repo-dashboard--marked nil
  "Hash table of marked repository paths.")

(defvar-local repo-dashboard--preserve-row nil
  "Non-nil means refresh should keep the same visual row.")

(defvar-local repo-dashboard--scan-generation 0
  "Generation counter for asynchronous dashboard scans.")

(defvar-local repo-dashboard--scan-queue nil
  "Pending descriptors for the current asynchronous scan.")

(defvar-local repo-dashboard--scan-active nil
  "Processes active in the current asynchronous scan.")

(defvar-local repo-dashboard--scan-total 0
  "Number of records in the current asynchronous scan.")

(defvar-local repo-dashboard--scan-completed 0
  "Number of records completed in the current asynchronous scan.")

(defconst repo-dashboard--library-file
  (or load-file-name
      buffer-file-name
      (locate-library "repo-dashboard"))
  "File used by async scan subprocesses to load `repo-dashboard'.")

(defconst repo-dashboard--scan-result-marker
  "\n;;; repo-dashboard-scan-result ;;;\n"
  "Marker preceding the readable async scan result on stdout.")

;;; Paths and descriptors

(defun repo-dashboard--expand-path (path)
  "Return PATH expanded with a leading tilde interpreted relative to HOME."
  (expand-file-name (substitute-in-file-name path)))

(defun repo-dashboard--short-path (path)
  "Return PATH with HOME abbreviated."
  (abbreviate-file-name (repo-dashboard--expand-path path)))

(defun repo-dashboard--repo-name (path)
  "Return a display name for repository PATH."
  (file-name-nondirectory
   (directory-file-name (repo-dashboard--expand-path path))))

(defun repo-dashboard--descriptor (path &rest props)
  "Return a repository descriptor for PATH with PROPS."
  (let ((expanded (repo-dashboard--expand-path path)))
    (append
     (list :path expanded
           :display-path (repo-dashboard--short-path expanded)
           :name (repo-dashboard--repo-name expanded))
     props)))

(defun repo-dashboard--normalize-descriptor (descriptor)
  "Return DESCRIPTOR with the display fields required by the dashboard."
  (let ((path (plist-get descriptor :path)))
    (unless (stringp path)
      (error "Repository descriptor missing string :path: %S" descriptor))
    (let* ((expanded (repo-dashboard--expand-path path))
           (normalized (copy-sequence descriptor)))
      (setq normalized (plist-put normalized :path expanded))
      (unless (plist-get normalized :display-path)
        (setq normalized
              (plist-put normalized :display-path
                         (repo-dashboard--short-path expanded))))
      (unless (plist-get normalized :name)
        (setq normalized
              (plist-put normalized :name
                         (repo-dashboard--repo-name expanded))))
      normalized)))

(defun repo-dashboard--parse-manifest-line (line source)
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
             (extra (seq-find (lambda (field)
                                (not (string-prefix-p "sync=" field)))
                              rest)))
        (when path
          (repo-dashboard--descriptor
           path
           :origin origin
           :extra-remote (unless (or (null extra) (string= extra "-")) extra)
           :sync-policy (and sync
                             (repo-dashboard--parse-sync-token
                              (substring sync (length "sync="))))
           :group "manifest"
           :source source))))))

(defun repo-dashboard--read-manifest (file)
  "Return repository descriptors from manifest FILE."
  (when (file-readable-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (let (repos)
        (while (not (eobp))
          (when-let* ((repo (repo-dashboard--parse-manifest-line
                             (buffer-substring (line-beginning-position)
                                               (line-end-position))
                             file)))
            (push repo repos))
          (forward-line 1))
        (nreverse repos)))))

;;;###autoload
(defun repo-dashboard-source-manifests ()
  "Return repository descriptors from `repo-dashboard-manifest-files'."
  (mapcan (lambda (file)
            (repo-dashboard--read-manifest
             (repo-dashboard--expand-path file)))
          repo-dashboard-manifest-files))

(defun repo-dashboard--direct-git-children (root group)
  "Return descriptors for Git repositories immediately under ROOT in GROUP."
  (let ((expanded (repo-dashboard--expand-path root))
        repos)
    (when (file-directory-p expanded)
      (dolist (child (directory-files expanded t directory-files-no-dot-files-regexp))
        (when (and (file-directory-p child)
                   (file-exists-p (expand-file-name ".git" child)))
          (push (repo-dashboard--descriptor
                 child
                 :group group
                 :source expanded)
                repos))))
    (nreverse repos)))

;;;###autoload
(defun repo-dashboard-source-extra-roots ()
  "Return Git repositories below `repo-dashboard-extra-roots'."
  (mapcan (lambda (root)
            (repo-dashboard--direct-git-children root "extra"))
          repo-dashboard-extra-roots))

;;;###autoload
(defun repo-dashboard-source-emacs-package-roots ()
  "Return Emacs package repositories below `repo-dashboard-emacs-package-roots'."
  (mapcan (lambda (root)
            (repo-dashboard--direct-git-children root "emacs-package"))
          repo-dashboard-emacs-package-roots))

(defun repo-dashboard--collect-descriptors ()
  "Collect and de-duplicate repository descriptors from dashboard sources."
  (let ((seen (make-hash-table :test #'equal))
        (sources (or repo-dashboard--source-functions repo-dashboard-sources))
        repos)
    (dolist (source sources)
      (dolist (raw (funcall source))
        (let* ((repo (repo-dashboard--normalize-descriptor raw))
               (key (file-truename (plist-get repo :path))))
          (unless (gethash key seen)
            (puthash key t seen)
            (push repo repos)))))
    (nreverse repos)))

(defun repo-dashboard--scan-key (descriptor)
  "Return a stable scan key for DESCRIPTOR."
  (or (plist-get descriptor :repo-dashboard-scan-key)
      (file-truename (repo-dashboard--expand-path
                      (plist-get descriptor :path)))))

(defun repo-dashboard--descriptor-with-scan-key (descriptor)
  "Return a copy of DESCRIPTOR with a stable async scan key."
  (plist-put (copy-sequence descriptor)
             :repo-dashboard-scan-key
             (repo-dashboard--scan-key descriptor)))

(defun repo-dashboard--pending-record (descriptor)
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

(defun repo-dashboard--scan-error-record (descriptor message)
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

(defun repo-dashboard--emacs-program ()
  "Return the Emacs executable used for async scan subprocesses."
  (let ((program (expand-file-name invocation-name invocation-directory)))
    (if (file-executable-p program)
        program
      invocation-name)))

(defun repo-dashboard--async-scan-form (descriptor)
  "Return an Emacs Lisp form string to scan DESCRIPTOR in a subprocess."
  (format
   (concat
    "(let ((repo-dashboard-git-program %S))"
    "  (condition-case err"
    "      (let ((record (repo-dashboard--scan '%S)))"
    "        (princ repo-dashboard--scan-result-marker)"
    "        (prin1 (list :ok record)))"
    "    (error"
    "     (princ repo-dashboard--scan-result-marker)"
    "     (prin1 (list :error (format \"%%S\" err))))))")
   repo-dashboard-git-program
   descriptor))

;;; Git helpers

(defun repo-dashboard--git-lines (dir &rest args)
  "Run Git in DIR with ARGS and return output lines, or nil on failure."
  (when (file-directory-p dir)
    (with-temp-buffer
      (let ((status (apply #'process-file
                           repo-dashboard-git-program nil t nil
                           "-C" dir args)))
        (when (and (integerp status) (zerop status))
          (split-string (buffer-string) "\n" t))))))

(defun repo-dashboard--git-string (dir &rest args)
  "Run Git in DIR with ARGS and return trimmed output, or nil on failure."
  (when-let* ((lines (apply #'repo-dashboard--git-lines dir args)))
    (string-trim (string-join lines "\n"))))

(defun repo-dashboard--git-success-p (dir &rest args)
  "Return non-nil if Git in DIR with ARGS exits successfully."
  (and (file-directory-p dir)
       (with-temp-buffer
         (let ((status (apply #'process-file
                              repo-dashboard-git-program nil t nil
                              "-C" dir args)))
           (and (integerp status) (zerop status))))))

(defun repo-dashboard--git-root (dir)
  "Return Git top-level for DIR, or nil if DIR is not inside a work tree."
  (repo-dashboard--git-string dir "rev-parse" "--show-toplevel"))

(defun repo-dashboard--normalize-url (url)
  "Normalize Git remote URL for comparison."
  (when url
    (let ((value (string-remove-suffix ".git" url)))
      (dolist (prefix '("git@" "https://" "http://" "ssh://" "git://"))
        (setq value (string-remove-prefix prefix value)))
      (when (string-match "\\`[^@]+@\\(.+\\)\\'" value)
        (setq value (match-string 1 value)))
      (replace-regexp-in-string ":" "/" value t t))))

(defun repo-dashboard--remote-url (dir name)
  "Return URL for remote NAME in DIR, or nil."
  (repo-dashboard--git-string dir "remote" "get-url" name))

(defun repo-dashboard--remote-matches-p (actual expected)
  "Return non-nil when ACTUAL and EXPECTED denote the same remote."
  (equal (repo-dashboard--normalize-url actual)
         (repo-dashboard--normalize-url expected)))

(defun repo-dashboard--extra-remote-parts (extra)
  "Return (NAME URL) parsed from EXTRA, or nil."
  (when (and extra (string-match "\\`\\([^=]+\\)=\\(.+\\)\\'" extra))
    (list (match-string 1 extra)
          (match-string 2 extra))))

(defun repo-dashboard--remote-problems (dir descriptor)
  "Return remote problems for DIR according to DESCRIPTOR."
  (let ((origin (plist-get descriptor :origin))
        (extra (plist-get descriptor :extra-remote))
        problems)
    (when (and origin (not (string-empty-p origin)))
      (let ((actual (repo-dashboard--remote-url dir "origin")))
        (cond
         ((null actual)
          (push "missing origin" problems))
         ((not (repo-dashboard--remote-matches-p actual origin))
          (push "origin mismatch" problems)))))
    (when extra
      (if-let* ((parts (repo-dashboard--extra-remote-parts extra))
                (name (nth 0 parts))
                (expected (nth 1 parts))
                (actual (repo-dashboard--remote-url dir name)))
          (unless (repo-dashboard--remote-matches-p actual expected)
            (push (format "%s mismatch" name) problems))
        (if-let* ((parts (repo-dashboard--extra-remote-parts extra))
                  (name (nth 0 parts)))
            (push (format "missing %s" name) problems)
          (push "bad extra remote" problems))))
    (nreverse problems)))

(defun repo-dashboard--current-branch (dir)
  "Return current branch name for DIR, or nil when detached."
  (repo-dashboard--git-string dir "symbolic-ref" "--quiet" "--short" "HEAD"))

(defun repo-dashboard--short-head (dir)
  "Return short HEAD hash for DIR, or nil."
  (repo-dashboard--git-string dir "rev-parse" "--short" "HEAD"))

(defun repo-dashboard--count-lines (text)
  "Return the number of non-empty lines in TEXT."
  (if (or (null text) (string-empty-p text))
      0
    (length (split-string text "\n" t))))

(defun repo-dashboard--dirty-count (dir)
  "Return count of dirty status lines in DIR."
  (repo-dashboard--count-lines
   (repo-dashboard--git-string dir "status" "--porcelain")))

(defun repo-dashboard--unpushed-count (dir)
  "Return count of commits in DIR on local branches but no remote."
  (repo-dashboard--count-lines
   (repo-dashboard--git-string dir "log" "--branches" "--not" "--remotes" "--oneline")))

(defun repo-dashboard--ahead-behind (dir branch upstream)
  "Return cons cell (AHEAD . BEHIND) for BRANCH and UPSTREAM in DIR."
  (when-let* ((counts (repo-dashboard--git-string
                       dir "rev-list" "--left-right" "--count"
                       (format "%s...%s" branch upstream)))
              (parts (split-string counts "[ \t]+" t))
              (ahead (string-to-number (or (nth 0 parts) "0")))
              (behind (string-to-number (or (nth 1 parts) "0"))))
    (cons ahead behind)))

(defun repo-dashboard--candidate-upstream (dir branch)
  "Return a same-named remote branch candidate for BRANCH in DIR."
  (seq-find
   (lambda (remote)
     (and (not (string-suffix-p "/HEAD" remote))
          (string-suffix-p (concat "/" branch) remote)))
   (repo-dashboard--git-lines
    dir "for-each-ref" "--format=%(refname:short)"
    "refs/remotes")))

(defun repo-dashboard--local-branch-lines (dir)
  "Return local branch/upstream lines for DIR."
  (repo-dashboard--git-lines
   dir "for-each-ref"
   "--format=%(refname:short)%09%(upstream:short)"
   "refs/heads"))

(defun repo-dashboard--branch-statuses (dir current-branch)
  "Return branch drift statuses for DIR.
CURRENT-BRANCH is used to mark the current row."
  (let (statuses)
    (dolist (line (repo-dashboard--local-branch-lines dir))
      (pcase-let* ((`(,branch ,upstream)
                    (split-string line "\t"))
                   (upstream (or (and upstream
                                      (not (string-empty-p upstream))
                                      upstream)
                                 (repo-dashboard--candidate-upstream dir branch)))
                   (counts (and upstream
                                (repo-dashboard--ahead-behind
                                 dir branch upstream))))
        (when counts
          (push (list :branch branch
                      :upstream upstream
                      :ahead (car counts)
                      :behind (cdr counts)
                      :current (equal branch current-branch))
                statuses))))
    (nreverse statuses)))

(defun repo-dashboard--current-branch-status (record)
  "Return current branch status plist from RECORD, or nil."
  (seq-find (lambda (status)
              (plist-get status :current))
            (plist-get record :branches)))

(defun repo-dashboard--problem-branches (record)
  "Return branch status plists from RECORD with non-zero ahead/behind."
  (seq-filter
   (lambda (status)
     (or (> (plist-get status :ahead) 0)
         (> (plist-get status :behind) 0)))
   (plist-get record :branches)))

(defun repo-dashboard--record-ahead (record)
  "Return aggregate ahead count for RECORD."
  (cl-loop for status in (plist-get record :branches)
           sum (plist-get status :ahead)))

(defun repo-dashboard--record-behind (record)
  "Return aggregate behind count for RECORD."
  (cl-loop for status in (plist-get record :branches)
           sum (plist-get status :behind)))

(defun repo-dashboard--safe-pull-p (record)
  "Return non-nil when RECORD can be pulled safely by default."
  (let ((current (repo-dashboard--current-branch-status record)))
    (and (eq (plist-get record :kind) 'git)
         current
         (zerop (plist-get record :dirty))
         (zerop (plist-get current :ahead))
         (> (plist-get current :behind) 0))))

(defun repo-dashboard--safe-push-p (record)
  "Return non-nil when RECORD can be pushed without force."
  (let ((current (repo-dashboard--current-branch-status record)))
    (and (eq (plist-get record :kind) 'git)
         current
         (zerop (plist-get record :dirty))
         (> (plist-get current :ahead) 0)
         (zerop (plist-get current :behind)))))

(defun repo-dashboard--record-state (record)
  "Return a short state string for RECORD."
  (pcase (plist-get record :kind)
    ('missing "missing")
    ('non-git "not git")
    ('git
     (let* ((dirty (plist-get record :dirty))
            (unpushed (plist-get record :unpushed))
            (remote-problems (plist-get record :remote-problems))
            (current (repo-dashboard--current-branch-status record))
            (branches (repo-dashboard--problem-branches record))
            (current-ahead (or (and current (plist-get current :ahead)) 0))
            (current-behind (or (and current (plist-get current :behind)) 0)))
       (cond
        ((and (eq (car (repo-dashboard--sync-policy record)) 'ignore)
              (or remote-problems (> dirty 0) (> unpushed 0)
                  branches (> current-ahead 0) (> current-behind 0)))
         "expected")
        (remote-problems
         "remote problem")
        ((eq (car (repo-dashboard--sync-policy record)) 'wip)
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
         "unpushed")
        (branches "branch drift")
        (t "ok"))))
    (_ "unknown")))

(defun repo-dashboard--plist-merge (base &rest props)
  "Return a copy of BASE with PROPS merged in, overwriting existing keys."
  (let ((result (copy-sequence base)))
    (while props
      (setq result (plist-put result (pop props) (pop props))))
    result))

(defun repo-dashboard--scan (descriptor)
  "Return dashboard status record for repository DESCRIPTOR."
  (let* ((path (plist-get descriptor :path))
         (base (copy-sequence descriptor))
         (root (and (file-exists-p path)
                    (repo-dashboard--git-root path))))
    (cond
     ((not (file-exists-p path))
      (repo-dashboard--plist-merge base
                                   :kind 'missing
                                   :state "missing"
                                   :branch ""
                                   :dirty 0
                                   :unpushed 0
                                   :ahead 0
                                   :behind 0
                                   :branches nil))
     ((not root)
      (repo-dashboard--plist-merge base
                                   :kind 'non-git
                                   :state "not git"
                                   :branch ""
                                   :dirty 0
                                   :unpushed 0
                                   :ahead 0
                                   :behind 0
                                   :branches nil))
     (t
      (let* ((branch (repo-dashboard--current-branch root))
             (branches (repo-dashboard--branch-statuses root branch))
             (record (repo-dashboard--plist-merge
                      base
                      :kind 'git
                      :path root
                      :display-path (repo-dashboard--short-path root)
                      :branch (or branch
                                  (format "(detached %s)"
                                          (or (repo-dashboard--short-head root)
                                              "?")))
                      :remote-problems
                      (repo-dashboard--remote-problems root descriptor)
                      :dirty (repo-dashboard--dirty-count root)
                      :unpushed (repo-dashboard--unpushed-count root)
                      :branches branches)))
        (setq record (plist-put record :ahead
                                (repo-dashboard--record-ahead record)))
        (setq record (plist-put record :behind
                                (repo-dashboard--record-behind record)))
        (pcase-let ((`(,policy . ,remote)
                     (repo-dashboard--sync-policy record)))
          (when (and (eq policy 'wip) branch)
            (let ((extras (repo-dashboard--wip-scan-extras
                           root branch remote)))
              (setq record (plist-put record :wip-push
                                      (plist-get extras :wip-push)))
              (setq record (plist-put record :siblings
                                      (plist-get extras :siblings))))))
        (plist-put record :state (repo-dashboard--record-state record)))))))

;;; Display formatting

(defun repo-dashboard--state-face (record)
  "Return face for RECORD state."
  (pcase (plist-get record :kind)
    ('pending 'repo-dashboard-muted-face)
    ('error 'repo-dashboard-error-face)
    ('missing 'repo-dashboard-error-face)
    ('non-git 'repo-dashboard-error-face)
    ('git
     (pcase (plist-get record :state)
       ("ok" 'repo-dashboard-ok-face)
       ("expected" 'repo-dashboard-muted-face)
       (_ 'repo-dashboard-warning-face)))
    (_ 'repo-dashboard-muted-face)))

(defun repo-dashboard--format-count (n)
  "Return N as a dashboard count, hiding zeroes."
  (if (and (integerp n) (> n 0))
      (number-to-string n)
    ""))

(defun repo-dashboard--branch-drift-string (status)
  "Return compact drift text for branch STATUS."
  (let ((ahead (plist-get status :ahead))
        (behind (plist-get status :behind))
        (branch (plist-get status :branch)))
    (concat branch
            (when (> ahead 0)
              (format " +%d" ahead))
            (when (> behind 0)
              (format " -%d" behind)))))

(defun repo-dashboard--branches-text (record)
  "Return compact branch and remote detail text for RECORD."
  (let* ((remote-problems (plist-get record :remote-problems))
         (branches (repo-dashboard--problem-branches record))
         (shown (seq-take branches 2))
         (parts (append remote-problems
                        (plist-get record :siblings)
                        (mapcar #'repo-dashboard--branch-drift-string shown))))
    (when (> (length branches) (length shown))
      (setq parts (append parts (list (format "+%d more"
                                              (- (length branches)
                                                 (length shown)))))))
    (string-join parts ", ")))

(defun repo-dashboard--entry (record)
  "Return tabulated list entry for RECORD."
  (let* ((path (plist-get record :path))
         (marked (and repo-dashboard--marked
                      (gethash path repo-dashboard--marked)))
         (face (repo-dashboard--state-face record))
         (state (propertize (plist-get record :state) 'face face)))
    (list path
          (vector
           (if marked "*" " ")
           (propertize (plist-get record :name) 'face face)
           (or (plist-get record :group) "")
           (or (plist-get record :branch) "")
           (repo-dashboard--format-count (plist-get record :dirty))
           (repo-dashboard--format-count (plist-get record :ahead))
           (repo-dashboard--format-count (plist-get record :behind))
           state
           (repo-dashboard--branches-text record)
           (propertize (plist-get record :display-path)
                       'face 'repo-dashboard-muted-face)))))

(defun repo-dashboard--state-rank (record)
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
    ("unpushed" 9)
    ("scanning" 98)
    ("expected" 97)
    ("ok" 99)
    (_ 50)))

(defun repo-dashboard--record< (a b)
  "Return non-nil if record A should sort before record B."
  (let ((rank-a (repo-dashboard--state-rank a))
        (rank-b (repo-dashboard--state-rank b)))
    (cond
     ((/= rank-a rank-b) (< rank-a rank-b))
     ((not (string= (or (plist-get a :group) "")
                    (or (plist-get b :group) "")))
      (string-lessp (or (plist-get a :group) "")
                    (or (plist-get b :group) "")))
     (t
      (string-lessp (plist-get a :name) (plist-get b :name))))))

(defun repo-dashboard--header-line (records)
  "Return header line text for RECORDS."
  (let ((missing 0)
        (dirty 0)
        (remote-problems 0)
        (pullable 0)
        (pushable 0)
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
         (when (repo-dashboard--safe-pull-p record)
           (setq pullable (1+ pullable)))
         (when (repo-dashboard--safe-push-p record)
           (setq pushable (1+ pushable)))
         (when (repo-dashboard--problem-branches record)
           (setq drift (1+ drift))))))
    (concat
     (format
      "Repos:%d%s%s  Missing:%d  Remote:%d  Dirty:%d  Pullable:%d  Pushable:%d  Drift:%d"
      (length records)
      (if (> scanning 0) (format "  Scanning:%d" scanning) "")
      (if (> errors 0) (format "  Errors:%d" errors) "")
      missing remote-problems dirty pullable pushable drift)
     "   RET:vc-dir  j:dired  =:diff  .:rescan  +:pull  P:push  x/X:safe pull  ?:help")))

(defun repo-dashboard--current-position-state (&optional position)
  "Return row state at POSITION, or point when POSITION is nil."
  (save-excursion
    (when position
      (goto-char position))
    (when-let* ((id (tabulated-list-get-id)))
      (list :id id
            :row (count-lines (point-min) (line-beginning-position))
            :column (current-column)))))

(defun repo-dashboard--goto-id (id)
  "Move to dashboard row ID and return non-nil if found."
  (goto-char (point-min))
  (catch 'found
    (while (not (eobp))
      (when (equal (tabulated-list-get-id) id)
        (throw 'found t))
      (forward-line 1))
    nil))

(defun repo-dashboard--goto-row (row column)
  "Move to ROW and COLUMN."
  (goto-char (point-min))
  (forward-line (max 0 row))
  (while (and (not (bobp)) (not (tabulated-list-get-id)))
    (forward-line -1))
  (move-to-column column))

(defun repo-dashboard--restore-position (state entries)
  "Restore point from STATE after rendering ENTRIES."
  (when state
    (let ((id (plist-get state :id))
          (row (plist-get state :row))
          (column (plist-get state :column)))
      (cond
       ((and id
             (not repo-dashboard--preserve-row)
             (assoc id entries)
             (repo-dashboard--goto-id id))
        (move-to-column column))
       ((and row entries)
        (repo-dashboard--goto-row (min row (1- (length entries))) column))))))

;;; Selection and marking

(defun repo-dashboard--ensure-mark-table ()
  "Ensure the current buffer has a mark table."
  (unless (hash-table-p repo-dashboard--marked)
    (setq repo-dashboard--marked (make-hash-table :test #'equal))))

(defun repo-dashboard--record-at-point ()
  "Return dashboard record at point, or nil."
  (when-let* ((id (tabulated-list-get-id)))
    (seq-find (lambda (record)
                (equal id (plist-get record :path)))
              repo-dashboard--records)))

(defun repo-dashboard--require-record ()
  "Return dashboard record at point, or signal a user error."
  (or (repo-dashboard--record-at-point)
      (user-error "No repository on this line")))

(defun repo-dashboard--marked-records ()
  "Return marked records in display order."
  (repo-dashboard--ensure-mark-table)
  (seq-filter (lambda (record)
                (gethash (plist-get record :path) repo-dashboard--marked))
              repo-dashboard--records))

(defun repo-dashboard--records-for-action ()
  "Return marked records, or the current record if none are marked."
  (or (repo-dashboard--marked-records)
      (list (repo-dashboard--require-record))))

(defun repo-dashboard--set-mark (record marked)
  "Set RECORD mark state to MARKED."
  (repo-dashboard--ensure-mark-table)
  (let ((path (plist-get record :path)))
    (if marked
        (puthash path t repo-dashboard--marked)
      (remhash path repo-dashboard--marked))))

(defun repo-dashboard-mark ()
  "Mark the repository at point and move to the next row."
  (interactive)
  (repo-dashboard--set-mark (repo-dashboard--require-record) t)
  (let ((repo-dashboard--preserve-row t))
    (repo-dashboard--render))
  (forward-line 1))

(defun repo-dashboard-unmark ()
  "Unmark the repository at point and move to the next row."
  (interactive)
  (repo-dashboard--set-mark (repo-dashboard--require-record) nil)
  (let ((repo-dashboard--preserve-row t))
    (repo-dashboard--render))
  (forward-line 1))

(defun repo-dashboard-toggle-mark ()
  "Toggle the mark on the repository at point."
  (interactive)
  (repo-dashboard--ensure-mark-table)
  (let* ((record (repo-dashboard--require-record))
         (path (plist-get record :path))
         (marked (gethash path repo-dashboard--marked)))
    (repo-dashboard--set-mark record (not marked)))
  (let ((repo-dashboard--preserve-row t))
    (repo-dashboard--render)))

(defun repo-dashboard-unmark-all ()
  "Clear all dashboard marks."
  (interactive)
  (repo-dashboard--ensure-mark-table)
  (clrhash repo-dashboard--marked)
  (repo-dashboard--render))

(defun repo-dashboard-mark-if (predicate message)
  "Mark all records satisfying PREDICATE and show MESSAGE."
  (repo-dashboard--ensure-mark-table)
  (let ((count 0))
    (dolist (record repo-dashboard--records)
      (when (funcall predicate record)
        (puthash (plist-get record :path) t repo-dashboard--marked)
        (setq count (1+ count))))
    (repo-dashboard--render)
    (message "%s: %d" message count)))

(defun repo-dashboard-mark-safe-pullable ()
  "Mark clean repositories whose current branch can be pulled safely."
  (interactive)
  (repo-dashboard-mark-if #'repo-dashboard--safe-pull-p "Marked safe pullable repos"))

(defun repo-dashboard-mark-safe-pushable ()
  "Mark clean repositories whose current branch can be pushed safely."
  (interactive)
  (repo-dashboard-mark-if #'repo-dashboard--safe-push-p "Marked safe pushable repos"))

(defun repo-dashboard-mark-dirty ()
  "Mark repositories with dirty working trees."
  (interactive)
  (repo-dashboard-mark-if
   (lambda (record)
     (and (eq (plist-get record :kind) 'git)
          (> (plist-get record :dirty) 0)))
   "Marked dirty repos"))

(defun repo-dashboard-mark-behind ()
  "Mark repositories with any local branch behind an upstream."
  (interactive)
  (repo-dashboard-mark-if
   (lambda (record)
     (and (eq (plist-get record :kind) 'git)
          (> (plist-get record :behind) 0)))
   "Marked repos behind upstreams"))

(defun repo-dashboard-mark-ahead ()
  "Mark repositories with any local branch ahead of an upstream."
  (interactive)
  (repo-dashboard-mark-if
   (lambda (record)
     (and (eq (plist-get record :kind) 'git)
          (> (plist-get record :ahead) 0)))
   "Marked repos ahead of upstreams"))

(defun repo-dashboard-mark-stale ()
  "Mark repositories with non-current stale or diverged local branches."
  (interactive)
  (repo-dashboard-mark-if
   (lambda (record)
     (seq-some (lambda (status)
                 (and (not (plist-get status :current))
                      (or (> (plist-get status :ahead) 0)
                          (> (plist-get status :behind) 0))))
               (plist-get record :branches)))
   "Marked repos with stale local branches"))

;;; Dashboard refresh

(defun repo-dashboard--save-window-starts ()
  "Return an alist of (WINDOW . START) for windows showing this buffer."
  (let (result)
    (walk-windows
     (lambda (w)
       (when (eq (window-buffer w) (current-buffer))
         (push (cons w (window-start w)) result)))
     nil t)
    result))

(defun repo-dashboard--restore-window-starts (saved)
  "Restore window-start positions from SAVED."
  (dolist (entry saved)
    (when (window-live-p (car entry))
      (set-window-start (car entry) (cdr entry) t))))

(defun repo-dashboard--render (&optional old-state skip-sort)
  "Render `repo-dashboard--records', restoring OLD-STATE when non-nil.
When SKIP-SORT is non-nil, preserve the current record order and
window scroll positions."
  (repo-dashboard--ensure-mark-table)
  (let* ((old-state (or old-state (repo-dashboard--current-position-state)))
         (window-starts (when skip-sort
                          (repo-dashboard--save-window-starts)))
         (records (if skip-sort
                      (copy-sequence repo-dashboard--records)
                    (sort (copy-sequence repo-dashboard--records)
                          #'repo-dashboard--record<)))
         (entries (mapcar #'repo-dashboard--entry records)))
    (setq repo-dashboard--records records)
    (setq header-line-format (repo-dashboard--header-line records))
    (setq tabulated-list-entries entries)
    (tabulated-list-print (not repo-dashboard--preserve-row))
    (repo-dashboard--restore-position old-state entries)
    (when window-starts
      (repo-dashboard--restore-window-starts window-starts))))

(defun repo-dashboard--async-refresh-available-p ()
  "Return non-nil when async refresh can start subprocesses."
  (and repo-dashboard--library-file
       (file-readable-p repo-dashboard--library-file)
       (let ((program (repo-dashboard--emacs-program)))
         (or (file-executable-p program)
             (executable-find program)))))

(defun repo-dashboard--cancel-async-scans ()
  "Cancel active async scan subprocesses for the current dashboard."
  (dolist (process (copy-sequence repo-dashboard--scan-active))
    (when (process-live-p process)
      (delete-process process))
    (when-let* ((buffer (process-buffer process))
                ((buffer-live-p buffer)))
      (kill-buffer buffer)))
  (setq repo-dashboard--scan-queue nil
        repo-dashboard--scan-active nil))

(defun repo-dashboard--replace-record (key record)
  "Replace the record identified by KEY with RECORD."
  (setq repo-dashboard--records
        (mapcar (lambda (old-record)
                  (if (equal key (repo-dashboard--scan-key old-record))
                      record
                    old-record))
                repo-dashboard--records)))

(defun repo-dashboard--read-scan-result (text)
  "Read an async scan result from process output TEXT."
  (when (string-match (regexp-quote repo-dashboard--scan-result-marker) text)
    (condition-case nil
        (car (read-from-string (substring text (match-end 0))))
      (error nil))))

(defun repo-dashboard--scan-process-record (process descriptor)
  "Return the dashboard record produced by PROCESS for DESCRIPTOR."
  (let* ((buffer (process-buffer process))
         (text (if (buffer-live-p buffer)
                   (with-current-buffer buffer
                     (buffer-substring-no-properties (point-min) (point-max)))
                 ""))
         (result (repo-dashboard--read-scan-result text)))
    (cond
     ((plist-member result :ok)
      (plist-get result :ok))
     ((plist-member result :error)
      (repo-dashboard--scan-error-record
       descriptor
       (format "scan failed: %s" (plist-get result :error))))
     (t
      (repo-dashboard--scan-error-record
       descriptor
       (format "scan process exited %s without a readable result"
               (process-exit-status process)))))))

(defun repo-dashboard--scan-sentinel (process _event)
  "Handle completion of asynchronous scan PROCESS."
  (when (memq (process-status process) '(exit signal))
    (let ((dashboard (process-get process :repo-dashboard-buffer))
          (generation (process-get process :repo-dashboard-generation))
          (descriptor (process-get process :repo-dashboard-descriptor)))
      (when (buffer-live-p dashboard)
        (with-current-buffer dashboard
          (when (= generation repo-dashboard--scan-generation)
            (let ((old-state (repo-dashboard--current-position-state))
                  (key (repo-dashboard--scan-key descriptor))
                  (record (repo-dashboard--scan-process-record process descriptor)))
              (setq repo-dashboard--scan-active
                    (delq process repo-dashboard--scan-active)
                    repo-dashboard--scan-completed
                    (1+ repo-dashboard--scan-completed))
              (repo-dashboard--replace-record key record)
              (repo-dashboard--start-next-scans)
              (let ((scanning-p (or repo-dashboard--scan-queue
                                    repo-dashboard--scan-active)))
                (repo-dashboard--render old-state scanning-p)
                (unless scanning-p
                  (message "Repo Dashboard scanned %d repo(s)"
                           repo-dashboard--scan-completed))))))))
      (when-let* ((buffer (process-buffer process))
                  ((buffer-live-p buffer)))
        (kill-buffer buffer))))

(defun repo-dashboard--start-next-scans ()
  "Start queued async scans up to `repo-dashboard-refresh-concurrency'."
  (let ((limit (max 1 repo-dashboard-refresh-concurrency)))
    (while (and repo-dashboard--scan-queue
                (< (length repo-dashboard--scan-active) limit))
      (let* ((descriptor (pop repo-dashboard--scan-queue))
             (name (plist-get descriptor :name))
             (buffer (generate-new-buffer
                      (format " *repo-dashboard-scan:%s*" name)))
             (process
              (make-process
               :name (format "repo-dashboard-scan:%s" name)
               :buffer buffer
               :connection-type 'pipe
               :command (list (repo-dashboard--emacs-program)
                              "-Q" "--batch"
                              "-l" repo-dashboard--library-file
                              "--eval"
                              (repo-dashboard--async-scan-form descriptor))
               :noquery t
               :sentinel #'repo-dashboard--scan-sentinel)))
        (process-put process :repo-dashboard-buffer (current-buffer))
        (process-put process :repo-dashboard-generation
                     repo-dashboard--scan-generation)
        (process-put process :repo-dashboard-descriptor descriptor)
        (push process repo-dashboard--scan-active)))))

(defun repo-dashboard-refresh-sync ()
  "Refresh the current repository dashboard synchronously."
  (repo-dashboard--ensure-mark-table)
  (setq repo-dashboard--scan-generation (1+ repo-dashboard--scan-generation))
  (repo-dashboard--cancel-async-scans)
  (let* ((old-state (repo-dashboard--current-position-state))
         (descriptors (mapcar #'repo-dashboard--descriptor-with-scan-key
                              (repo-dashboard--collect-descriptors))))
    (setq repo-dashboard--scan-total 0
          repo-dashboard--scan-completed 0
          repo-dashboard--records (mapcar #'repo-dashboard--scan descriptors))
    (repo-dashboard--render old-state)))

(defun repo-dashboard-refresh-async ()
  "Refresh the current repository dashboard asynchronously."
  (repo-dashboard--ensure-mark-table)
  (setq repo-dashboard--scan-generation (1+ repo-dashboard--scan-generation))
  (repo-dashboard--cancel-async-scans)
  (let* ((old-state (repo-dashboard--current-position-state))
         (descriptors (mapcar #'repo-dashboard--descriptor-with-scan-key
                              (repo-dashboard--collect-descriptors))))
    (setq repo-dashboard--scan-total (length descriptors)
          repo-dashboard--scan-completed 0
          repo-dashboard--scan-queue descriptors
          repo-dashboard--records (mapcar #'repo-dashboard--pending-record
                                          descriptors))
    (repo-dashboard--render old-state)
    (if descriptors
        (progn
          (repo-dashboard--start-next-scans)
          (message "Repo Dashboard scanning %d repo(s)"
                   repo-dashboard--scan-total))
      (message "Repo Dashboard has no repositories"))))

(defun repo-dashboard-refresh ()
  "Refresh the current repository dashboard."
  (interactive)
  (if (repo-dashboard--async-refresh-available-p)
      (repo-dashboard-refresh-async)
    (repo-dashboard-refresh-sync)))

(defun repo-dashboard-refresh-at-point ()
  "Rescan the repository at point, or all marked repositories."
  (interactive)
  (let* ((records (repo-dashboard--records-for-action))
         (old-state (repo-dashboard--current-position-state)))
    (dolist (record records)
      (let* ((key (repo-dashboard--scan-key record))
             (new-record (repo-dashboard--scan record)))
        (repo-dashboard--replace-record key new-record)))
    (repo-dashboard--render old-state)
    (message "Rescanned %d repo(s)" (length records))))

(defun repo-dashboard-next-line ()
  "Move to the next dashboard row."
  (interactive)
  (forward-line 1)
  (when (eobp)
    (forward-line -1)))

(defun repo-dashboard-previous-line ()
  "Move to the previous dashboard row."
  (interactive)
  (forward-line -1)
  (when (bobp)
    (forward-line 1)))

;;; Actions

(defun repo-dashboard--record-directory (record)
  "Return existing directory for RECORD, or signal an error."
  (let ((path (plist-get record :path)))
    (unless (file-directory-p path)
      (user-error "No directory for %s" (plist-get record :display-path)))
    path))

(defun repo-dashboard-vc-dir ()
  "Open `vc-dir' for the repository at point."
  (interactive)
  (vc-dir (repo-dashboard--record-directory (repo-dashboard--require-record))))

(defun repo-dashboard-dired ()
  "Open Dired for the repository at point."
  (interactive)
  (dired (repo-dashboard--record-directory (repo-dashboard--require-record))))

(defun repo-dashboard-magit-status ()
  "Open Magit status for the repository at point."
  (interactive)
  (unless (require 'magit nil t)
    (user-error "Magit is not available"))
  (magit-status (repo-dashboard--record-directory (repo-dashboard--require-record))))

(defun repo-dashboard--find-vc-dir-buffer (dir)
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

(defun repo-dashboard--call-vc-command (record command fallback &optional prefix)
  "Call VC COMMAND for RECORD via an existing `vc-dir' buffer.
If no `vc-dir' buffer exists for the repository, call FALLBACK instead.
PREFIX is passed through as `current-prefix-arg'."
  (let ((dir (repo-dashboard--record-directory record)))
    (if-let* (((fboundp command))
              (vc-buf (repo-dashboard--find-vc-dir-buffer dir)))
        (let ((current-prefix-arg prefix))
          (pop-to-buffer vc-buf)
          (call-interactively command)
          'vc)
      (funcall fallback record)
      'fallback)))

(defun repo-dashboard--fallback-pull (record)
  "Fallback pull implementation for RECORD."
  (repo-dashboard--display-git-command record "*Repo Dashboard Pull*"
                                       "pull" "--ff-only"))

(defun repo-dashboard--fallback-push (record)
  "Fallback push implementation for RECORD."
  (repo-dashboard--display-git-command record "*Repo Dashboard Push*"
                                       "push"))

(defun repo-dashboard--rescan-record (record dashboard)
  "Rescan RECORD and update DASHBOARD buffer."
  (let* ((key (repo-dashboard--scan-key record))
         (new-record (repo-dashboard--scan record)))
    (when (buffer-live-p dashboard)
      (with-current-buffer dashboard
        (repo-dashboard--replace-record key new-record)
        (repo-dashboard--render)))))

(defun repo-dashboard-pull (&optional prefix)
  "Run `vc-pull' in the repository at point.
PREFIX is forwarded to `vc-pull'."
  (interactive "P")
  (let ((record (repo-dashboard--require-record))
        (dashboard (current-buffer)))
    (pcase (repo-dashboard--call-vc-command
            record 'vc-pull #'repo-dashboard--fallback-pull prefix)
      ('vc (message "VC pull started; press . to rescan"))
      ('fallback (repo-dashboard--rescan-record record dashboard)))))

(defun repo-dashboard-push (&optional prefix)
  "Run `vc-push' in the repository at point.
PREFIX is forwarded to `vc-push'."
  (interactive "P")
  (let ((record (repo-dashboard--require-record))
        (dashboard (current-buffer)))
    (pcase (repo-dashboard--call-vc-command
            record 'vc-push #'repo-dashboard--fallback-push prefix)
      ('vc (message "VC push started; press . to rescan"))
      ('fallback (repo-dashboard--rescan-record record dashboard)))))

(defun repo-dashboard--insert-git-output (dir args)
  "Insert Git output from DIR using ARGS at point.
Return the process exit status."
  (insert "$ git")
  (dolist (arg args)
    (insert " " (shell-quote-argument arg)))
  (insert "\n\n")
  (let ((status (apply #'process-file
                       repo-dashboard-git-program nil t nil
                       "-C" dir args)))
    (insert "\n")
    status))

(defun repo-dashboard--display-git-command (record buffer-name &rest args)
  "Run Git ARGS for RECORD and display output in BUFFER-NAME."
  (let ((dir (repo-dashboard--record-directory record))
        (buffer (get-buffer-create buffer-name)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "%s\n%s\n\n"
                        (plist-get record :name)
                        (plist-get record :display-path)))
        (repo-dashboard--insert-git-output dir args)
        (special-mode)))
    (pop-to-buffer buffer)))

(defun repo-dashboard-fetch ()
  "Fetch all remotes for the repository at point."
  (interactive)
  (let ((record (repo-dashboard--require-record)))
    (apply #'repo-dashboard--display-git-command
           record "*Repo Dashboard Fetch*"
           repo-dashboard-fetch-arguments)
    (repo-dashboard-refresh)))

(defun repo-dashboard-fetch-all ()
  "Fetch all remotes asynchronously for all known Git repositories."
  (interactive)
  (let ((records (seq-filter (lambda (record)
                               (eq (plist-get record :kind) 'git))
                             repo-dashboard--records)))
    (repo-dashboard--run-shell-command records
                                       (string-join
                                        (cons repo-dashboard-git-program
                                              repo-dashboard-fetch-arguments)
                                        " ")
                                       t)
    (message "Fetch started for %d repo(s); press g after it finishes to refresh"
             (length records))))

(defun repo-dashboard--choose-branch-status (record prompt)
  "Choose a branch status from RECORD using PROMPT."
  (let* ((statuses (repo-dashboard--problem-branches record))
         (current (repo-dashboard--current-branch-status record))
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

(defun repo-dashboard--insert-branch-diff (record status)
  "Insert diff for branch STATUS in RECORD."
  (let* ((dir (repo-dashboard--record-directory record))
         (branch (plist-get status :branch))
         (upstream (plist-get status :upstream))
         (ahead (plist-get status :ahead))
         (behind (plist-get status :behind)))
    (insert (format "%s -> %s (+%d/-%d)\n\n" branch upstream ahead behind))
    (cond
     ((and (> ahead 0) (> behind 0))
      (repo-dashboard--insert-git-output
       dir (list "log" "--left-right" "--graph" "--decorate" "--oneline"
                 (format "%s...%s" branch upstream)))
      (insert "\nIncoming diff:\n\n")
      (repo-dashboard--insert-git-output
       dir (list "diff" (format "%s...%s" branch upstream)))
      (insert "\nOutgoing diff:\n\n")
      (repo-dashboard--insert-git-output
       dir (list "diff" (format "%s...%s" upstream branch))))
     ((> behind 0)
      (repo-dashboard--insert-git-output
       dir (list "diff" (format "%s..%s" branch upstream))))
     ((> ahead 0)
      (repo-dashboard--insert-git-output
       dir (list "diff" (format "%s..%s" upstream branch))))
     (t
      (insert "No branch diff.\n")))))

(defun repo-dashboard-diff ()
  "Show a useful Git diff for the repository at point."
  (interactive)
  (let* ((record (repo-dashboard--require-record))
         (dir (repo-dashboard--record-directory record))
         (buffer (get-buffer-create repo-dashboard-diff-buffer-name)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "%s\n%s\n\n"
                        (plist-get record :name)
                        (plist-get record :display-path)))
        (cond
         ((> (plist-get record :dirty) 0)
          (repo-dashboard--insert-git-output dir (list "status" "--short"))
          (insert "\n")
          (repo-dashboard--insert-git-output dir (list "diff" "HEAD" "--")))
         ((repo-dashboard--problem-branches record)
          (repo-dashboard--insert-branch-diff
           record
           (repo-dashboard--choose-branch-status record "Diff branch: ")))
         (t
          (insert "No dirty files or branch drift.\n")))
        (diff-mode)))
    (pop-to-buffer buffer)))

(defun repo-dashboard-log ()
  "Show a branch log for the repository at point."
  (interactive)
  (let* ((record (repo-dashboard--require-record))
         (dir (repo-dashboard--record-directory record))
         (status (repo-dashboard--choose-branch-status record "Log branch: "))
         (branch (or (plist-get status :branch)
                     (and (repo-dashboard--current-branch-status record)
                          (plist-get (repo-dashboard--current-branch-status record)
                                     :branch))
                     "HEAD"))
         (upstream (plist-get status :upstream))
         (buffer (get-buffer-create repo-dashboard-log-buffer-name)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "%s\n%s\n\n"
                        (plist-get record :name)
                        (plist-get record :display-path)))
        (if upstream
            (repo-dashboard--insert-git-output
             dir (list "log" "--left-right" "--graph" "--decorate" "--oneline"
                       (format "%s...%s" branch upstream)))
          (repo-dashboard--insert-git-output
           dir (list "log" "--graph" "--decorate" "--oneline" "-n" "80"
                     branch)))
        (special-mode)))
    (pop-to-buffer buffer)))

;;; Bulk safe action

(defun repo-dashboard-execute-safe ()
  "Execute the pending safe sync action on marked repos, or current repo.
Depending on each repository's sync policy (see
`repo-dashboard-sync-policies'), the safe action is a fast-forward
pull, a push of a cleanly-ahead current branch, or a wip
snapshot-and-push."
  (interactive)
  (repo-dashboard--execute-sync-actions (repo-dashboard--records-for-action)))

(defun repo-dashboard-execute-safe-all ()
  "Execute the pending safe sync action on all eligible repositories."
  (interactive)
  (repo-dashboard--execute-sync-actions repo-dashboard--records))

;;; Sync policies (wrap-up)

(defcustom repo-dashboard-sync-policies nil
  "Alist mapping repositories to sync policies.
Each key is a repository path (tilde allowed) or a bare repository
name.  Each value is one of:

- `origin' -- a normal repository: the dashboard expects it clean and
  `repo-dashboard-execute-safe' pushes branches that are ahead of
  their upstream and cleanly fast-forwardable.
- `wip', or (wip . REMOTE) -- a working repository:
  `repo-dashboard-execute-safe' snapshot-commits any uncommitted
  changes and force-pushes the current branch to
  NAMESPACE/BRANCH on REMOTE (default \"wip\"); see
  `repo-dashboard-wip-namespace'.
- `ignore' -- expected to be dirty or diverged: shown as \"expected\"
  and never acted on.

Repositories not listed default to `origin'.  Manifest lines can also
set the policy with a field of the form sync=origin, sync=wip,
sync=wip:REMOTE, or sync=ignore."
  :type '(alist :key-type string :value-type sexp)
  :group 'repo-dashboard)

(defcustom repo-dashboard-wip-namespace 'system-name
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
  :group 'repo-dashboard)

(defcustom repo-dashboard-snapshot-message-function
  #'repo-dashboard-default-snapshot-message
  "Function returning the commit message for wip snapshot commits.
Called with the repository record."
  :type 'function
  :group 'repo-dashboard)

(defun repo-dashboard-default-snapshot-message (_record)
  "Return the default wip snapshot commit message."
  (format-time-string "; wip snapshot %F %R"))

(defun repo-dashboard--parse-sync-token (token)
  "Return a sync policy for manifest TOKEN, or nil if unrecognized."
  (pcase token
    ("origin" 'origin)
    ("ignore" 'ignore)
    ("wip" 'wip)
    ((pred (string-prefix-p "wip:"))
     (cons 'wip (substring token (length "wip:"))))
    (_ nil)))

(defun repo-dashboard--sync-policy (record)
  "Return normalized sync policy (SYMBOL . REMOTE) for RECORD."
  (let ((raw (or (plist-get record :sync-policy)
                 (cdr (seq-find
                       (lambda (cell)
                         (let ((key (car cell)))
                           (or (equal key (plist-get record :name))
                               (and (string-match-p "/" key)
                                    (file-exists-p
                                     (repo-dashboard--expand-path key))
                                    (file-equal-p
                                     (repo-dashboard--expand-path key)
                                     (plist-get record :path))))))
                       repo-dashboard-sync-policies)))))
    (pcase raw
      ('ignore '(ignore . nil))
      ('wip '(wip . "wip"))
      (`(wip . ,remote) (cons 'wip remote))
      (_ '(origin . nil)))))

(defun repo-dashboard--wip-namespace ()
  "Return the effective wip namespace string, or nil."
  (pcase repo-dashboard-wip-namespace
    ('system-name
     (let ((name (downcase (car (split-string (system-name) "\\.")))))
       (replace-regexp-in-string "[^a-z0-9-]" "-" name)))
    ((and (pred stringp) name) name)
    (_ nil)))

(defun repo-dashboard--wip-target (branch)
  "Return the namespaced wip target ref name for BRANCH."
  (let ((namespace (repo-dashboard--wip-namespace)))
    (if namespace (format "%s/%s" namespace branch) branch)))

(defun repo-dashboard--git-status-output (dir &rest args)
  "Run Git in DIR with ARGS; return (EXIT-STATUS . OUTPUT)."
  (with-temp-buffer
    (let ((status (apply #'process-file
                         repo-dashboard-git-program nil t nil
                         "-C" dir args)))
      (cons status (string-trim (buffer-string))))))

(defun repo-dashboard--wip-scan-extras (dir branch remote)
  "Return wip scan info for DIR's BRANCH against REMOTE.
Returns a plist with :wip-push (non-nil when the namespaced remote ref
is missing or behind BRANCH) and :siblings (strings describing other
namespaces' refs holding commits absent from BRANCH).  Uses existing
remote-tracking refs; fetch (\\[repo-dashboard-fetch-all]) to refresh
them."
  (let* ((target (repo-dashboard--wip-target branch))
         (remote-ref (format "%s/%s" remote target))
         (has-ref (repo-dashboard--git-success-p
                   dir "rev-parse" "--verify" "--quiet" remote-ref))
         (ahead (and has-ref
                     (car (repo-dashboard--ahead-behind
                           dir branch remote-ref))))
         (namespace (repo-dashboard--wip-namespace))
         siblings)
    (dolist (ref (repo-dashboard--git-lines
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
          (let ((missing (cdr (repo-dashboard--ahead-behind
                               dir branch ref))))
            (when (and missing (> missing 0))
              (push (format "%s +%d" sibling-ns missing) siblings))))))
    (list :wip-push (or (not has-ref) (> (or ahead 0) 0))
          :siblings (nreverse siblings))))

(defun repo-dashboard--snapshot-commit (record)
  "Snapshot-commit uncommitted changes in RECORD.
Return a result string, or nil when the commit failed."
  (let ((dir (plist-get record :path))
        (message (funcall repo-dashboard-snapshot-message-function record)))
    (and (repo-dashboard--git-success-p dir "add" "-A")
         (repo-dashboard--git-success-p dir "commit" "-m" message)
         (format "snapshot committed (%s)" message))))

(defun repo-dashboard--wip-sync (record remote)
  "Fetch, snapshot and push RECORD's current branch to REMOTE.
Return (t . MESSAGE) on success, (nil . MESSAGE) on failure."
  (let* ((dir (plist-get record :path))
         (branch (plist-get record :branch))
         (target (repo-dashboard--wip-target branch)))
    (cond
     ((not (repo-dashboard--git-success-p dir "fetch" remote))
      (cons nil (format "fetch from %s failed" remote)))
     ((and (> (repo-dashboard--dirty-count dir) 0)
           (not (repo-dashboard--snapshot-commit record)))
      (cons nil "snapshot commit failed"))
     (t
      (let ((result (repo-dashboard--git-status-output
                     dir "push"
                     (format "--force-with-lease=refs/heads/%s" target)
                     remote
                     (format "%s:refs/heads/%s" branch target))))
        (if (zerop (car result))
            (cons t (format "pushed %s to %s/%s" branch remote target))
          (cons nil (cdr result))))))))

(defun repo-dashboard--origin-push (record)
  "Push RECORD's current branch to its upstream.
Return (t . MESSAGE) on success, (nil . MESSAGE) on failure."
  (let* ((dir (plist-get record :path))
         (current (repo-dashboard--current-branch-status record))
         (branch (plist-get current :branch))
         (upstream (plist-get current :upstream))
         (remote (car (split-string upstream "/")))
         (result (repo-dashboard--git-status-output
                  dir "push" remote branch)))
    (if (zerop (car result))
        (cons t (format "pushed %s to %s" branch upstream))
      (cons nil (cdr result)))))

(defun repo-dashboard--sync-action (record)
  "Return the pending sync action symbol for RECORD, or nil.
One of `pull', `push' or `wip-sync'."
  (when (eq (plist-get record :kind) 'git)
    (pcase-let ((`(,policy . ,_remote) (repo-dashboard--sync-policy record)))
      (pcase policy
        ('ignore nil)
        ('wip (and (or (> (plist-get record :dirty) 0)
                       (plist-get record :wip-push))
                   'wip-sync))
        (_ (cond
            ((repo-dashboard--safe-pull-p record) 'pull)
            ((repo-dashboard--safe-push-p record) 'push)))))))

(defun repo-dashboard--execute-sync-actions (records)
  "Execute pending sync actions for RECORDS, reporting a receipt."
  (let* ((dashboard (current-buffer))
         (actionable (seq-filter #'repo-dashboard--sync-action records))
         (summary (mapconcat
                   (lambda (kind)
                     (let ((n (seq-count
                               (lambda (r)
                                 (eq (repo-dashboard--sync-action r) kind))
                               actionable)))
                       (and (> n 0) (format "%s %d" kind n))))
                   '(pull push wip-sync) " ")))
    (unless actionable
      (user-error "No safe sync actions available"))
    (when (yes-or-no-p (format "Sync (%s repo(s): %s)? "
                               (length actionable) (string-trim summary)))
      (let (failures)
        (dolist (record actionable)
          (let* ((action (repo-dashboard--sync-action record))
                 (result
                  (pcase action
                    ('pull (repo-dashboard--git-status-output
                            (plist-get record :path) "pull" "--ff-only"))
                    ('push (repo-dashboard--origin-push record))
                    ('wip-sync
                     (repo-dashboard--wip-sync
                      record
                      (cdr (repo-dashboard--sync-policy record))))))
                 (ok (pcase action
                       ('pull (zerop (car result)))
                       (_ (car result)))))
            (unless ok
              (push (format "%s: %s" (plist-get record :name) (cdr result))
                    failures))
            (repo-dashboard--rescan-record record dashboard)))
        (if failures
            (message "Sync finished with failures: %s"
                     (string-join (nreverse failures) " | "))
          (message "Sync finished: %s" (string-trim summary)))))))

(defun repo-dashboard-outgoing-log ()
  "Show the log of commits that a sync would push for the repo at point."
  (interactive)
  (let* ((record (repo-dashboard--require-record))
         (branch (plist-get record :branch))
         (range
          (pcase-let ((`(,policy . ,remote)
                       (repo-dashboard--sync-policy record)))
            (pcase policy
              ('wip
               (let ((remote-ref (format "%s/%s" remote
                                         (repo-dashboard--wip-target branch))))
                 (if (repo-dashboard--git-success-p
                      (plist-get record :path)
                      "rev-parse" "--verify" "--quiet" remote-ref)
                     (format "%s..%s" remote-ref branch)
                   branch)))
              (_
               (let ((current (repo-dashboard--current-branch-status record)))
                 (if (and current (plist-get current :upstream))
                     (format "%s..%s" (plist-get current :upstream) branch)
                   branch)))))))
    (repo-dashboard--display-git-command
     record repo-dashboard-log-buffer-name
     "log" "--stat" range)))

;;; Shell commands

(defun repo-dashboard--shell-buffer ()
  "Return the command output buffer."
  (get-buffer-create repo-dashboard-command-buffer-name))

(defun repo-dashboard--insert-command-header (record command)
  "Insert a command header for RECORD and COMMAND."
  (insert (format "\n\n== %s (%s)\n$ %s\n\n"
                  (plist-get record :name)
                  (plist-get record :display-path)
                  command)))

(defun repo-dashboard--insert-prefixed-lines (buffer prefix chunk pending)
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

(defun repo-dashboard--run-shell-command (records command async)
  "Run shell COMMAND in RECORDS.
When ASYNC is non-nil, start one process per repository and prefix
output lines with the repository name."
  (let ((buffer (repo-dashboard--shell-buffer)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "repo-dashboard command: %s\n" command))))
    (if async
        (dolist (record records)
          (let* ((default-directory (repo-dashboard--record-directory record))
                 (repo-name (plist-get record :name))
                 (prefix (format "[%s] " repo-name))
                 (pending nil))
            (with-current-buffer buffer
              (let ((inhibit-read-only t))
                (insert (format "\n== %s (%s)\n"
                                repo-name
                                (plist-get record :display-path)))))
            (make-process
             :name (format "repo-dashboard:%s" (plist-get record :name))
             :buffer buffer
             :command (list shell-file-name shell-command-switch command)
             :noquery t
             :filter (lambda (_process chunk)
                       (setq pending
                             (repo-dashboard--insert-prefixed-lines
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
        (let ((default-directory (repo-dashboard--record-directory record)))
          (with-current-buffer buffer
            (let ((inhibit-read-only t))
              (repo-dashboard--insert-command-header record command)
              (call-process shell-file-name nil buffer nil
                            shell-command-switch command))))))
    (with-current-buffer buffer
      (special-mode))
    (pop-to-buffer buffer)))

(defun repo-dashboard-shell-command (command)
  "Run shell COMMAND in marked repos, or the repo at point."
  (interactive
   (list (read-shell-command "Shell command in repos: ")))
  (repo-dashboard--run-shell-command (repo-dashboard--records-for-action)
                                     command nil))

(defun repo-dashboard-async-shell-command (command)
  "Run shell COMMAND asynchronously in marked repos, or the repo at point."
  (interactive
   (list (read-shell-command "Async shell command in repos: ")))
  (repo-dashboard--run-shell-command (repo-dashboard--records-for-action)
                                     command t))

;;; Keymap and mode

(defvar repo-dashboard-mark-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "p") #'repo-dashboard-mark-safe-pullable)
    (define-key map (kbd "P") #'repo-dashboard-mark-safe-pushable)
    (define-key map (kbd "d") #'repo-dashboard-mark-dirty)
    (define-key map (kbd "b") #'repo-dashboard-mark-behind)
    (define-key map (kbd "a") #'repo-dashboard-mark-ahead)
    (define-key map (kbd "s") #'repo-dashboard-mark-stale)
    map)
  "Prefix keymap for repository mark commands.")

(defvar repo-dashboard-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "n") #'repo-dashboard-next-line)
    (define-key map (kbd "p") #'repo-dashboard-previous-line)
    (define-key map (kbd "g") #'repo-dashboard-refresh)
    (define-key map (kbd ".") #'repo-dashboard-refresh-at-point)
    (define-key map (kbd "RET") #'repo-dashboard-vc-dir)
    (define-key map (kbd "j") #'repo-dashboard-dired)
    (define-key map (kbd "C-x g") #'repo-dashboard-magit-status)
    (define-key map (kbd "=") #'repo-dashboard-diff)
    (define-key map (kbd "l") #'repo-dashboard-log)
    (define-key map (kbd "f") #'repo-dashboard-fetch)
    (define-key map (kbd "F") #'repo-dashboard-fetch-all)
    (define-key map (kbd "+") #'repo-dashboard-pull)
    (define-key map (kbd "P") #'repo-dashboard-push)
    (define-key map (kbd "m") #'repo-dashboard-mark)
    (define-key map (kbd "u") #'repo-dashboard-unmark)
    (define-key map (kbd "t") #'repo-dashboard-toggle-mark)
    (define-key map (kbd "U") #'repo-dashboard-unmark-all)
    (define-key map (kbd "*") repo-dashboard-mark-map)
    (define-key map (kbd "x") #'repo-dashboard-execute-safe)
    (define-key map (kbd "X") #'repo-dashboard-execute-safe-all)
    (define-key map (kbd "o") #'repo-dashboard-outgoing-log)
    (define-key map (kbd "!") #'repo-dashboard-shell-command)
    (define-key map (kbd "&") #'repo-dashboard-async-shell-command)
    (define-key map (kbd "?") #'describe-mode)
    map)
  "Keymap used in `repo-dashboard-mode'.")

;;;###autoload
(define-derived-mode repo-dashboard-mode tabulated-list-mode "Repo Dashboard"
  "Major mode for a Dired-like dashboard of Git repositories."
  (repo-dashboard--ensure-mark-table)
  (setq tabulated-list-format
        [("M" 1 nil)
         ("Repo" 22 t)
         ("Group" 14 t)
         ("Branch" 24 t)
         ("D" 4 tabulated-list-entry-size->)
         ("A" 4 tabulated-list-entry-size->)
         ("B" 4 tabulated-list-entry-size->)
         ("State" 16 t)
         ("Details" 30 t)
         ("Path" 0 t)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key nil)
  (add-hook 'tabulated-list-revert-hook #'repo-dashboard-refresh nil t)
  (tabulated-list-init-header))

;;;###autoload
(defun repo-dashboard (&optional sources buffer-name)
  "Open a repository dashboard.
When SOURCES is non-nil, use it as the source function list for the
created dashboard buffer.  BUFFER-NAME overrides
`repo-dashboard-buffer-name'."
  (interactive)
  (let ((buffer (get-buffer-create (or buffer-name repo-dashboard-buffer-name))))
    (with-current-buffer buffer
      (repo-dashboard-mode)
      (setq-local repo-dashboard--source-functions sources)
      (repo-dashboard-refresh))
    (pop-to-buffer buffer)))

;;;###autoload
(defun repo-dashboard-emacs-packages ()
  "Open a repository dashboard for Emacs package repositories."
  (interactive)
  (repo-dashboard '(repo-dashboard-source-emacs-package-roots)
                  "*Repo Dashboard: Emacs Packages*"))

(provide 'repo-dashboard)

;;; repo-dashboard.el ends here
