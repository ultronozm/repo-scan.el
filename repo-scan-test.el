;;; repo-scan-test.el --- Tests for repo-scan -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Paul D. Nelson

;;; Commentary:

;; Run with:
;;
;;   emacs -Q --batch -L . -l repo-scan-test.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'repo-scan)

(defun repo-scan-test--wait-for-async-refresh (&optional timeout)
  "Wait until async dashboard refresh finishes, or fail after TIMEOUT seconds."
  (let ((deadline (+ (float-time) (or timeout 10))))
    (while (and (or repo-scan--scan-active
                    repo-scan--scan-queue)
                (< (float-time) deadline))
      (accept-process-output nil 0.1))
    (should-not repo-scan--scan-active)
    (should-not repo-scan--scan-queue)))

(ert-deftest repo-scan-parse-manifest-line ()
  (let ((repo (repo-scan--parse-manifest-line
               "~/work/example git@github.com:user/example.git upstream=https://example.invalid/x.git"
               "/tmp/repos.manifest")))
    (should (equal (plist-get repo :name) "example"))
    (should (equal (plist-get repo :display-path) "~/work/example"))
    (should (equal (plist-get repo :origin)
                   "git@github.com:user/example.git"))
    (should (equal (plist-get repo :extra-remote)
                   "upstream=https://example.invalid/x.git"))
    (should (equal (plist-get repo :group) "manifest"))))

(ert-deftest repo-scan-parse-manifest-extra-sentinel ()
  (let ((repo (repo-scan--parse-manifest-line
               "~/work/example git@github.com:user/example.git -"
               "/tmp/repos.manifest")))
    (should (equal (plist-get repo :origin)
                   "git@github.com:user/example.git"))
    (should-not (plist-get repo :extra-remote))))

(ert-deftest repo-scan-parse-manifest-expected-local ()
  (let ((repo (repo-scan--parse-manifest-line
               "~/work/example git@github.com:user/example.git upstream=https://example.invalid/x.git expected-local=working,scratch"
               "/tmp/repos.manifest")))
    (should (equal (plist-get repo :extra-remote)
                   "upstream=https://example.invalid/x.git"))
    (should (equal (plist-get repo :expected-local-branches)
                   '("working" "scratch")))))

(ert-deftest repo-scan-parse-manifest-path-only ()
  (let ((repo (repo-scan--parse-manifest-line
               "~/work/example"
               "/tmp/repos.manifest")))
    (should (equal (plist-get repo :name) "example"))
    (should-not (plist-get repo :origin))))

(ert-deftest repo-scan-parse-manifest-ignores-comments ()
  (should-not (repo-scan--parse-manifest-line "" "/tmp/repos.manifest"))
  (should-not (repo-scan--parse-manifest-line "  # comment" "/tmp/repos.manifest")))

(ert-deftest repo-scan-collect-descriptors-normalizes-custom-sources ()
  (let* ((dir (make-temp-file "repo-scan-test" t))
         (repo-scan--source-functions
          (list (lambda ()
                  (list (list :path dir
                              :group "custom"
                              :source 'test-source))))))
    (unwind-protect
        (let ((repo (car (repo-scan--collect-descriptors))))
          (should (equal (plist-get repo :path) dir))
          (should (equal (plist-get repo :name)
                         (file-name-nondirectory
                          (directory-file-name dir))))
          (should (stringp (plist-get repo :display-path)))
          (should (equal (plist-get repo :group) "custom")))
      (delete-directory dir t))))

(ert-deftest repo-scan-normalize-url ()
  (should (equal (repo-scan--normalize-url "git@github.com:user/repo.git")
                 "github.com/user/repo"))
  (should (equal (repo-scan--normalize-url "https://github.com/user/repo.git")
                 "github.com/user/repo"))
  (should (equal (repo-scan--normalize-url "ssh://git@github.com/user/repo.git")
                 "github.com/user/repo")))

(ert-deftest repo-scan-safe-pull-p ()
  (let ((record (list :kind 'git
                      :dirty 0
                      :branches (list (list :branch "main"
                                            :upstream "origin/main"
                                            :ahead 0
                                            :behind 2
                                            :current t)))))
    (should (repo-scan--safe-pull-p record))))

(ert-deftest repo-scan-safe-pull-rejects-dirty ()
  (let ((record (list :kind 'git
                      :dirty 1
                      :branches (list (list :branch "main"
                                            :upstream "origin/main"
                                            :ahead 0
                                            :behind 2
                                            :current t)))))
    (should-not (repo-scan--safe-pull-p record))))

(ert-deftest repo-scan-safe-push-p ()
  (let ((record (list :kind 'git
                      :dirty 0
                      :branches (list (list :branch "main"
                                            :upstream "origin/main"
                                            :ahead 3
                                            :behind 0
                                            :current t)))))
    (should (repo-scan--safe-push-p record))))

(ert-deftest repo-scan-record-state-branches ()
  (should (equal (repo-scan--record-state
                  (list :kind 'git
                        :dirty 1
                        :unpushed 0
                        :branches (list (list :branch "main"
                                              :ahead 0
                                              :behind 1
                                              :current t))))
                 "dirty+drift"))
  (should (equal (repo-scan--record-state
                  (list :kind 'git
                        :dirty 0
                        :unpushed 0
                        :branches (list (list :branch "main"
                                              :ahead 1
                                              :behind 1
                                              :current t))))
                 "diverged"))
  (should (equal (repo-scan--record-state
                  (list :kind 'git
                        :dirty 0
                        :unpushed 0
                        :branches (list (list :branch "main"
                                              :ahead 0
                                              :behind 2
                                              :current t))))
                 "pullable"))
  (should (equal (repo-scan--record-state
                  (list :kind 'git
                        :dirty 0
                        :unpushed 0
                        :branches (list (list :branch "main"
                                              :ahead 2
                                              :behind 0
                                              :current t))))
                 "pushable"))
  (should (equal (repo-scan--record-state
                  (list :kind 'git
                        :dirty 0
                        :unpushed 0
                        :branches (list (list :branch "main"
                                              :ahead 0
                                              :behind 0
                                              :current t)
                                        (list :branch "old"
                                              :ahead 0
                                              :behind 1
                                              :current nil))))
                 "branch drift"))
  (should (equal (repo-scan--record-state
                  (list :kind 'git
                        :dirty 0
                        :unpushed 2
                        :branches nil))
                 "local-only"))
  (should (equal (repo-scan--record-state
                  (list :kind 'git
                        :dirty 0
                        :unpushed 0
                        :remote-problems '("origin mismatch")
                        :branches nil))
                 "remote problem"))
  (should (equal (repo-scan--record-state
                  (list :kind 'git
                        :dirty 0
                        :unpushed 0
                        :branches nil))
                 "ok")))

(ert-deftest repo-scan-branches-text-truncates ()
  (let ((record (list :branches (list (list :branch "a"
                                            :ahead 1
                                            :behind 0)
                                      (list :branch "b"
                                            :ahead 0
                                            :behind 2)
                                      (list :branch "c"
                                            :ahead 0
                                            :behind 3)))))
    (should (equal (repo-scan--branches-text record)
                   "drift: a +1, b -2, +1 more"))))

(ert-deftest repo-scan-branches-text-prioritizes-local-only ()
  (let ((record (list :unpushed 2
                      :unpushed-branches
                      (list (list :branch "topic-a" :count 1)
                            (list :branch "topic-b" :count 1))
                      :branches (list (list :branch "old"
                                            :ahead 0
                                            :behind 2)))))
    (should (equal (repo-scan--branches-text record)
                   "local-only: topic-a +1, topic-b +1; drift: old -2"))))

(ert-deftest repo-scan-details-column-expands ()
  (with-temp-buffer
    (repo-scan-mode)
    (let ((last-column (aref tabulated-list-format
                             (1- (length tabulated-list-format)))))
      (should (equal (nth 0 last-column) "Details"))
      (should (= (nth 1 last-column) 0)))))

(ert-deftest repo-scan-remote-problems ()
  (let ((dir (make-temp-file "repo-scan-test" t)))
    (unwind-protect
        (progn
          (should (repo-scan--git-success-p dir "init"))
          (should (repo-scan--git-success-p
                   dir "remote" "add" "origin"
                   "git@github.com:user/repo.git"))
          (should-not
           (repo-scan--remote-problems
            dir (list :origin "https://github.com/user/repo.git")))
          (should (equal
                   (repo-scan--remote-problems
                    dir (list :origin "https://github.com/other/repo.git"))
                   '("origin mismatch"))))
      (delete-directory dir t))))

(ert-deftest repo-scan-expected-local-branches-do-not-count-as-local-only ()
  (let ((dir (make-temp-file "repo-scan-test" t))
        (remote (make-temp-file "repo-scan-remote" t)))
    (unwind-protect
        (progn
          (should (repo-scan--git-success-p dir "init" "-b" "main"))
          (should (repo-scan--git-success-p dir "config" "user.email"
                                            "repo-scan@example.invalid"))
          (should (repo-scan--git-success-p dir "config" "user.name"
                                            "Repo Scan Test"))
          (should (repo-scan--git-success-p remote "init" "--bare"))
          (with-temp-file (expand-file-name "file.txt" dir)
            (insert "base\n"))
          (should (repo-scan--git-success-p dir "add" "file.txt"))
          (should (repo-scan--git-success-p dir "commit" "-m" "base"))
          (should (repo-scan--git-success-p dir "remote" "add" "origin"
                                            remote))
          (should (repo-scan--git-success-p dir "push" "-u" "origin"
                                            "main"))
          (should (repo-scan--git-success-p dir "checkout" "-b" "working"))
          (with-temp-file (expand-file-name "working.txt" dir)
            (insert "generated\n"))
          (should (repo-scan--git-success-p dir "add" "working.txt"))
          (should (repo-scan--git-success-p dir "commit" "-m" "working"))
          (should (repo-scan--git-success-p dir "checkout" "main"))
          (should (repo-scan--git-success-p dir "checkout" "-b" "topic"))
          (with-temp-file (expand-file-name "topic.txt" dir)
            (insert "real topic\n"))
          (should (repo-scan--git-success-p dir "add" "topic.txt"))
          (should (repo-scan--git-success-p dir "commit" "-m" "topic"))
          (should (repo-scan--git-success-p dir "checkout" "main"))
          (let* ((record (repo-scan--scan
                          (repo-scan--descriptor
                           dir :expected-local-branches '("working"))))
                 (branches (plist-get record :unpushed-branches)))
            (should (= (plist-get record :unpushed) 1))
            (should (equal (mapcar (lambda (branch)
                                     (plist-get branch :branch))
                                   branches)
                           '("topic")))
            (should (equal (plist-get record :state) "local-only"))))
      (delete-directory dir t)
      (delete-directory remote t))))

(ert-deftest repo-scan-refresh-async-populates-records ()
  (let ((dir (make-temp-file "repo-scan-test" t)))
    (unwind-protect
        (progn
          (should (repo-scan--git-success-p dir "init"))
          (with-temp-buffer
            (repo-scan-mode)
            (setq-local repo-scan--source-functions
                        (list (lambda ()
                                (list (repo-scan--descriptor
                                       dir :group "test")))))
            (let ((repo-scan-refresh-concurrency 1))
              (repo-scan-refresh)
              (should (= (length repo-scan--records) 1))
              (should (eq (plist-get (car repo-scan--records) :kind)
                          'pending))
              (repo-scan-test--wait-for-async-refresh)
              (should (= (length repo-scan--records) 1))
              (should (eq (plist-get (car repo-scan--records) :kind)
                          'git))
              (should (equal (plist-get (car repo-scan--records) :state)
                             "ok")))))
      (delete-directory dir t))))

(ert-deftest repo-scan-call-vc-command-uses-vc-dir-buffer ()
  (let* ((record (list :path default-directory
                       :display-path default-directory))
         (vc-buf (get-buffer-create " *repo-scan-test-vc-dir*"))
         seen)
    (unwind-protect
        (progn
          ;; Pre-create a vc-dir-mode buffer so --find-vc-dir-buffer finds it.
          (with-current-buffer vc-buf
            (setq default-directory (file-name-as-directory
                                     (expand-file-name default-directory)))
            (setq major-mode 'vc-dir-mode))
          (cl-letf (((symbol-function 'repo-scan-test--vc-command)
                     (lambda (&optional _arg)
                       (interactive "P")
                       (setq seen
                             (list :buffer (current-buffer)
                                   :vc-dir-p (derived-mode-p 'vc-dir-mode)
                                   :prefix current-prefix-arg
                                   :directory default-directory)))))
            (should (eq (repo-scan--call-vc-command
                         record 'repo-scan-test--vc-command #'ignore '(4))
                        'vc))
            (should (plist-get seen :vc-dir-p))
            (should (equal (plist-get seen :prefix) '(4)))
            (should (equal (file-name-as-directory (plist-get seen :directory))
                           (file-name-as-directory
                            (expand-file-name default-directory))))))
      (kill-buffer vc-buf))))

(provide 'repo-scan-test)

;;; repo-scan-test.el ends here
