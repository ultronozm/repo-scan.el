;;; repo-dashboard-test.el --- Tests for repo-dashboard -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Paul D. Nelson

;;; Commentary:

;; Run with:
;;
;;   emacs -Q --batch -L . -l repo-dashboard-test.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'repo-dashboard)

(defun repo-dashboard-test--wait-for-async-refresh (&optional timeout)
  "Wait until async dashboard refresh finishes, or fail after TIMEOUT seconds."
  (let ((deadline (+ (float-time) (or timeout 10))))
    (while (and (or repo-dashboard--scan-active
                    repo-dashboard--scan-queue)
                (< (float-time) deadline))
      (accept-process-output nil 0.1))
    (should-not repo-dashboard--scan-active)
    (should-not repo-dashboard--scan-queue)))

(ert-deftest repo-dashboard-parse-manifest-line ()
  (let ((repo (repo-dashboard--parse-manifest-line
               "~/work/example git@github.com:user/example.git upstream=https://example.invalid/x.git"
               "/tmp/repos.manifest")))
    (should (equal (plist-get repo :name) "example"))
    (should (equal (plist-get repo :display-path) "~/work/example"))
    (should (equal (plist-get repo :origin)
                   "git@github.com:user/example.git"))
    (should (equal (plist-get repo :extra-remote)
                   "upstream=https://example.invalid/x.git"))
    (should (equal (plist-get repo :group) "manifest"))))

(ert-deftest repo-dashboard-parse-manifest-extra-sentinel ()
  (let ((repo (repo-dashboard--parse-manifest-line
               "~/work/example git@github.com:user/example.git -"
               "/tmp/repos.manifest")))
    (should (equal (plist-get repo :origin)
                   "git@github.com:user/example.git"))
    (should-not (plist-get repo :extra-remote))))

(ert-deftest repo-dashboard-parse-manifest-path-only ()
  (let ((repo (repo-dashboard--parse-manifest-line
               "~/work/example"
               "/tmp/repos.manifest")))
    (should (equal (plist-get repo :name) "example"))
    (should-not (plist-get repo :origin))))

(ert-deftest repo-dashboard-parse-manifest-ignores-comments ()
  (should-not (repo-dashboard--parse-manifest-line "" "/tmp/repos.manifest"))
  (should-not (repo-dashboard--parse-manifest-line "  # comment" "/tmp/repos.manifest")))

(ert-deftest repo-dashboard-collect-descriptors-normalizes-custom-sources ()
  (let* ((dir (make-temp-file "repo-dashboard-test" t))
         (repo-dashboard--source-functions
          (list (lambda ()
                  (list (list :path dir
                              :group "custom"
                              :source 'test-source))))))
    (unwind-protect
        (let ((repo (car (repo-dashboard--collect-descriptors))))
          (should (equal (plist-get repo :path) dir))
          (should (equal (plist-get repo :name)
                         (file-name-nondirectory
                          (directory-file-name dir))))
          (should (stringp (plist-get repo :display-path)))
          (should (equal (plist-get repo :group) "custom")))
      (delete-directory dir t))))

(ert-deftest repo-dashboard-normalize-url ()
  (should (equal (repo-dashboard--normalize-url "git@github.com:user/repo.git")
                 "github.com/user/repo"))
  (should (equal (repo-dashboard--normalize-url "https://github.com/user/repo.git")
                 "github.com/user/repo"))
  (should (equal (repo-dashboard--normalize-url "ssh://git@github.com/user/repo.git")
                 "github.com/user/repo")))

(ert-deftest repo-dashboard-safe-pull-p ()
  (let ((record (list :kind 'git
                      :dirty 0
                      :branches (list (list :branch "main"
                                            :upstream "origin/main"
                                            :ahead 0
                                            :behind 2
                                            :current t)))))
    (should (repo-dashboard--safe-pull-p record))))

(ert-deftest repo-dashboard-safe-pull-rejects-dirty ()
  (let ((record (list :kind 'git
                      :dirty 1
                      :branches (list (list :branch "main"
                                            :upstream "origin/main"
                                            :ahead 0
                                            :behind 2
                                            :current t)))))
    (should-not (repo-dashboard--safe-pull-p record))))

(ert-deftest repo-dashboard-safe-push-p ()
  (let ((record (list :kind 'git
                      :dirty 0
                      :branches (list (list :branch "main"
                                            :upstream "origin/main"
                                            :ahead 3
                                            :behind 0
                                            :current t)))))
    (should (repo-dashboard--safe-push-p record))))

(ert-deftest repo-dashboard-record-state-branches ()
  (should (equal (repo-dashboard--record-state
                  (list :kind 'git
                        :dirty 1
                        :unpushed 0
                        :branches (list (list :branch "main"
                                              :ahead 0
                                              :behind 1
                                              :current t))))
                 "dirty+drift"))
  (should (equal (repo-dashboard--record-state
                  (list :kind 'git
                        :dirty 0
                        :unpushed 0
                        :branches (list (list :branch "main"
                                              :ahead 1
                                              :behind 1
                                              :current t))))
                 "diverged"))
  (should (equal (repo-dashboard--record-state
                  (list :kind 'git
                        :dirty 0
                        :unpushed 0
                        :branches (list (list :branch "main"
                                              :ahead 0
                                              :behind 2
                                              :current t))))
                 "pullable"))
  (should (equal (repo-dashboard--record-state
                  (list :kind 'git
                        :dirty 0
                        :unpushed 0
                        :branches (list (list :branch "main"
                                              :ahead 2
                                              :behind 0
                                              :current t))))
                 "pushable"))
  (should (equal (repo-dashboard--record-state
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
  (should (equal (repo-dashboard--record-state
                  (list :kind 'git
                        :dirty 0
                        :unpushed 2
                        :branches nil))
                 "local-only"))
  (should (equal (repo-dashboard--record-state
                  (list :kind 'git
                        :dirty 0
                        :unpushed 0
                        :remote-problems '("origin mismatch")
                        :branches nil))
                 "remote problem"))
  (should (equal (repo-dashboard--record-state
                  (list :kind 'git
                        :dirty 0
                        :unpushed 0
                        :branches nil))
                 "ok")))

(ert-deftest repo-dashboard-branches-text-truncates ()
  (let ((record (list :branches (list (list :branch "a"
                                            :ahead 1
                                            :behind 0)
                                      (list :branch "b"
                                            :ahead 0
                                            :behind 2)
                                      (list :branch "c"
                                            :ahead 0
                                            :behind 3)))))
    (should (equal (repo-dashboard--branches-text record)
                   "drift: a +1, b -2, +1 more"))))

(ert-deftest repo-dashboard-branches-text-prioritizes-local-only ()
  (let ((record (list :unpushed 2
                      :unpushed-branches
                      (list (list :branch "topic-a" :count 1)
                            (list :branch "topic-b" :count 1))
                      :branches (list (list :branch "old"
                                            :ahead 0
                                            :behind 2)))))
    (should (equal (repo-dashboard--branches-text record)
                   "local-only: topic-a +1, topic-b +1; drift: old -2"))))

(ert-deftest repo-dashboard-remote-problems ()
  (let ((dir (make-temp-file "repo-dashboard-test" t)))
    (unwind-protect
        (progn
          (should (repo-dashboard--git-success-p dir "init"))
          (should (repo-dashboard--git-success-p
                   dir "remote" "add" "origin"
                   "git@github.com:user/repo.git"))
          (should-not
           (repo-dashboard--remote-problems
            dir (list :origin "https://github.com/user/repo.git")))
          (should (equal
                   (repo-dashboard--remote-problems
                    dir (list :origin "https://github.com/other/repo.git"))
                   '("origin mismatch"))))
      (delete-directory dir t))))

(ert-deftest repo-dashboard-refresh-async-populates-records ()
  (let ((dir (make-temp-file "repo-dashboard-test" t)))
    (unwind-protect
        (progn
          (should (repo-dashboard--git-success-p dir "init"))
          (with-temp-buffer
            (repo-dashboard-mode)
            (setq-local repo-dashboard--source-functions
                        (list (lambda ()
                                (list (repo-dashboard--descriptor
                                       dir :group "test")))))
            (let ((repo-dashboard-refresh-concurrency 1))
              (repo-dashboard-refresh)
              (should (= (length repo-dashboard--records) 1))
              (should (eq (plist-get (car repo-dashboard--records) :kind)
                          'pending))
              (repo-dashboard-test--wait-for-async-refresh)
              (should (= (length repo-dashboard--records) 1))
              (should (eq (plist-get (car repo-dashboard--records) :kind)
                          'git))
              (should (equal (plist-get (car repo-dashboard--records) :state)
                             "ok")))))
      (delete-directory dir t))))

(ert-deftest repo-dashboard-call-vc-command-uses-vc-dir-buffer ()
  (let* ((record (list :path default-directory
                       :display-path default-directory))
         (vc-buf (get-buffer-create " *repo-dashboard-test-vc-dir*"))
         seen)
    (unwind-protect
        (progn
          ;; Pre-create a vc-dir-mode buffer so --find-vc-dir-buffer finds it.
          (with-current-buffer vc-buf
            (setq default-directory (file-name-as-directory
                                     (expand-file-name default-directory)))
            (setq major-mode 'vc-dir-mode))
          (cl-letf (((symbol-function 'repo-dashboard-test--vc-command)
                     (lambda (&optional _arg)
                       (interactive "P")
                       (setq seen
                             (list :buffer (current-buffer)
                                   :vc-dir-p (derived-mode-p 'vc-dir-mode)
                                   :prefix current-prefix-arg
                                   :directory default-directory)))))
            (should (eq (repo-dashboard--call-vc-command
                         record 'repo-dashboard-test--vc-command #'ignore '(4))
                        'vc))
            (should (plist-get seen :vc-dir-p))
            (should (equal (plist-get seen :prefix) '(4)))
            (should (equal (file-name-as-directory (plist-get seen :directory))
                           (file-name-as-directory
                            (expand-file-name default-directory))))))
      (kill-buffer vc-buf))))

(provide 'repo-dashboard-test)

;;; repo-dashboard-test.el ends here
