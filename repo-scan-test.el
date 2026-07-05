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

(ert-deftest repo-scan-parse-manifest-path-only ()
  (let ((repo (repo-scan--parse-manifest-line
               "~/work/example"
               "/tmp/repos.manifest")))
    (should (equal (plist-get repo :name) "example"))
    (should-not (plist-get repo :origin))))

(ert-deftest repo-scan-parse-manifest-ignores-comments ()
  (should-not (repo-scan--parse-manifest-line "" "/tmp/repos.manifest"))
  (should-not (repo-scan--parse-manifest-line "  # comment" "/tmp/repos.manifest")))

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
                 "unpushed"))
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
                   "a +1, b -2, +1 more"))))

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
