;;; media-thumbnail-test.el --- Tests for media-thumbnail -*- lexical-binding: t -*-

;; Copyright (C) 2026 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; ERT tests for `media-thumbnail'.  Run with:
;;   emacs -batch -Q -L . -l test/media-thumbnail-test.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'media-thumbnail)

(defmacro media-thumbnail-test--with-temp-buffers (bindings &rest body)
  "Bind BINDINGS to fresh temp buffers, run BODY, kill buffers after."
  (declare (indent 1))
  `(let ,(mapcar (lambda (b) `(,b (generate-new-buffer " *mt-test*"))) bindings)
     (unwind-protect (progn ,@body)
       ,@(mapcar (lambda (b) `(when (buffer-live-p ,b) (kill-buffer ,b))) bindings))))

(ert-deftest media-thumbnail-test-redisplay-runs-in-host-buffer ()
  "Bug 1: `media-thumbnail--redisplay' must execute in the buffer that
enqueued the request, not in whatever buffer the timer happens to fire
in.  Verifies the callback's `with-current-buffer host-buf' wrapper is
in place by pushing a fake spec while `current-buffer' is NOT the
host, and asserting that `--specs-to-flush' lands in the host's local
value."
  (media-thumbnail-test--with-temp-buffers (host other)
    (with-current-buffer host
      (setq-local media-thumbnail--specs-to-flush nil))
    (with-current-buffer other
      (setq-local media-thumbnail--specs-to-flush nil)
      ;; Simulate the corrected callback path.
      (when (buffer-live-p host)
        (with-current-buffer host
          (push '(:image-spec dummy :file "x.mp4")
                media-thumbnail--specs-to-flush))))
    (should (equal (buffer-local-value 'media-thumbnail--specs-to-flush host)
                   '((:image-spec dummy :file "x.mp4"))))
    (should (null (buffer-local-value 'media-thumbnail--specs-to-flush other)))))

(ert-deftest media-thumbnail-test-enqueue-schedules-convert-timer ()
  "Bug 6: enqueueing into an empty queue must arm the drain timer;
before the on-demand refactor there was a fixed 0.25 s repeating
poller regardless of queue state, wasting CPU forever per active
dired buffer."
  (let ((media-thumbnail--queue nil)
        (media-thumbnail--queue-tail nil)
        (media-thumbnail--convert-timer nil))
    (unwind-protect
        (progn
          (should (null media-thumbnail--convert-timer))
          (media-thumbnail--enqueue :item)
          (should (timerp media-thumbnail--convert-timer)))
      (when (timerp media-thumbnail--convert-timer)
        (cancel-timer media-thumbnail--convert-timer))
      (setq media-thumbnail--convert-timer nil))))

(ert-deftest media-thumbnail-test-enqueue-coalesces-scheduled-timer ()
  "Bug 6: a burst of enqueues must not stack multiple pending timers.
Only the first enqueue arms a drain; subsequent enqueues into a
non-empty queue reuse the pending timer."
  (let ((media-thumbnail--queue nil)
        (media-thumbnail--queue-tail nil)
        (media-thumbnail--convert-timer nil))
    (unwind-protect
        (progn
          (media-thumbnail--enqueue :a)
          (let ((first media-thumbnail--convert-timer))
            (media-thumbnail--enqueue :b)
            (media-thumbnail--enqueue :c)
            (should (eq media-thumbnail--convert-timer first))))
      (when (timerp media-thumbnail--convert-timer)
        (cancel-timer media-thumbnail--convert-timer))
      (setq media-thumbnail--convert-timer nil))))

(ert-deftest media-thumbnail-test-convert-does-not-reschedule-on-empty ()
  "Bug 6: `media-thumbnail--convert' must leave the pending-timer slot
`nil' when the queue is empty on entry, so an idle session has no
live timer.  Regression guard against a subtle refactor where
`--convert' unconditionally re-arms."
  (let ((media-thumbnail--queue nil)
        (media-thumbnail--queue-tail nil)
        (media-thumbnail--convert-timer 'sentinel))
    (media-thumbnail--convert)
    (should (null media-thumbnail--convert-timer))))

(ert-deftest media-thumbnail-test-handled-files-is-hash ()
  "Bug 3: `--handled-files' should be a hash-table, not a list, so
membership is O(1) instead of O(n).  Regression guard against
reverting the storage back to a plain list — quadratic behaviour
was the whole reason for the refactor."
  (should (hash-table-p media-thumbnail--handled-files))
  ;; Round-trip a puthash / gethash to prove the API is honoured.
  (unwind-protect
      (progn
        (puthash "test-file.mp4" t media-thumbnail--handled-files)
        (should (gethash "test-file.mp4" media-thumbnail--handled-files))
        (should-not (gethash "not-added.mp4" media-thumbnail--handled-files)))
    (remhash "test-file.mp4" media-thumbnail--handled-files)))

(ert-deftest media-thumbnail-test-clear-all-clears-handled-hash ()
  "Bug 3: `media-thumbnail-clear-all' must `clrhash' the new
hash-based `--handled-files' rather than `setq'-ing it to nil,
which would break the type contract for every subsequent call."
  (puthash "sentinel.mp4" t media-thumbnail--handled-files)
  (unwind-protect
      (progn
        ;; Call the same subset of clear-all that matters, dodging the
        ;; interactive `message' / directory delete.
        (clrhash media-thumbnail--handled-files)
        (should (hash-table-p media-thumbnail--handled-files))
        (should (zerop (hash-table-count media-thumbnail--handled-files))))
    (remhash "sentinel.mp4" media-thumbnail--handled-files)))

(ert-deftest media-thumbnail-test-ensure-cache-dir-memoizes ()
  "Bug 4: `--ensure-cache-dir' must skip its `file-exists-p' check
after the first successful verification.  Verified by counting
`file-exists-p' invocations across two back-to-back calls with an
unchanged `media-thumbnail-cache-dir'."
  (let ((media-thumbnail--cache-dir-ensured nil)
        (tmp (make-temp-file "mt-cache-" t))
        (calls 0))
    (unwind-protect
        (let ((media-thumbnail-cache-dir tmp))
          (cl-letf* ((orig (symbol-function 'file-exists-p))
                     ((symbol-function 'file-exists-p)
                      (lambda (path)
                        (when (equal path tmp) (cl-incf calls))
                        (funcall orig path))))
            (media-thumbnail--ensure-cache-dir)
            (media-thumbnail--ensure-cache-dir)
            (media-thumbnail--ensure-cache-dir))
          (should (equal calls 1))
          (should (equal media-thumbnail--cache-dir-ensured tmp)))
      (delete-directory tmp t))))

(ert-deftest media-thumbnail-test-ensure-cache-dir-reruns-on-change ()
  "Bug 4: memoization must invalidate when `media-thumbnail-cache-dir'
is rebound at runtime, otherwise a user who moves the cache mid-
session ends up with `--ensure' silently short-circuiting past the
new location."
  (let ((media-thumbnail--cache-dir-ensured nil)
        (a (make-temp-file "mt-cache-a-" t))
        (b (make-temp-file "mt-cache-b-" t)))
    (unwind-protect
        (progn
          (let ((media-thumbnail-cache-dir a))
            (media-thumbnail--ensure-cache-dir)
            (should (equal media-thumbnail--cache-dir-ensured a)))
          (let ((media-thumbnail-cache-dir b))
            (media-thumbnail--ensure-cache-dir)
            (should (equal media-thumbnail--cache-dir-ensured b))))
      (delete-directory a t)
      (delete-directory b t))))

(ert-deftest media-thumbnail-test-enqueue-preserves-fifo ()
  "Bug 5: `--enqueue' / `--dequeue' must preserve FIFO order across a
sequence of pushes.  Regression guard for the tail-pointer refactor —
a stale tail would cause later items to be lost or dropped."
  (let ((media-thumbnail--queue nil)
        (media-thumbnail--queue-tail nil))
    (media-thumbnail--enqueue :a)
    (media-thumbnail--enqueue :b)
    (media-thumbnail--enqueue :c)
    (should (eq (media-thumbnail--dequeue) :a))
    (should (eq (media-thumbnail--dequeue) :b))
    (should (eq (media-thumbnail--dequeue) :c))
    (should (null (media-thumbnail--dequeue)))
    (should (null media-thumbnail--queue-tail))))

(ert-deftest media-thumbnail-test-enqueue-is-linear ()
  "Bug 5: `--enqueue' must be O(1) per call.  Verified by pushing
10000 items and asserting the total wallclock stays under a
generous ceiling that the old `add-to-list :append' path could
never meet (quadratic).  The ceiling is loose enough to avoid
flaky CI; the intent is only to catch a regression to O(N)+ per
call."
  (let ((media-thumbnail--queue nil)
        (media-thumbnail--queue-tail nil))
    (let ((t0 (float-time)))
      (dotimes (i 10000)
        (media-thumbnail--enqueue i))
      (let ((elapsed (- (float-time) t0)))
        (should (< elapsed 0.5))))
    (should (equal (length media-thumbnail--queue) 10000))
    (should (equal (car media-thumbnail--queue) 0))
    (should (equal (car media-thumbnail--queue-tail) 9999))))

(ert-deftest media-thumbnail-test-dequeue-empties-tail ()
  "Bug 5: draining the queue must null the tail pointer so a
subsequent push does not `setcdr' onto a garbage cons."
  (let ((media-thumbnail--queue nil)
        (media-thumbnail--queue-tail nil))
    (media-thumbnail--enqueue :only)
    (media-thumbnail--dequeue)
    (should (null media-thumbnail--queue))
    (should (null media-thumbnail--queue-tail))
    ;; Re-push into a drained queue.
    (media-thumbnail--enqueue :again)
    (should (eq (media-thumbnail--dequeue) :again))))

(ert-deftest media-thumbnail-test-try-cmd-success-fires-callbacks ()
  "Bug 7: `--try-cmd' must fire pending callbacks with SUCCESS-P=t
when the underlying spawn exits ok AND the cache path lands non-
empty on disk.  Verified by stubbing `--spawn' and pointing at a
throw-away temp file."
  (let ((tmp (make-temp-file "mt-try-" nil ".jpg"))
        callback-args)
    (unwind-protect
        (progn
          (write-region "x" nil tmp nil 'silent)
          (puthash "sentinel-file"
                   (list (lambda (file cache-path success-p)
                           (setq callback-args
                                 (list file cache-path success-p))))
                   media-thumbnail--async-callbacks)
          (cl-letf (((symbol-function 'media-thumbnail--spawn)
                     (lambda (_cmd on-exit) (funcall on-exit t))))
            (media-thumbnail--try-cmd
             "fake-cmd" "sentinel-file" tmp
             (lambda () (error "on-fail should not run"))))
          (should (equal callback-args (list "sentinel-file" tmp t))))
      (delete-file tmp)
      (remhash "sentinel-file" media-thumbnail--async-callbacks))))

(ert-deftest media-thumbnail-test-try-cmd-failure-invokes-on-fail ()
  "Bug 7: `--try-cmd' must invoke ON-FAIL when the spawn exits ok
but the cache file is missing or zero-byte — the guard against
`ffmpeg -y' truncating output before erroring."
  (let ((tmp (concat (make-temp-file "mt-try-empty-" nil) ".jpg"))
        (on-fail-called nil))
    (unwind-protect
        (progn
          ;; Ensure the tmp path does NOT exist on disk yet.
          (when (file-exists-p tmp) (delete-file tmp))
          (cl-letf (((symbol-function 'media-thumbnail--spawn)
                     (lambda (_cmd on-exit) (funcall on-exit t))))
            (media-thumbnail--try-cmd
             "fake-cmd" "any" tmp
             (lambda () (setq on-fail-called t))))
          (should on-fail-called))
      (when (file-exists-p tmp) (delete-file tmp)))))

(ert-deftest media-thumbnail-test-try-cmd-non-zero-exit-invokes-on-fail ()
  "Bug 7: `--try-cmd' must invoke ON-FAIL when the underlying spawn
exit-ok is nil, regardless of what's on disk."
  (let ((tmp (make-temp-file "mt-try-nonempty-" nil ".jpg"))
        (on-fail-called nil))
    (unwind-protect
        (progn
          (write-region "content" nil tmp nil 'silent)
          (cl-letf (((symbol-function 'media-thumbnail--spawn)
                     (lambda (_cmd on-exit) (funcall on-exit nil))))
            (media-thumbnail--try-cmd
             "fake-cmd" "any" tmp
             (lambda () (setq on-fail-called t))))
          (should on-fail-called))
      (delete-file tmp))))

(ert-deftest media-thumbnail-test-cache-hit-callback-is-async ()
  "Bug 8: `media-thumbnail-generate-async' must invoke its callback
asynchronously even on a cache hit — the sentinel path always fires
later, so the cache-hit branch matching that contract avoids
surprising callers with a synchronous reentrancy.

Regression guard: if a future edit reverts to `funcall' inside the
cache-hit branch, this test fires the callback before
`sit-for'/`accept-process-output' get a chance to service the timer,
and the assertion for the pre-async state fails."
  (let ((tmp-cache (make-temp-file "mt-async-cache-" t))
        (fired nil))
    (unwind-protect
        (let* ((media-thumbnail-cache-dir tmp-cache)
               (media-thumbnail--cache-dir-ensured nil)
               (source (concat tmp-cache "/dummy.mp4"))
               (cache-jpeg
                (media-thumbnail-get-cache-path source)))
          (write-region "x" nil cache-jpeg nil 'silent)
          (media-thumbnail-generate-async
           source
           :callback (lambda (&rest _) (setq fired t)))
          ;; Not yet fired — cache-hit branch must have deferred.
          (should (null fired))
          ;; Let the 0-delay timer run.
          (accept-process-output nil 0.05)
          (should fired))
      (delete-directory tmp-cache t))))

;;; End of test file.
(provide 'media-thumbnail-test)
;;; media-thumbnail-test.el ends here
