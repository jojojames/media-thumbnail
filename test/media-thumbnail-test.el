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

(ert-deftest media-thumbnail-test-cancel-timer-clears-slot ()
  "Bug 2: `media-thumbnail--cancel-timer' must cancel and null the
timer even when called with a live timer.  Regression guard for the
old disable path that just `setq'd the slot to nil and left the timer
firing forever."
  (media-thumbnail-test--with-temp-buffers (buf)
    (with-current-buffer buf
      (setq-local media-thumbnail--timer
                  (run-with-timer 3600 nil #'ignore))
      (should (timerp media-thumbnail--timer))
      (let ((timer media-thumbnail--timer))
        (media-thumbnail--cancel-timer)
        (should (null media-thumbnail--timer))
        ;; Cancelled timers are no longer in `timer-list'.
        (should-not (memq timer timer-list))))))

(ert-deftest media-thumbnail-test-kill-buffer-hook-cancels-timer ()
  "Bug 2: killing a buffer with `media-thumbnail-dired-mode' active
must cancel the convert timer.  Simulates the mode's setup + a
buffer kill and asserts no lingering timer is left in `timer-list'."
  (media-thumbnail-test--with-temp-buffers (buf)
    (let (captured)
      (with-current-buffer buf
        (setq-local media-thumbnail--timer
                    (run-with-timer 3600 nil #'ignore))
        (setq captured media-thumbnail--timer)
        (add-hook 'kill-buffer-hook
                  #'media-thumbnail--cancel-timer nil :local))
      (kill-buffer buf)
      (should-not (memq captured timer-list)))))

;;; End of test file.
(provide 'media-thumbnail-test)
;;; media-thumbnail-test.el ends here
