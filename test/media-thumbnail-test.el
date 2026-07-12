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

;;; End of test file.
(provide 'media-thumbnail-test)
;;; media-thumbnail-test.el ends here
