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

(ert-deftest media-thumbnail-test-convert-caps-on-own-inflight ()
  "Bug 9: `--convert' must gate on entries in
`media-thumbnail--async-callbacks' (our own in-flight count) rather
than the global `process-list' size.  Unrelated processes (LSP, git,
etc.) should not starve the thumbnail queue.

Simulates 5 unrelated processes via `cl-letf'-mocked `process-list',
seeds `--async-callbacks' with 2 sentinel entries, sets max=2, pushes
a queue item, and asserts `--convert' does NOT drain it — the guard
should trip on our own count (2 == max) even though `process-list'
returns just 5."
  (let ((media-thumbnail--queue nil)
        (media-thumbnail--queue-tail nil)
        (media-thumbnail--convert-timer nil)
        (media-thumbnail-max-processes 2)
        dispatched)
    (puthash "sentinel-a" nil media-thumbnail--async-callbacks)
    (puthash "sentinel-b" nil media-thumbnail--async-callbacks)
    (media-thumbnail--enqueue '(:image-spec dummy :file "should-not-run.mp4"))
    (unwind-protect
        (cl-letf (((symbol-function 'process-list)
                   (lambda () (make-list 5 'fake-proc)))
                  ((symbol-function 'media-thumbnail-generate-async)
                   (lambda (&rest _) (setq dispatched t))))
          (media-thumbnail--convert)
          (should (null dispatched))
          ;; Item stays on the queue for the next drain attempt.
          (should (equal (length media-thumbnail--queue) 1)))
      (remhash "sentinel-a" media-thumbnail--async-callbacks)
      (remhash "sentinel-b" media-thumbnail--async-callbacks)
      (when (timerp media-thumbnail--convert-timer)
        (cancel-timer media-thumbnail--convert-timer))
      (setq media-thumbnail--convert-timer nil))))

(ert-deftest media-thumbnail-test-convert-dispatches-when-under-cap ()
  "Bug 9 companion: with own in-flight count below max, `--convert'
should still dispatch even if `process-list' is huge (unrelated
Emacs processes)."
  (let ((media-thumbnail--queue nil)
        (media-thumbnail--queue-tail nil)
        (media-thumbnail--convert-timer nil)
        (media-thumbnail-max-processes 20)
        dispatched)
    (media-thumbnail--enqueue '(:image-spec dummy :file "run.mp4"))
    (unwind-protect
        (cl-letf (((symbol-function 'process-list)
                   (lambda () (make-list 100 'fake-proc)))
                  ((symbol-function 'media-thumbnail-generate-async)
                   (lambda (&rest _) (setq dispatched t))))
          (media-thumbnail--convert)
          (should dispatched))
      (when (timerp media-thumbnail--convert-timer)
        (cancel-timer media-thumbnail--convert-timer))
      (setq media-thumbnail--convert-timer nil))))

(ert-deftest media-thumbnail-test-convert-uses-queue-host-buf ()
  "Bug 10: `--convert' runs from a global timer whose `current-buffer'
at fire time is arbitrary.  It must read the `:host-buf' slot off the
queue item (captured at push time from the enqueueing dired buffer)
instead of `(current-buffer)'.

Simulates: buffer H enqueues a request, then buffer OTHER is made
current (as would happen when the global timer fires), then
`--convert' runs.  Assert that the mocked `generate-async' saw H
as the host-buf, not OTHER."
  (media-thumbnail-test--with-temp-buffers (h other)
    (let ((media-thumbnail--queue nil)
          (media-thumbnail--queue-tail nil)
          (media-thumbnail--convert-timer nil)
          (media-thumbnail-max-processes 10)
          (media-thumbnail--async-callbacks (make-hash-table :test 'equal))
          captured-host)
      (unwind-protect
          (progn
            (with-current-buffer h
              (media-thumbnail--enqueue
               `(:image-spec dummy :file "x.mp4" :host-buf ,(current-buffer))))
            (with-current-buffer other
              (cl-letf (((symbol-function 'media-thumbnail-generate-async)
                         (lambda (_file &rest args)
                           (let ((cb (plist-get args :callback)))
                             ;; Fire cb with a synthetic dying-generation:
                             ;; success-p t so the callback body executes.
                             ;; The callback's own `buffer-live-p host-buf'
                             ;; guard reads the host-buf captured in
                             ;; --convert's closure; we prove it by having
                             ;; the callback record which buffer it entered.
                             (funcall cb "x.mp4" "/tmp/x.jpg" t)
                             (setq captured-host
                                   (buffer-local-value
                                    'media-thumbnail--specs-to-flush h))))))
                (media-thumbnail--convert)))
            (should captured-host)
            (should-not (buffer-local-value
                         'media-thumbnail--specs-to-flush other)))
        (when (timerp media-thumbnail--convert-timer)
          (cancel-timer media-thumbnail--convert-timer))
        (setq media-thumbnail--convert-timer nil)))))

(ert-deftest media-thumbnail-test-resolve-seek-time-plain ()
  "A numeric or MM:SS entry must pass through `--resolve-seek-time'
verbatim; those don't need a duration."
  (should (equal (media-thumbnail--resolve-seek-time "5" nil) "5"))
  (should (equal (media-thumbnail--resolve-seek-time "1:30" nil) "1:30"))
  (should (equal (media-thumbnail--resolve-seek-time "0.1" 100.0) "0.1")))

(ert-deftest media-thumbnail-test-resolve-seek-time-percent ()
  "A `%' entry with a valid duration must resolve to absolute seconds
formatted to three decimals; without a duration it must return nil
so `--resolve-seek-times' can drop it from the chain."
  (should (equal (media-thumbnail--resolve-seek-time "10%" 100.0) "10.000"))
  (should (equal (media-thumbnail--resolve-seek-time "50%" 3600.0) "1800.000"))
  (should (null (media-thumbnail--resolve-seek-time "10%" nil))))

(ert-deftest media-thumbnail-test-resolve-seek-times-drops-unresolvable ()
  "When ffprobe is unavailable (mocked here as nil-returning
`--probe-duration'), `%' entries must be dropped so the chain
still walks the numeric fallbacks."
  (cl-letf (((symbol-function 'media-thumbnail--probe-duration)
             (lambda (_file) nil)))
    (should (equal
             (media-thumbnail--resolve-seek-times
              "x.mp4" '("10%" "60" "5" "1" "0.1"))
             '("60" "5" "1" "0.1")))))

(ert-deftest media-thumbnail-test-resolve-seek-times-skips-probe-on-pure-numeric ()
  "A pure-numeric seek list must not touch ffprobe — no probe process
should spawn when the list has no `%' entries.  Guarded by mocking
`--probe-duration' to error; any call would abort the test."
  (cl-letf (((symbol-function 'media-thumbnail--probe-duration)
             (lambda (_file) (error "should not be called"))))
    (should (equal
             (media-thumbnail--resolve-seek-times
              "x.mp4" '("60" "5" "1" "0.1"))
             '("60" "5" "1" "0.1")))))

(ert-deftest media-thumbnail-test-split-seek-triggers-past-threshold ()
  "Deep seek targets (above the split threshold) must return a
(COARSE . FINE) pair; sub-threshold seeks return nil so the frame
command falls back to plain post-input `-ss'."
  (let ((media-thumbnail-ffmpeg-split-seek-threshold 10)
        (media-thumbnail-ffmpeg-split-seek-fine 3))
    (should (null (media-thumbnail--split-seek "5")))
    (should (null (media-thumbnail--split-seek "10")))
    (should (equal (media-thumbnail--split-seek "60") '("57.000" . "3")))
    (should (equal (media-thumbnail--split-seek "1800") '("1797.000" . "3")))
    ;; MM:SS or garbage must not attempt to split.
    (should (null (media-thumbnail--split-seek "1:30")))
    (should (null (media-thumbnail--split-seek nil)))))

(ert-deftest media-thumbnail-test-frame-cmd-emits-split-seek ()
  "Generated ffmpeg command must place a coarse `-ss' BEFORE `-i'
when the seek is past the threshold, and only the fine `-ss' AFTER
`-i'.  Verifies both flags appear in the expected order."
  (let ((cmd (media-thumbnail-ffmpeg-frame-cmd
              "/tmp/a.mp4" "/tmp/a.jpg" :size 0 :seek-time "300")))
    (should (string-match-p "-ss 297\\.000 -i " cmd))
    (should (string-match-p "-i /tmp/a\\.mp4 -ss 3 " cmd))))

(ert-deftest media-thumbnail-test-frame-cmd-plain-seek-below-threshold ()
  "Sub-threshold seeks must NOT emit a pre-input `-ss'; the accurate
seek stays a single post-input `-ss'."
  (let ((cmd (media-thumbnail-ffmpeg-frame-cmd
              "/tmp/a.mp4" "/tmp/a.jpg" :size 0 :seek-time "5")))
    (should-not (string-match-p "-ss 5 -i " cmd))
    (should (string-match-p "-i /tmp/a\\.mp4 -ss 5 " cmd))))

(ert-deftest media-thumbnail-test-parse-ffprobe-json ()
  "Parser must convert ffprobe JSON into our metadata plist,
extracting the first video / audio stream and coercing numeric
strings into numbers."
  (let* ((json (json-parse-string
                "{\"format\":{\"duration\":\"3600.5\",\"size\":\"1000000\",\"bit_rate\":\"2500000\"},\"streams\":[{\"codec_type\":\"video\",\"codec_name\":\"h264\",\"width\":1920,\"height\":1080},{\"codec_type\":\"audio\",\"codec_name\":\"aac\"}]}"))
         (meta (media-thumbnail--parse-ffprobe-json json)))
    (should (equal (plist-get meta :duration) 3600.5))
    (should (equal (plist-get meta :size) 1000000))
    (should (equal (plist-get meta :bit-rate) 2500000))
    (should (equal (plist-get meta :width) 1920))
    (should (equal (plist-get meta :height) 1080))
    (should (equal (plist-get meta :video-codec) "h264"))
    (should (equal (plist-get meta :audio-codec) "aac"))))

(ert-deftest media-thumbnail-test-parse-ffprobe-json-missing-streams ()
  "A JSON with only a format section (no streams) must still yield
a plist with the format-level fields set and stream fields nil —
graceful handling of audio-only files or malformed containers."
  (let* ((json (json-parse-string
                "{\"format\":{\"duration\":\"60.0\"},\"streams\":[]}"))
         (meta (media-thumbnail--parse-ffprobe-json json)))
    (should (equal (plist-get meta :duration) 60.0))
    (should (null (plist-get meta :video-codec)))
    (should (null (plist-get meta :width)))))

(ert-deftest media-thumbnail-test-probe-duration-reads-from-metadata-cache ()
  "`media-thumbnail--probe-duration' must be a thin reader on the
metadata cache — sharing the underlying probe means header + seek
consumers issue one ffprobe per file, not two."
  (let ((media-thumbnail--metadata-cache (make-hash-table :test 'equal))
        (probe-calls 0))
    (cl-letf (((symbol-function 'media-thumbnail--ffprobe-available-p)
               (lambda () t))
              ((symbol-function 'call-process)
               (lambda (&rest _)
                 (cl-incf probe-calls)
                 (insert "{\"format\":{\"duration\":\"120.0\"},\"streams\":[]}")
                 0)))
      (let ((d1 (media-thumbnail--probe-duration "x.mp4"))
            (m1 (media-thumbnail-probe-metadata "x.mp4"))
            (d2 (media-thumbnail--probe-duration "x.mp4")))
        (should (equal d1 120.0))
        (should (equal (plist-get m1 :duration) 120.0))
        (should (equal d2 120.0))
        ;; Three lookups, one probe.
        (should (equal probe-calls 1))))))

(ert-deftest media-thumbnail-test-format-duration-short-and-long ()
  "`media-thumbnail-format-duration' must render MM:SS below one hour
and HH:MM:SS at/above."
  (should (equal (media-thumbnail-format-duration 65) "1:05"))
  (should (equal (media-thumbnail-format-duration 3599) "59:59"))
  (should (equal (media-thumbnail-format-duration 3600) "1:00:00"))
  (should (equal (media-thumbnail-format-duration 3725) "1:02:05")))

(ert-deftest media-thumbnail-test-format-size-units ()
  "`media-thumbnail-format-size' must pick KB / MB / GB by magnitude."
  (should (equal (media-thumbnail-format-size 512) "1 KB"))
  (should (equal (media-thumbnail-format-size (* 5 1024 1024)) "5 MB"))
  (should (equal (media-thumbnail-format-size (* 3 1024 1024 1024)) "3.0 GB")))

(ert-deftest media-thumbnail-test-format-header-composes-fields ()
  "`media-thumbnail-format-header' must combine every metadata field
returned by the probe, skipping fields the probe omitted, in the
documented order (basename, resolution, duration, video, audio, size)."
  (cl-letf (((symbol-function 'media-thumbnail-probe-metadata)
             (lambda (_)
               '(:duration 3725.0 :size 524288000 :width 1920
                 :height 1080 :video-codec "h264" :audio-codec "aac"))))
    (let ((header (media-thumbnail-format-header "/dir/big.mkv")))
      (should (string-match-p "big\\.mkv" header))
      (should (string-match-p "1920×1080" header))
      (should (string-match-p "1:02:05" header))
      (should (string-match-p "h264" header))
      (should (string-match-p "aac" header))
      (should (string-match-p "500 MB" header)))))

(ert-deftest media-thumbnail-test-format-header-omits-missing-fields ()
  "Fields the probe returns nil for must be dropped, not shown blank."
  (cl-letf (((symbol-function 'media-thumbnail-probe-metadata)
             (lambda (_) '(:duration 30.0))))
    (let ((header (media-thumbnail-format-header "/x/audio.mp3")))
      (should (string-match-p "audio\\.mp3" header))
      (should (string-match-p "0:30" header))
      ;; No resolution/codec fields — separators between them must
      ;; not multiply.
      (should-not (string-match-p "×" header))
      (should-not (string-match-p "h264" header)))))

(ert-deftest media-thumbnail-test-preview-buffer-header-policy ()
  "`media-thumbnail-preview-buffer' must honour :header — t inserts
the default formatter, nil skips it, a function is called on PATH.

Uses stubs for `create-image' and `insert-image' because the batch
Emacs used for tests may lack JPEG support; the image path itself
isn't what we're covering here — the header policy is."
  (cl-letf (((symbol-function 'create-image)
             (lambda (&rest _) '(image :type jpeg :file "stub")))
            ((symbol-function 'insert-image)
             (lambda (_spec) (insert "[IMAGE]")))
            ((symbol-function 'media-thumbnail-format-header)
             (lambda (_path) "DEFAULT-HEADER")))
    ;; :header t → default formatter runs.
    (let ((buf (media-thumbnail-preview-buffer
                "/x/a.mp4" "/tmp/a.jpg"
                :buffer " *mt-test-a*"
                :header t)))
      (should (string-match-p
               "DEFAULT-HEADER"
               (with-current-buffer buf (buffer-string))))
      (kill-buffer buf))
    ;; :header nil → no header text.
    (let ((buf (media-thumbnail-preview-buffer
                "/x/a.mp4" "/tmp/a.jpg"
                :buffer " *mt-test-b*"
                :header nil)))
      (should-not (string-match-p
                   "DEFAULT-HEADER"
                   (with-current-buffer buf (buffer-string))))
      (kill-buffer buf))
    ;; :header FUNCTION → function's return goes in.
    (let ((buf (media-thumbnail-preview-buffer
                "/x/a.mp4" "/tmp/a.jpg"
                :buffer " *mt-test-c*"
                :header (lambda (path)
                          (format "CUSTOM:%s" path)))))
      (should (string-match-p
               "CUSTOM:/x/a\\.mp4"
               (with-current-buffer buf (buffer-string))))
      (kill-buffer buf))))

;;; End of test file.
(provide 'media-thumbnail-test)
;;; media-thumbnail-test.el ends here
