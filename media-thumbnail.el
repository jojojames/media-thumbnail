;;; media-thumbnail.el --- Utility package to provide media icons -*- lexical-binding: t -*-

;; Copyright (C) 2022 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; URL: https://github.com/jojojames/media-thumbnail
;; Version: 1.0.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: files, tools
;; HomePage: https://github.com/jojojames/media-thumbnail

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; This package provides a utility function that returns thumbnails for
;; various media files.
;; The entry point is `media-thumbnail-for-file'.

;;; Code:
(require 'image)
(require 'cl-lib)
(eval-when-compile (require 'subr-x))
;; `json-parse-buffer' is a C primitive present since Emacs 27 (>= our
;; minimum 28.1 in Package-Requires); no `require' needed.  The
;; declare-function keeps the byte compiler quiet on batch builds that
;; happen to short-circuit the primitive lookup.
(declare-function json-parse-buffer "json.c" (&rest args))

(declare-function dired-hide-details-mode "dired")
(declare-function dired-get-filename "dired")
(declare-function dired-move-to-filename "dired")
(declare-function dired-do-redisplay "dired")
;; `image-flush' and `clear-image-cache' are shipped in image.el (already
;; `require'd at the top of this file), but the byte compiler doesn't
;; always follow the `require' — `declare-function' pins the lookup so
;; the warnings stay quiet.
(declare-function image-flush "image" (spec &optional frame))
(declare-function clear-image-cache "image" (&optional filter animation-cache))

;;
;; (@* "Macros" )
;;

(defmacro media-thumbnail--log (&rest args)
  "Log ARGS only if `media-thumbnail-log' is enabled."
  `(when media-thumbnail-log
     (message ,@args)))

;;
;; (@* "Constants" )
;;

(defconst media-thumbnail-image-exts '("webp" "wmf" "pcx" "xif" "wbmp" "vtf" "tap" "s1j" "sjp" "sjpg" "s1g" "sgi" "sgif" "s1n" "spn" "spng" "xyze" "rgbe" "hdr" "b16" "mdi" "apng" "ico" "pgb" "rlc" "mmr" "fst" "fpx" "fbs" "dxf" "dwg" "djv" "uvvg" "uvg" "uvvi" "uvi" "azv" "psd" "tfx" "t38" "svgz" "svg" "pti" "btf" "btif" "ktx2" "ktx" "jxss" "jxsi" "jxsc" "jxs" "jxrs" "jxra" "jxr" "jxl" "jpf" "jpx" "jpgm" "jpm" "jfif" "jhc" "jph" "jpg2" "jp2" "jls" "hsj2" "hej2" "heifs" "heif" "heics" "heic" "fts" "fit" "fits" "emf" "drle" "cgm" "dib" "bmp" "hif" "avif" "avcs" "avci" "exr" "fax" "icon" "ief" "jpg" "macp" "pbm" "pgm" "pict" "png" "pnm" "ppm" "ras" "rgb" "tga" "tif" "tiff" "xbm" "xpm" "xwd" "jpe" "jpeg"))
(defconst media-thumbnail-video-exts '("ts" "f4v" "rmvb" "wvx" "wmx" "wmv" "wm" "asx" "mk3d" "mkv" "fxm" "flv" "axv" "webm" "viv" "yt" "s1q" "smo" "smov" "ssw" "sswf" "s14" "s11" "smpg" "smk" "bk2" "bik" "nim" "pyv" "m4u" "mxu" "fvt" "dvb" "uvvv" "uvv" "uvvs" "uvs" "uvvp" "uvp" "uvvu" "uvu" "uvvm" "uvm" "uvvh" "uvh" "ogv" "m2v" "m1v" "m4v" "mpg4" "mp4" "mjp2" "mj2" "m4s" "3gpp2" "3g2" "3gpp" "3gp" "avi" "mov" "movie" "mpe" "mpeg" "mpegv" "mpg" "mpv" "qt" "vbs"))

;;
;; (@* "Customizations" )
;;

(defcustom media-thumbnail-log nil
  "Enable debug logging."
  :type 'boolean
  :group 'media-thumbnail)

(defcustom media-thumbnail-size 0
  "The size of the icon when creating a video thumbnail.

0 is the original size of the video."
  :type 'number
  :group 'media-thumbnail)

(defcustom media-thumbnail-cache-dir (expand-file-name "cache/media/" user-emacs-directory)
  "Where thumbnails are located."
  :type 'string
  :group 'media-thumbnail)

(defcustom media-thumbnail-image-width 150
  "Width of images."
  :type 'float
  :group 'media-thumbnail)

(defcustom media-thumbnail-max-processes 20
  "Max number of processes to use when creating thumbnails."
  :type 'int
  :group 'media-thumbnail)

(defcustom media-thumbnail-image-margin 5
  "Padding to add to inserted thumbnails."
  :type 'int
  :group 'media-thumbnail)

(defcustom media-thumbnail-dired-should-hide-details-fn
  #'media-thumbnail-hide-in-some-media-directories
  "Function used to determine whether or not to call `dired-hide-details-mode'."
  :type `(choice
          (const :tag "Default"
                 ,#'media-thumbnail-hide-in-some-media-directories)
          (function :tag "Custom function"))
  :group 'media-thumbnail)

(defcustom media-thumbnail-special-refresh-commands
  '(dired-do-delete
    dired-do-rename
    dired-do-copy
    dired-do-flagged-delete
    dired-create-directory
    (delete-file . 2)
    (save-buffer . 2)
    magit-format-patch)
  "A list of commands that will trigger a refresh of `dired'.

Each entry is either a bare command symbol (refresh immediately) or a
cons (COMMAND . DELAY-SECONDS) — the CDR is the seconds to wait after
COMMAND runs before triggering the refresh.

Set to nil to disable automatic refresh on special commands."
  :type '(repeat (choice (symbol :tag "Command")
                         (cons :tag "Command with delay"
                               (symbol :tag "Command")
                               (number :tag "Delay (seconds)"))))
  :group 'media-thumbnail)

(defcustom media-thumbnail-ignore-aspect-ratio nil
  "If true, ignore aspect ratio when creating thumbnails."
  :type 'boolean
  :group 'media-thumbnail)

(defcustom media-thumbnail-ffmpeg-executable "ffmpeg"
  "Location of the ffmpeg binary."
  :type 'string
  :group 'media-thumbnail)

(defcustom media-thumbnail-ffprobe-executable "ffprobe"
  "Location of the ffprobe binary.

Used to resolve percentage entries in `media-thumbnail-ffmpeg-seek-times'
to absolute seconds via a one-shot duration probe.  When ffprobe is
not on PATH, percentage entries are silently skipped and the chain
falls back to the numeric entries only."
  :type 'string
  :group 'media-thumbnail)

(defcustom media-thumbnail-ffmpeg-seek-times '("10%" "60" "5" "1" "0.1")
  "Seek positions the ffmpeg frame-decode pass will try, in order.

Each entry is either

  \"N\" or \"N.M\" — absolute seconds.
  \"MM:SS\" / \"HH:MM:SS\" — anything `ffmpeg -ss' understands.
  \"P%\" — a percentage of the source duration.  Requires
          `media-thumbnail-ffprobe-executable' on PATH; when it is not,
          percentage entries are silently skipped so the chain still
          works with the numeric fallbacks.

The pipeline runs the frame command with the first (resolved) entry;
on non-zero exit or empty output the next entry is tried; failure is
reported only when the whole list is exhausted.

Defaults cover the common cases: 10% picks a representative frame
from a long video (a movie's studio-logo phase is well past 10%);
60/5/1/0.1 are the fallback ladder for shorter or broken files."
  :type '(repeat string)
  :group 'media-thumbnail)

(defcustom media-thumbnail-ffmpeg-split-seek-threshold 10
  "Seconds threshold beyond which the frame command uses a split seek.

An accurate ffmpeg seek (`-ss' AFTER `-i') decodes from the start of
the file until reaching the target — decode cost scales linearly with
the seek target.  For long videos a 10% seek can be many minutes and
turn into multi-second wall time.

When the target exceeds this threshold, the frame command instead
issues a coarse `-ss (target - fine)' BEFORE `-i' (fast keyframe
seek) plus a fine `-ss (fine)' AFTER `-i' (accurate refinement).
Total decode work is capped at `media-thumbnail-ffmpeg-split-seek-fine'
seconds regardless of how far into the file the target sits.

Values below the threshold, non-numeric strings (e.g. MM:SS), and the
zero-length \"0\" seek skip the split and use plain accurate seek."
  :type 'number
  :group 'media-thumbnail)

(defcustom media-thumbnail-ffmpeg-split-seek-fine 3
  "Seconds of accurate decode retained after the coarse split-seek jump.

Small enough to keep decode cost bounded, large enough to survive
short GOPs — a 3-second window is roughly a keyframe-plus-follow-up on
typical H.264 videos, which is where the frame-decode pass gets a
complete frame with no `frame not finished' error."
  :type 'number
  :group 'media-thumbnail)

(defcustom media-thumbnail-dired-animate-thumbnails t
  "Whether or not `dired' animates thumbnails/gifs."
  :type 'boolean
  :group 'media-thumbnail)

;;
;; (@* "Variables" )
;;

(defvar media-thumbnail--handled-files (make-hash-table :test 'equal)
  "Files already processed by `media-thumbnail'.

Was a plain list; the `member' guard in `media-thumbnail-for-file'
scaled quadratically with the number of unique videos a session had
enumerated (dired refresh on a 500-video directory ran ~125k `equal'
comparisons).  Hash table membership is O(1), so the guard is
now constant-time regardless of how many videos have been touched.")

(defvar media-thumbnail--queue '()
  "FIFO of pending thumbnail requests, drained by `media-thumbnail--convert'.

Paired with `media-thumbnail--queue-tail' so `media-thumbnail--enqueue'
appends in O(1); the old design used `add-to-list :append', which
walked the entire list per enqueue for an O(N^2) dired refresh over
N unique videos.  Dedup used to matter for that `add-to-list' path;
now the `media-thumbnail--handled-files' guard already prevents the
same file from being pushed twice per session.")

(defvar media-thumbnail--queue-tail nil
  "Last cons of `media-thumbnail--queue', or nil when the queue is empty.

Tail pointer for O(1) `media-thumbnail--enqueue' appends.  Kept in
sync with the head by `media-thumbnail--enqueue' /
`media-thumbnail--dequeue'.")

(defvar-local media-thumbnail--redisplay-timer nil
  "Timer used after converting for redisplay.")

(defvar-local media-thumbnail--specs-to-flush nil
  "Image specs to flush to refresh images upon convert finish.")

(defvar media-thumbnail--ffprobe-available 'unchecked
  "Cached ffprobe availability: t, nil, or the sentinel `unchecked'.

Populated by `media-thumbnail--ffprobe-available-p' on first use so
we call `executable-find' at most once per Emacs session.  Reset by
`media-thumbnail-clear-all' so a user who installed ffprobe after
Emacs startup can re-detect it.")

(defvar media-thumbnail--metadata-cache (make-hash-table :test 'equal)
  "FILE → metadata plist memo for the ffprobe path.

Each unique FILE is probed at most once per session; the same
ffprobe invocation feeds the seek-chain duration lookup and any
consumer that wants richer metadata (`media-thumbnail-probe-metadata').
See that function's docstring for the plist keys.")

(defvar media-thumbnail--convert-timer nil
  "Pending one-shot timer for `media-thumbnail--convert', or nil.

Replaces the old buffer-local 0.25 s repeating timer that fired
four times per second per active dired buffer regardless of queue
state.  Now scheduled on demand by `media-thumbnail--enqueue' when
a fresh request lands in an empty queue, and re-scheduled by
`media-thumbnail--convert' when it still has more work to drain.
Cleared to nil the instant the queue empties, so an idle session
consumes no CPU.")

(defvar media-thumbnail--async-callbacks (make-hash-table :test 'equal)
  "Hash table mapping in-flight FILE → list of pending callbacks.

Consulted by `media-thumbnail-generate-async' to coalesce concurrent
requests for the same file into a single ffmpeg pipeline.")

(defvar media-thumbnail--cache-dir-ensured nil
  "Cache-dir path we last verified exists on disk, or nil.

Used by `media-thumbnail--ensure-cache-dir' to skip the `file-exists-p'
syscall on every generate-async call once we've already created the
directory this session — an N=500-video dired refresh previously did
500+ redundant `file-exists-p' calls in a hot loop.  Invalidated by
`media-thumbnail-clear-all' when the directory is deleted, and by any
runtime change to `media-thumbnail-cache-dir' (compared by value).")

;;
;; (@* "Implementation" )
;;

(defun media-thumbnail-get-cache-path (file)
  "Return the cached image path for FILE."
  (format "%s%s.jpg" (expand-file-name media-thumbnail-cache-dir)
          (file-name-base file)))

(defun media-thumbnail--schedule-convert ()
  "Ensure `media-thumbnail--convert' is scheduled to run soon.

Only queues a one-shot timer when none is currently pending, so a
burst of enqueues coalesces into a single scheduled drain.  A no-op
when `media-thumbnail--convert-timer' already points at a live
timer."
  (unless (timerp media-thumbnail--convert-timer)
    (setq media-thumbnail--convert-timer
          (run-with-timer 0.25 nil #'media-thumbnail--convert))))

(defun media-thumbnail--enqueue (item)
  "Append ITEM to `media-thumbnail--queue' in O(1).

Threads through `media-thumbnail--queue-tail' so a full dired
refresh over N videos runs in O(N) instead of the O(N^2) that
`add-to-list :append' produced when it walked the whole list on
every push.  Also schedules the drain timer if none is pending,
so the queue makes progress without a background poller."
  (let ((cell (cons item nil)))
    (if media-thumbnail--queue-tail
        (setcdr media-thumbnail--queue-tail cell)
      (setq media-thumbnail--queue cell))
    (setq media-thumbnail--queue-tail cell))
  (media-thumbnail--schedule-convert))

(defun media-thumbnail--dequeue ()
  "Pop the head of `media-thumbnail--queue' and return it, or nil.

Kept next to `media-thumbnail--enqueue' so tail-pointer maintenance
stays in one place."
  (when media-thumbnail--queue
    (let ((head (car media-thumbnail--queue)))
      (setq media-thumbnail--queue (cdr media-thumbnail--queue))
      (unless media-thumbnail--queue
        (setq media-thumbnail--queue-tail nil))
      head)))

(defun media-thumbnail--ensure-cache-dir ()
  "Ensure `media-thumbnail-cache-dir' exists on disk.

Memoized via `media-thumbnail--cache-dir-ensured' so repeated calls
in a hot loop (dired refresh, batch enqueues) skip the syscall once
the directory has been verified this session.  Handles a runtime
change to `media-thumbnail-cache-dir' by re-checking when the current
value differs from the cached one."
  (unless (equal media-thumbnail--cache-dir-ensured
                 media-thumbnail-cache-dir)
    (unless (file-exists-p media-thumbnail-cache-dir)
      (make-directory media-thumbnail-cache-dir t))
    (setq media-thumbnail--cache-dir-ensured
          media-thumbnail-cache-dir)))

;;;###autoload
(defun media-thumbnail-for-file (file)
  "Return image spec for FILE."
  (cond
   ((or (string-match-p "^\\._" (file-name-base file))
        (not (file-name-extension file)))
    nil)
   ((member (downcase (file-name-extension file))
            media-thumbnail-image-exts)
    (media-thumbnail--create-image file))
   ((equal (downcase (file-name-extension file)) "gif")
    (media-thumbnail--create-image file))
   ((member (downcase (file-name-extension file))
            media-thumbnail-video-exts)
    (let ((cache-path (media-thumbnail-get-cache-path file)))
      (if (or (gethash file media-thumbnail--handled-files)
              (file-exists-p cache-path))
          (media-thumbnail--create-image cache-path)
        (puthash file t media-thumbnail--handled-files)
        (let ((image-spec (media-thumbnail--create-image cache-path)))
          ;; Command is not stored on the queue — dired shares the
          ;; same ffmpeg pipeline as the single-shot async path so the
          ;; drain step is a plain call into
          ;; `media-thumbnail-generate-async'.  `:host-buf' pins the
          ;; enqueueing dired buffer so `media-thumbnail--convert' can
          ;; route the eventual redisplay into the right place; the
          ;; drain runs from a global timer whose `current-buffer' at
          ;; fire time is whatever the user has focused, not the
          ;; enqueueing buffer.
          (media-thumbnail--enqueue
           `(:image-spec ,image-spec :file ,file :host-buf ,(current-buffer)))
          image-spec))))
   (t nil)))

(defun media-thumbnail--create-image (filename)
  "Helper method to create and return an image given FILENAME."
  (if (equal (file-name-extension filename) "png")
      (create-image filename 'png nil
                    :width media-thumbnail-image-width
                    :margin media-thumbnail-image-margin
                    :ascent 'center)
    (create-image filename 'jpeg nil
                  :width media-thumbnail-image-width
                  :margin media-thumbnail-image-margin
                  :ascent 'center)))

(defun media-thumbnail--convert ()
  "Pop and dispatch one task from `media-thumbnail--queue'.

Routes through `media-thumbnail-generate-async' — dired uses the same
2-step ffmpeg pipeline (poster then frame chain) as the single-shot
async path.  Concurrent processes are throttled via
`media-thumbnail-max-processes' so a dired buffer with hundreds of
videos doesn't spawn a subprocess storm.

Runs as the single-shot callback of
`media-thumbnail--convert-timer' — clears the pending-timer slot at
entry so `media-thumbnail--enqueue' can schedule the next tick, and
re-arms via `media-thumbnail--schedule-convert' if the queue still
holds items.  When the queue is empty on entry, the function exits
without rescheduling, so an idle session leaves no live timer."
  (setq media-thumbnail--convert-timer nil)
  (when (and media-thumbnail--queue
             ;; Cap in-flight against our own generations, not the
             ;; global `process-list' — an LSP-heavy user might have
             ;; 30+ language-server processes running and never see
             ;; the thumbnail queue drain otherwise.  Each entry in
             ;; `--async-callbacks' corresponds to one in-flight
             ;; ffmpeg pipeline (added by dispatch, removed by
             ;; `--fire-callbacks').
             (< (hash-table-count media-thumbnail--async-callbacks)
                media-thumbnail-max-processes))
    (pcase-let* ((convert-request (media-thumbnail--dequeue))
                 (`(:image-spec ,image-spec :file ,file :host-buf ,host-buf)
                  convert-request))
      (media-thumbnail--log "Dispatching: %S %s" image-spec file)
      (media-thumbnail-generate-async
       file
       :callback
       (lambda (_file _cache-path success-p)
         (when success-p
           (when (buffer-live-p host-buf)
             (with-current-buffer host-buf
               (push convert-request media-thumbnail--specs-to-flush)
               (unless media-thumbnail--redisplay-timer
                 (media-thumbnail--log "Setting up redisplay!")
                 (setq-local
                  media-thumbnail--redisplay-timer
                  (run-with-timer
                   0.1 nil
                   (lambda ()
                     (when (buffer-live-p host-buf)
                       (with-current-buffer host-buf
                         (media-thumbnail--redisplay)))))))))))))
    (when media-thumbnail--queue
      (media-thumbnail--schedule-convert))))

(defun media-thumbnail--fire-callbacks (file cache-path success-p)
  "Fire every pending callback registered for FILE, then clear the entry."
  (let ((callbacks (gethash file media-thumbnail--async-callbacks)))
    (remhash file media-thumbnail--async-callbacks)
    (dolist (cb callbacks)
      (ignore-errors (funcall cb file cache-path success-p)))))

(defun media-thumbnail--spawn (cmd on-exit)
  "Run shell CMD asynchronously; call ON-EXIT with SUCCESS-P once it exits.

SUCCESS-P is t only when the process exited normally with status 0.
Callers layer their own file-existence / non-empty checks on top."
  (media-thumbnail--log "Async: %s" cmd)
  (let ((proc (start-process-shell-command
               (format "media-thumbnail %s" (substring cmd 0 (min 40 (length cmd))))
               nil cmd)))
    (set-process-sentinel
     proc
     (lambda (proc _event)
       (when (memq (process-status proc) '(exit signal))
         (funcall on-exit
                  (and (eq (process-status proc) 'exit)
                       (zerop (process-exit-status proc)))))))))

(defun media-thumbnail--file-non-empty-p (path)
  "Return non-nil if PATH exists on disk with non-zero size.

Guards against `ffmpeg -y' opening the output file and truncating it
before failing — a zero-byte artifact should count as failure, not
success, so the frame-fallback still runs and overwrites it."
  (when-let* ((attrs (file-attributes path)))
    (> (file-attribute-size attrs) 0)))

(defun media-thumbnail--ffprobe-available-p ()
  "Return non-nil if `media-thumbnail-ffprobe-executable' is on PATH.

Result is cached in `media-thumbnail--ffprobe-available' for the rest
of the session; `media-thumbnail-clear-all' resets the cache so a
freshly-installed ffprobe is picked up without a restart."
  (when (eq media-thumbnail--ffprobe-available 'unchecked)
    (setq media-thumbnail--ffprobe-available
          (and (executable-find media-thumbnail-ffprobe-executable) t)))
  media-thumbnail--ffprobe-available)

(defun media-thumbnail--parse-ffprobe-json (json)
  "Convert a parsed ffprobe JSON object JSON to our metadata plist.

Extracted so tests can drive the shape without spawning ffprobe.
Returns nil when JSON has no `format' section (malformed output).
Unknown keys collapse to nil; numeric strings are coerced to
numbers; the first video and audio streams win when the file has
multiples."
  (let* ((format (gethash "format" json))
         (streams (append (gethash "streams" json) nil))
         (video (cl-find-if (lambda (s)
                              (equal (gethash "codec_type" s) "video"))
                            streams))
         (audio (cl-find-if (lambda (s)
                              (equal (gethash "codec_type" s) "audio"))
                            streams))
         (num (lambda (s) (and (stringp s)
                               (string-match-p "\\`[0-9.]+\\'" s)
                               (string-to-number s)))))
    (when format
      (list :duration    (funcall num (gethash "duration" format))
            :size        (funcall num (gethash "size" format))
            :bit-rate    (funcall num (gethash "bit_rate" format))
            :width       (and video (gethash "width" video))
            :height      (and video (gethash "height" video))
            :video-codec (and video (gethash "codec_name" video))
            :audio-codec (and audio (gethash "codec_name" audio))))))

(defun media-thumbnail-probe-metadata (file)
  "Return a metadata plist for FILE via `ffprobe', memoized.

Runs one `ffprobe' subprocess per unique FILE per Emacs session;
the result is cached in `media-thumbnail--metadata-cache' so
consumers (seek-chain resolver, preview headers, etc.) share the
same probe.  Returns nil when ffprobe is unavailable or its output
does not parse.

Plist keys (all may be nil when ffprobe omits them):

  :duration     Float seconds.
  :size         Bytes on disk.
  :bit-rate     Bits per second, container-level.
  :width        Video stream pixel width.
  :height       Video stream pixel height.
  :video-codec  Codec name of the first video stream, e.g. \"h264\".
  :audio-codec  Codec name of the first audio stream, e.g. \"aac\"."
  (when (media-thumbnail--ffprobe-available-p)
    (let ((cached (gethash file media-thumbnail--metadata-cache 'miss)))
      (if (not (eq cached 'miss))
          cached
        (let ((meta
               (with-temp-buffer
                 (when (zerop
                        (call-process
                         media-thumbnail-ffprobe-executable nil t nil
                         "-v" "error"
                         "-show_entries"
                         "format=duration,size,bit_rate:stream=codec_name,codec_type,width,height"
                         "-of" "json" file))
                   (goto-char (point-min))
                   (ignore-errors
                     (media-thumbnail--parse-ffprobe-json
                      (json-parse-buffer)))))))
          (puthash file meta media-thumbnail--metadata-cache)
          meta)))))

(defun media-thumbnail--probe-duration (file)
  "Return FILE's duration in seconds via `media-thumbnail-probe-metadata'.

Thin reader on the shared metadata cache; sharing means the
seek-chain resolver and header consumers issue a single ffprobe
per file rather than one each."
  (plist-get (media-thumbnail-probe-metadata file) :duration))

;;
;; (@* "Preview buffer helpers" )
;;

(defun media-thumbnail-format-duration (seconds)
  "Format SECONDS as MM:SS or HH:MM:SS depending on length."
  (let* ((s (round seconds))
         (h (/ s 3600))
         (m (/ (mod s 3600) 60))
         (sec (mod s 60)))
    (if (zerop h)
        (format "%d:%02d" m sec)
      (format "%d:%02d:%02d" h m sec))))

(defun media-thumbnail-format-size (bytes)
  "Format BYTES as a short human-readable size (KB / MB / GB)."
  (cond
   ((>= bytes (* 1024 1024 1024))
    (format "%.1f GB" (/ bytes 1024.0 1024.0 1024.0)))
   ((>= bytes (* 1024 1024))
    (format "%.0f MB" (/ bytes 1024.0 1024.0)))
   (t
    (format "%d KB" (max 1 (/ bytes 1024))))))

(defcustom media-thumbnail-preview-buffer-name " *media-thumbnail-preview*"
  "Name of the shared buffer used by `media-thumbnail-preview-buffer'.

Prefixed with a space so the buffer stays out of the default
`buffer-menu' / vertico `switch-to-buffer' lists — the buffer is a
short-lived reusable render target, not something the user opens by
name."
  :type 'string
  :group 'media-thumbnail)

(defun media-thumbnail-format-header (path)
  "Return a one-line metadata header string for PATH.

Reads `media-thumbnail-probe-metadata' (shared cache — no extra
ffprobe per call).  Degrades gracefully: only fields ffprobe
returned appear in the header; the basename is always present even
when the probe fails.  Separator style: middle dot in `shadow'
face; basename in `bold'.  Rendered format:

  basename  ·  1920×1080  ·  1:02:06  ·  h264  ·  aac  ·  500 MB"
  (let* ((basename (file-name-nondirectory path))
         (meta (media-thumbnail-probe-metadata path))
         (width (plist-get meta :width))
         (height (plist-get meta :height))
         (duration (plist-get meta :duration))
         (size (plist-get meta :size))
         (video-codec (plist-get meta :video-codec))
         (audio-codec (plist-get meta :audio-codec))
         (parts
          (delq nil
                (list
                 (propertize basename 'face 'bold)
                 (and width height (format "%d×%d" width height))
                 (and duration (media-thumbnail-format-duration duration))
                 video-codec
                 audio-codec
                 (and size (media-thumbnail-format-size size))))))
    (mapconcat #'identity parts
               (propertize "  ·  " 'face 'shadow))))

(cl-defun media-thumbnail-preview-buffer (path cache-path
                                               &key
                                               buffer
                                               max-width
                                               max-height
                                               (header t))
  "Return a buffer displaying CACHE-PATH's image with an optional header.

PATH is the source media file — used to build the metadata header.
CACHE-PATH is the JPEG produced by `media-thumbnail-generate-async'.

Keyword args:

  :buffer      Buffer or name to reuse.  Defaults to
               `media-thumbnail-preview-buffer-name' — a shared
               buffer, so successive calls swap content in place
               instead of accumulating dead buffers.
  :max-width   Passed through to `create-image :max-width'.  Nil
               skips the constraint (image renders at natural
               size).  Set to the display pane's pixel width when
               known so Emacs downscales oversized JPEGs to fit.
  :max-height  Same, for `:max-height'.
  :header      Header policy.
                 t         — prepend `media-thumbnail-format-header'.
                 nil       — image only.
                 function  — call with PATH, insert returned string.
               Default t."
  (let* ((buf (get-buffer-create
               (or buffer media-thumbnail-preview-buffer-name)))
         (spec (apply #'create-image cache-path 'jpeg nil
                      (append (when max-width  (list :max-width max-width))
                              (when max-height (list :max-height max-height))
                              (list :ascent 'center))))
         (inhibit-read-only t))
    (with-current-buffer buf
      (erase-buffer)
      (pcase header
        ('t (insert (media-thumbnail-format-header path) "\n\n"))
        ((pred functionp) (insert (funcall header path) "\n\n"))
        (_ nil))
      (insert-image spec)
      (goto-char (point-min))
      (setq-local buffer-read-only t))
    buf))

(defun media-thumbnail--resolve-seek-time (entry duration)
  "Return a plain-seconds string for ENTRY, or nil if it cannot resolve.

ENTRY is one of the strings from `media-thumbnail-ffmpeg-seek-times'.
A plain time literal (`\"5\"', `\"1:30\"') passes through unchanged.
A percentage entry (`\"10%\"') is multiplied by DURATION (float
seconds) and formatted to three decimal places.  Returns nil when the
entry needs a duration we don't have — the caller drops nils from the
chain, so the chain still runs against whatever numeric fallbacks
remain."
  (cond
   ((not (stringp entry)) nil)
   ((string-match "\\`\\([0-9.]+\\)%\\'" entry)
    (when duration
      (format "%.3f"
              (* duration (/ (string-to-number (match-string 1 entry))
                             100.0)))))
   (t entry)))

(defun media-thumbnail--resolve-seek-times (file entries)
  "Return ENTRIES with each `%' entry resolved against FILE's duration.

Nil entries — those that needed ffprobe but couldn't get a duration —
are dropped.  A duration probe is issued only when ENTRIES actually
contains a percentage; a pure-numeric list never touches ffprobe."
  (let* ((needs-duration
          (cl-some (lambda (e)
                     (and (stringp e) (string-match-p "%" e)))
                   entries))
         (duration (and needs-duration
                        (media-thumbnail--probe-duration file))))
    (delq nil
          (mapcar (lambda (e)
                    (media-thumbnail--resolve-seek-time e duration))
                  entries))))

(cl-defun media-thumbnail-ffmpeg-poster-cmd (file cache-path)
  "Return an ffmpeg shell command that extracts FILE's embedded cover art.

Writes to CACHE-PATH as JPEG.  Exits non-zero when the source has no
attached-picture stream, so the caller can fall through to a
frame-decode pass without inspecting stderr."
  (mapconcat
   #'identity
   `(,media-thumbnail-ffmpeg-executable
     "-nostdin" "-y" "-loglevel" "error"
     "-i" ,(shell-quote-argument file)
     "-map" "0:v:m:disposition:attached_pic"
     "-frames:v" "1"
     "-q:v" "2"
     ,(shell-quote-argument cache-path))
   " "))

(defun media-thumbnail--split-seek (seek)
  "Return (COARSE . FINE) split for absolute SEEK string, or nil.

Only applies to a plain numeric string above
`media-thumbnail-ffmpeg-split-seek-threshold'.  MM:SS / HH:MM:SS
strings and values at or below the threshold return nil — the caller
falls back to a single post-input `-ss'.  COARSE is the pre-input
fast seek target (in seconds, three decimals); FINE is the residual
post-input accurate seek retained after the coarse jump."
  (when (and (stringp seek)
             (string-match "\\`\\([0-9.]+\\)\\'" seek))
    (let ((seek-num (string-to-number seek))
          (fine media-thumbnail-ffmpeg-split-seek-fine))
      (when (> seek-num media-thumbnail-ffmpeg-split-seek-threshold)
        (cons (format "%.3f" (- seek-num fine))
              (number-to-string fine))))))

(cl-defun media-thumbnail-ffmpeg-frame-cmd (file cache-path &key size ignore-aspect-ratio seek-time)
  "Return an ffmpeg shell command that decodes a single frame from FILE.

Places `-ss' AFTER `-i' by default so ffmpeg does an accurate seek —
decode from start until the timestamp — instead of the input-side
fast seek that lands on the nearest prior keyframe.  Fast seek is
faster but produces `frame not finished' errors on files with sparse
keyframes.

For deep seeks (target seconds above
`media-thumbnail-ffmpeg-split-seek-threshold') the command instead
splits the seek: a coarse `-ss' BEFORE `-i' that fast-seeks close to
the target, plus a small `-ss (fine)' AFTER `-i' for accurate
refinement.  Decode work is then bounded by
`media-thumbnail-ffmpeg-split-seek-fine' seconds regardless of how
far into the file the target sits — 50% of a 2h movie now costs the
same as 10%.

Writes a JPEG to CACHE-PATH.  SIZE overrides `media-thumbnail-size';
IGNORE-ASPECT-RATIO overrides `media-thumbnail-ignore-aspect-ratio'.
A SIZE of 0 skips scaling.  SEEK-TIME defaults to the first entry of
`media-thumbnail-ffmpeg-seek-times' — supply an override when driving
the retry chain."
  (let* ((size (or size media-thumbnail-size))
         (ignore-aspect-ratio (if ignore-aspect-ratio
                                  ignore-aspect-ratio
                                media-thumbnail-ignore-aspect-ratio))
         (seek (or seek-time
                   (car media-thumbnail-ffmpeg-seek-times)
                   "5"))
         (split (media-thumbnail--split-seek seek))
         (scale (unless (zerop size)
                  (if ignore-aspect-ratio
                      (format "scale=%d:%d" size size)
                    (format "scale=%d:%d:force_original_aspect_ratio=decrease"
                            size size)))))
    (mapconcat
     #'identity
     `(,media-thumbnail-ffmpeg-executable
       "-nostdin" "-y" "-loglevel" "error"
       ,@(when split (list "-ss" (car split)))
       "-i" ,(shell-quote-argument file)
       "-ss" ,(if split (cdr split) seek)
       "-frames:v" "1"
       "-q:v" "2"
       ,@(when scale (list "-vf" (shell-quote-argument scale)))
       ,(shell-quote-argument cache-path))
     " ")))

(defun media-thumbnail--try-cmd (cmd file cache-path on-fail)
  "Spawn CMD; on success fire pending callbacks, on failure call ON-FAIL.

Success is defined the same way across every stage of the pipeline:
`media-thumbnail--spawn' exit-ok AND CACHE-PATH exists non-empty on
disk.  Extracted here so `media-thumbnail--generate' and
`media-thumbnail--ffmpeg-frame-chain' don't repeat the pattern, and
so that any future guard on \"was the output actually written\"
lands in one place."
  (media-thumbnail--spawn
   cmd
   (lambda (exit-ok)
     (if (and exit-ok (media-thumbnail--file-non-empty-p cache-path))
         (media-thumbnail--fire-callbacks file cache-path t)
       (funcall on-fail)))))

(defun media-thumbnail--ffmpeg-frame-chain (file cache-path size ignore-aspect-ratio seek-times)
  "Try to decode a frame from FILE, walking SEEK-TIMES until one succeeds.

Each attempt spawns `ffmpeg' with the head of SEEK-TIMES as `-ss'.
Non-zero exit or an empty output file advances to the next entry;
first successful attempt fires the pending callbacks with SUCCESS-P
= t.  Exhausted list without success fires SUCCESS-P = nil."
  (cond
   ((null seek-times)
    (media-thumbnail--fire-callbacks file cache-path nil))
   (t
    (media-thumbnail--try-cmd
     (media-thumbnail-ffmpeg-frame-cmd
      file cache-path
      :size size
      :ignore-aspect-ratio ignore-aspect-ratio
      :seek-time (car seek-times))
     file cache-path
     (lambda ()
       (media-thumbnail--ffmpeg-frame-chain
        file cache-path size ignore-aspect-ratio (cdr seek-times)))))))

(defun media-thumbnail--generate (file cache-path size ignore-aspect-ratio)
  "Two-step ffmpeg pipeline: embedded poster, then decoded-frame chain.

Attempts to extract an attached-picture stream first — quick when it
succeeds and generally the best-quality result for files that carry a
poster (music videos, movies with cover art).  When the poster attempt
exits non-zero or produces an empty JPEG (source has no
attached-picture stream, or ffmpeg failed to write it), falls through
to `media-thumbnail--ffmpeg-frame-chain'.

The frame-chain input is `media-thumbnail-ffmpeg-seek-times' with any
`%' entries pre-resolved against FILE's duration via ffprobe.
Percentage entries that cannot be resolved (ffprobe absent, or probe
failed for this file) are silently dropped, so the chain still runs
whatever numeric entries remain."
  (media-thumbnail--try-cmd
   (media-thumbnail-ffmpeg-poster-cmd file cache-path)
   file cache-path
   (lambda ()
     (media-thumbnail--ffmpeg-frame-chain
      file cache-path size ignore-aspect-ratio
      (media-thumbnail--resolve-seek-times
       file media-thumbnail-ffmpeg-seek-times)))))

;;;###autoload
(cl-defun media-thumbnail-generate-async (file &key size ignore-aspect-ratio callback)
  "Generate a thumbnail for FILE asynchronously.

Returns the cache path where the JPEG will land.

SIZE and IGNORE-ASPECT-RATIO override `media-thumbnail-size' and
`media-thumbnail-ignore-aspect-ratio' for this call, letting callers
size the thumbnail to fit their preview surface without mutating
package-wide defcustoms.

CALLBACK, if non-nil, is invoked with (FILE CACHE-PATH SUCCESS-P) once
generation completes.  The invocation is always asynchronous — even
on a cache hit CALLBACK fires from a `run-at-time' one-shot, not
inside the caller's stack frame.  This matches the sentinel path's
semantics so callers can rely on \"my callback fires later\"
regardless of whether the JPEG was on disk already.

Concurrent requests for the same FILE are coalesced through
`media-thumbnail--async-callbacks': subsequent calls append their
callback to the pending list and share the in-flight pipeline.
Callers do not need their own dedup / in-flight bookkeeping."
  (media-thumbnail--ensure-cache-dir)
  (let ((cache-path (media-thumbnail-get-cache-path file)))
    (cond
     ((file-exists-p cache-path)
      (when callback
        (run-at-time 0 nil callback file cache-path t))
      cache-path)
     ((gethash file media-thumbnail--async-callbacks)
      (when callback
        (puthash file
                 (append (gethash file media-thumbnail--async-callbacks)
                         (list callback))
                 media-thumbnail--async-callbacks))
      cache-path)
     (t
      (puthash file (if callback (list callback) nil)
               media-thumbnail--async-callbacks)
      (media-thumbnail--generate
       file cache-path size ignore-aspect-ratio)
      cache-path))))

(defun media-thumbnail--redisplay (&rest _)
  "Call `redisplay' and reset `media-thumbnail--redisplay-timer'."
  (media-thumbnail--log "Calling redisplay!")
  (while media-thumbnail--specs-to-flush
    (pcase-let* ((`(:image-spec ,image-spec :file ,file . ,_)
                  (car media-thumbnail--specs-to-flush)))
      (media-thumbnail--log
       "-----\nFlushing: %S\nFile: %s\n-----" image-spec file)
      (image-flush image-spec))
    (setq-local media-thumbnail--specs-to-flush
                (cdr media-thumbnail--specs-to-flush)))
  ;; Might have to loop through the modes instead?
  ;; Otherwise, this doesn't update if we're not on the buffer.
  (when (derived-mode-p 'dired-mode)
    ;; When deleting/moving at the end of the buffer, `dired-do-redisplay'
    ;; doesn't clean up the extraneous thumbnail.
    (if (eobp)
        (save-mark-and-excursion
          (forward-line -1)
          (dired-do-redisplay))
      (dired-do-redisplay)))
  (setq-local media-thumbnail--redisplay-timer nil))

;;
;; (@* "User Facing" )
;;

;;;###autoload
(defun media-thumbnail-clear-all ()
  "Reset everything related to `media-thumbnail'."
  (interactive)
  (message "Clearing `media-thumbnail'...")
  (clear-image-cache)
  (setq media-thumbnail--specs-to-flush nil)
  (setq media-thumbnail--redisplay-timer nil)
  (setq media-thumbnail--queue nil)
  (setq media-thumbnail--queue-tail nil)
  (when (timerp media-thumbnail--convert-timer)
    (cancel-timer media-thumbnail--convert-timer))
  (setq media-thumbnail--convert-timer nil)
  (clrhash media-thumbnail--handled-files)
  (clrhash media-thumbnail--metadata-cache)
  (setq media-thumbnail--ffprobe-available 'unchecked)
  (setq media-thumbnail--cache-dir-ensured nil)
  (when (file-exists-p media-thumbnail-cache-dir)
    (message "Deleting %s directory." media-thumbnail-cache-dir)
    (delete-directory media-thumbnail-cache-dir t t)))

(defun media-thumbnail-hide-in-some-media-directories ()
  "Determine if we're looking at a media directory."
  (when default-directory
    (let ((f (downcase (file-name-directory default-directory))))
      (and
       (or
        (string-match-p "videos" f)
        (string-match-p "pictures" f)
        (string-match-p "photos" f))
       'media-thumbnail--directory-has-media))))

(defun media-thumbnail--directory-has-media (directory)
  "Return if current DIRECTORY has media files.

If DIRECTORY is nil, use `default-directory'."
  (cl-find-if (lambda (x)
                (when-let ((ext (file-name-extension x)))
                  (or
                   (member (downcase ext) media-thumbnail-video-exts)
                   (member (downcase ext) media-thumbnail-image-exts))))
              (directory-files (or directory default-directory))))

;;
;; (@* "Dired" )
;;

(defun media-thumbnail-dired--display ()
  "Display the icons of files in a Dired buffer."
  (interactive)
  (remove-images (point-min) (point-max))
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (when (dired-move-to-filename nil)
          (dired-move-to-filename)
          (unless (member (dired-get-filename 'verbatim t) '("." ".."))
            (let ((filename (dired-get-filename nil t)))
              (when-let ((image (media-thumbnail-for-file filename)))
                (put-image image (point))
                (when (and media-thumbnail-dired-animate-thumbnails
                           (equal (file-name-extension filename) "gif"))
                  (image-animate image 0 t))))))
        (forward-line 1)))))

(define-minor-mode media-thumbnail-dired-mode
  "Toggle `media-thumbnail-dired-mode'."
  :lighter " Dired-Thumbnails"
  (if media-thumbnail-dired-mode
      (progn
        (when (funcall media-thumbnail-dired-should-hide-details-fn)
          (dired-hide-details-mode +1))
        (add-hook 'dired-after-readin-hook
                  #'media-thumbnail-dired--display :append :local)
        (mapc
         (lambda (x)
           (if (consp x)
               (let ((command (car x))
                     (delay (cdr x)))
                 (advice-add
                  command
                  :after
                  (defalias (intern
                             (format "media-thumbnail-refresh-after-%S"
                                     command))
                    (function
                     (lambda (&rest _)
                       (let ((timer-symbol
                              (intern
                               (format
                                "media-thumbnail-refresh-%S-timer" command))))
                         (when (and (boundp timer-symbol)
                                    (timerp (symbol-value timer-symbol)))
                           (cancel-timer (symbol-value timer-symbol)))
                         (setf
                          (symbol-value timer-symbol)
                          (run-with-idle-timer
                           delay
                           nil
                           #'media-thumbnail--redisplay))))))))
             (advice-add x :after #'media-thumbnail--redisplay)))
         media-thumbnail-special-refresh-commands)
        (media-thumbnail-dired--display))
    (when (funcall media-thumbnail-dired-should-hide-details-fn)
      (dired-hide-details-mode -1))
    (remove-hook 'dired-after-readin-hook
                 #'media-thumbnail-dired--display :local)
    (mapc
     (lambda (x)
       (if (consp x)
           (let ((command (car x)))
             (advice-remove
              command
              (intern
               (format "media-thumbnail-refresh-after-%S"
                       command))))
         (advice-remove x 'media-thumbnail--redisplay)))
     media-thumbnail-special-refresh-commands)
    (remove-images (point-min) (point-max))))

(provide 'media-thumbnail)
;;; media-thumbnail.el ends here
