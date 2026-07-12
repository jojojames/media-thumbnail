;;; media-thumbnail.el --- Utility package to provide media icons -*- lexical-binding: t -*-

;; Copyright (C) 2022 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; URL: https://github.com/jojojames/media-thumbnail
;; Version: 0.0.1
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

(declare-function dired-hide-details-mode "dired")
(declare-function dired-get-filename "dired")
(declare-function dired-move-to-filename "dired")
(declare-function dired-do-redisplay "dired")

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

(defcustom media-thumbnail-cache-dir (format "%scache/" user-emacs-directory)
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

The command can be an alist with the CDR of the alist being the amount of time
to wait to refresh the sidebar after the CAR of the alist is called.

Set this to nil or set `media-thumbnail-special-refresh-commands' to nil
to disable automatic refresh when a special command is triggered."
  :type 'list
  :group 'media-thumbnail)

(defcustom media-thumbnail-ignore-aspect-ratio nil
  "If true, ignore aspect ratio when creating thumbnails."
  :type 'boolean
  :group 'media-thumbnail)

(defcustom media-thumbnail-ffmpeg-executable "ffmpeg"
  "Location of the ffmpeg binary."
  :type 'string
  :group 'media-thumbnail)

(defcustom media-thumbnail-ffmpeg-seek-times '("5" "1" "0.1")
  "Seek positions the ffmpeg frame-decode pass will try, in order.

Each entry is passed to `ffmpeg -ss' in turn: the frame command runs
with the first entry; on non-zero exit or empty output the next entry
is tried; the pipeline reports failure only when the whole list is
exhausted.  Handles the common failure modes: 5s covers most files;
1s covers short clips; 0.1s covers extremely short clips and files
whose only decodable frame is right at the start.

Accepts any duration `ffmpeg' understands — seconds (\"5\"), MM:SS
(\"1:30\"), or HH:MM:SS.  Percentages are NOT supported by `ffmpeg'
itself.  Set to a single-element list to disable the retry chain."
  :type '(repeat string)
  :group 'media-thumbnail)

(defcustom media-thumbnail-dired-animate-thumbnails t
  "Whether or not `dired' animates thumbnails/gifs."
  :type 'boolean
  :group 'media-thumbnail)

;;
;; (@* "Variables" )
;;

(defvar media-thumbnail--handled-files '()
  "Files already processed by `media-thumbnail'.")

(defvar media-thumbnail--queue '()
  "To be processed by `media-thumbnail'.")

(defvar-local media-thumbnail--redisplay-timer nil
  "Timer used after converting for redisplay.")

(defvar-local media-thumbnail--specs-to-flush nil
  "Image specs to flush to refresh images upon convert finish.")

(defvar-local media-thumbnail--timer nil
  "Buffer-local `media-thumbnail--convert' polling timer.

Set by `media-thumbnail-dired-mode' on enable, cancelled on disable
and on `kill-buffer-hook' — declared here so the disable path can
`cancel-timer' it before nulling the slot.")

(defvar media-thumbnail--async-callbacks (make-hash-table :test 'equal)
  "Hash table mapping in-flight FILE → list of pending callbacks.

Consulted by `media-thumbnail-generate-async' to coalesce concurrent
requests for the same file into a single ffmpeg pipeline.")

;;
;; (@* "Implementation" )
;;

(defun media-thumbnail-get-cache-path (file)
  "Return the cached image path for FILE."
  (format "%s%s.jpg" (expand-file-name media-thumbnail-cache-dir)
          (file-name-base file)))

;;;###autoload
(defun media-thumbnail-for-file (file)
  "Return image spec for FILE."
  (unless (file-exists-p media-thumbnail-cache-dir)
    (make-directory media-thumbnail-cache-dir))
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
      (if (or (member file media-thumbnail--handled-files)
              (file-exists-p cache-path))
          (media-thumbnail--create-image cache-path)
        (push file media-thumbnail--handled-files)
        (let ((image-spec (media-thumbnail--create-image cache-path)))
          ;; Command is not stored on the queue — dired shares the
          ;; same ffmpeg pipeline as the single-shot async path so the
          ;; drain step is a plain call into
          ;; `media-thumbnail-generate-async'.
          (add-to-list 'media-thumbnail--queue
                       `(:image-spec ,image-spec :file ,file)
                       :append)
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
async path.  The queue itself still throttles concurrent processes
via `media-thumbnail-max-processes' so a dired buffer with hundreds
of videos doesn't spawn a subprocess storm."
  (when (and media-thumbnail--queue
             (< (length (process-list))
                media-thumbnail-max-processes))
    (pcase-let* ((convert-request (pop media-thumbnail--queue))
                 (`(:image-spec ,image-spec :file ,file)
                  convert-request)
                 (host-buf (current-buffer)))
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
                         (media-thumbnail--redisplay)))))))))))))))

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

(cl-defun media-thumbnail-ffmpeg-frame-cmd (file cache-path &key size ignore-aspect-ratio seek-time)
  "Return an ffmpeg shell command that decodes a single frame from FILE.

Places `-ss' AFTER `-i' so ffmpeg does an accurate seek — decode from
start until the timestamp — instead of the input-side fast seek that
lands on the nearest prior keyframe.  Fast seek is faster but produces
`frame not finished' errors on files with sparse keyframes; the
accurate variant is one linear read per file and always yields a
complete frame.

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
         (scale (unless (zerop size)
                  (if ignore-aspect-ratio
                      (format "scale=%d:%d" size size)
                    (format "scale=%d:%d:force_original_aspect_ratio=decrease"
                            size size)))))
    (mapconcat
     #'identity
     `(,media-thumbnail-ffmpeg-executable
       "-nostdin" "-y" "-loglevel" "error"
       "-i" ,(shell-quote-argument file)
       "-ss" ,seek
       "-frames:v" "1"
       "-q:v" "2"
       ,@(when scale (list "-vf" (shell-quote-argument scale)))
       ,(shell-quote-argument cache-path))
     " ")))

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
    (media-thumbnail--spawn
     (media-thumbnail-ffmpeg-frame-cmd
      file cache-path
      :size size
      :ignore-aspect-ratio ignore-aspect-ratio
      :seek-time (car seek-times))
     (lambda (frame-ok)
       (if (and frame-ok (media-thumbnail--file-non-empty-p cache-path))
           (media-thumbnail--fire-callbacks file cache-path t)
         (media-thumbnail--ffmpeg-frame-chain
          file cache-path size ignore-aspect-ratio (cdr seek-times))))))))

(defun media-thumbnail--generate (file cache-path size ignore-aspect-ratio)
  "Two-step ffmpeg pipeline: embedded poster, then decoded-frame chain.

Attempts to extract an attached-picture stream first — quick when it
succeeds and generally the best-quality result for files that carry a
poster (music videos, movies with cover art).  When the poster attempt
exits non-zero or produces an empty JPEG (source has no
attached-picture stream, or ffmpeg failed to write it), falls through
to `media-thumbnail--ffmpeg-frame-chain', which walks
`media-thumbnail-ffmpeg-seek-times' until one seek position yields a
decodable frame or the list is exhausted."
  (media-thumbnail--spawn
   (media-thumbnail-ffmpeg-poster-cmd file cache-path)
   (lambda (poster-ok)
     (if (and poster-ok (media-thumbnail--file-non-empty-p cache-path))
         (media-thumbnail--fire-callbacks file cache-path t)
       (media-thumbnail--ffmpeg-frame-chain
        file cache-path size ignore-aspect-ratio
        media-thumbnail-ffmpeg-seek-times)))))

;;;###autoload
(cl-defun media-thumbnail-generate-async (file &key size ignore-aspect-ratio callback)
  "Generate a thumbnail for FILE asynchronously.

Returns the cache path where the JPEG will land.

SIZE and IGNORE-ASPECT-RATIO override `media-thumbnail-size' and
`media-thumbnail-ignore-aspect-ratio' for this call, letting callers
size the thumbnail to fit their preview surface without mutating
package-wide defcustoms.

CALLBACK, if non-nil, is invoked with (FILE CACHE-PATH SUCCESS-P) once
generation completes.  When the JPEG already exists on disk, CALLBACK
is invoked immediately with SUCCESS-P = t and no subprocess is
spawned.

Concurrent requests for the same FILE are coalesced through
`media-thumbnail--async-callbacks': subsequent calls append their
callback to the pending list and share the in-flight pipeline.
Callers do not need their own dedup / in-flight bookkeeping."
  (unless (file-exists-p media-thumbnail-cache-dir)
    (make-directory media-thumbnail-cache-dir t))
  (let ((cache-path (media-thumbnail-get-cache-path file)))
    (cond
     ((file-exists-p cache-path)
      (when callback (funcall callback file cache-path t))
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
    (pcase-let* ((`(:image-spec ,image-spec :file ,file)
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
  (setq media-thumbnail--handled-files nil)
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

(defun media-thumbnail--cancel-timer ()
  "Cancel `media-thumbnail--timer' in the current buffer if still live.

Safe to call from `kill-buffer-hook' and from the mode disable path;
both routes need the timer stopped, and the alternative — letting
`run-with-timer' fire `media-thumbnail--convert' forever after the
dired buffer is gone — leaks CPU proportional to the number of
long-lived dired buffers a user has opened over a session."
  (when (timerp media-thumbnail--timer)
    (cancel-timer media-thumbnail--timer))
  (setq-local media-thumbnail--timer nil))

(define-minor-mode media-thumbnail-dired-mode
  "Toggle `media-thumbnail-dired-mode'."
  :lighter " Dired-Thumbnails"
  (if media-thumbnail-dired-mode
      (progn
        (when (funcall media-thumbnail-dired-should-hide-details-fn)
          (dired-hide-details-mode +1))
        (setq-local media-thumbnail--timer
                    (run-with-timer 0 0.25 #'media-thumbnail--convert))
        (add-hook 'kill-buffer-hook
                  #'media-thumbnail--cancel-timer nil :local)
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
    (media-thumbnail--cancel-timer)
    (remove-hook 'kill-buffer-hook
                 #'media-thumbnail--cancel-timer :local)
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
