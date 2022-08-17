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
(eval-when-compile (require 'subr-x)) ; `if-let*' and `when-let*'

;;
;; (@* "Constants" )
;;

(defconst media-thumbnail-image-exts '("webp" "wmf" "pcx" "xif" "wbmp" "vtf" "tap" "s1j" "sjp" "sjpg" "s1g" "sgi" "sgif" "s1n" "spn" "spng" "xyze" "rgbe" "hdr" "b16" "mdi" "apng" "ico" "pgb" "rlc" "mmr" "fst" "fpx" "fbs" "dxf" "dwg" "djv" "uvvg" "uvg" "uvvi" "uvi" "azv" "psd" "tfx" "t38" "svgz" "svg" "pti" "btf" "btif" "ktx2" "ktx" "jxss" "jxsi" "jxsc" "jxs" "jxrs" "jxra" "jxr" "jxl" "jpf" "jpx" "jpgm" "jpm" "jfif" "jhc" "jph" "jpg2" "jp2" "jls" "hsj2" "hej2" "heifs" "heif" "heics" "heic" "fts" "fit" "fits" "emf" "drle" "cgm" "dib" "bmp" "hif" "avif" "avcs" "avci" "exr" "fax" "icon" "ief" "jpg" "macp" "pbm" "pgm" "pict" "png" "pnm" "ppm" "ras" "rgb" "tga" "tif" "tiff" "xbm" "xpm" "xwd" "jpe" "jpeg"))
(defconst media-thumbnail-video-exts '("f4v" "rmvb" "wvx" "wmx" "wmv" "wm" "asx" "mk3d" "mkv" "fxm" "flv" "axv" "webm" "viv" "yt" "s1q" "smo" "smov" "ssw" "sswf" "s14" "s11" "smpg" "smk" "bk2" "bik" "nim" "pyv" "m4u" "mxu" "fvt" "dvb" "uvvv" "uvv" "uvvs" "uvs" "uvvp" "uvp" "uvvu" "uvu" "uvvm" "uvm" "uvvh" "uvh" "ogv" "m2v" "m1v" "m4v" "mpg4" "mp4" "mjp2" "mj2" "m4s" "3gpp2" "3g2" "3gpp" "3gp" "avi" "mov" "movie" "mpe" "mpeg" "mpegv" "mpg" "mpv" "qt" "vbs"))

;;
;; (@* "Customizations" )
;;

(defcustom media-thumbnail-size 256
  "The size of the icon when creating an icon."
  :type 'number
  :group 'media-thumbnail)

(defcustom media-thumbnail-cache-dir "~/.emacs.d/cache/"
  "Where thumbnails are located."
  :type 'string
  :group 'media-thumbnail)

(defcustom media-thumbnail-image-scale 1
  "How much to scale images when creating them."
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

;;
;; (@* "Variables" )
;;

(defvar media-thumbnail--handled-files '()
  "Files already processed by `media-thumbnail'.")
(defvar media-thumbnail--queue '()
  "To be processed by `media-thumbnail'.")

(defun media-thumbnail-get-cache-path (file)
  "Returns the cached image path for FILE."
  (format "%s%s.jpg" (expand-file-name media-thumbnail-cache-dir)
          (file-name-base file)))

(defun media-thumbnail-ffmpegthumbnailer-cmd (file)
  "Returns a command that generates a thumbnail for FILE."
  (mapconcat
   (lambda (x) x)
   `("ffmpegthumbnailer"
     "-m"
     "-i"
     ,(format "\"%s\"" file)
     "-o"
     ,(format "\"%s\""
              (media-thumbnail-get-cache-path file))
     "-q" "10" ;; Max quality for jpeg
     ;; "-a" ;; Ignore aspect ratio
     "-s"
     ,(number-to-string media-thumbnail-size))
   " "))

(defun media-thumbnail-for-file (file)
  "Returns image spec for FILE."
  (unless (file-exists-p media-thumbnail-cache-dir)
    (make-directory media-thumbnail-cache-dir))
  (cond
   ((string-match-p "^._" (file-name-base file)) nil)
   ((not (file-name-extension file)) nil)
   ((member (downcase (file-name-extension file))
            media-thumbnail-video-exts)
    (unless (or (member file media-thumbnail--handled-files)
                (file-exists-p (media-thumbnail-get-cache-path file)))
      (push file media-thumbnail--handled-files)
      (let ((command (media-thumbnail-ffmpegthumbnailer-cmd file)))
        (add-to-list 'media-thumbnail--queue command :append)))
    (media-thumbnail--create-image (media-thumbnail-get-cache-path file)))
   (:default nil)))

(defun media-thumbnail--create-image (filename)
  "Helper method to create and return an image given FILENAME."
  (create-image filename 'jpeg nil
                :scale media-thumbnail-image-scale
                :margin media-thumbnail-image-margin
                :ascent 'center))

(defun media-thumbnail--convert ()
  "Pop and run tasks in `media-thumbnail--queue'."
  (when (and media-thumbnail--queue
             (< (length (process-list))
                media-thumbnail-max-processes))
    (let ((command (pop media-thumbnail--queue)))
      ;; (message "Calling: %s" command)
      (call-process-shell-command command nil 0))))
;;
;; (@* "Dired" )
;;

(defun media-thumbnail--dired-display ()
  "Display the icons of files in a dired buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (when (dired-move-to-filename nil)
          (dired-move-to-filename)
          (let ((file (dired-get-filename 'verbatim t)))
            (unless (member file '("." ".."))
              (let* ((filename (dired-get-filename nil t))
                     (image (media-thumbnail-for-file filename)))
                (when image
                  (insert-image image " "))))))
        (forward-line 1)))))

;;
;; (@* "User Facing" )
;;

(defun media-thumbnail-clear-all ()
  "Reset everything related to `media-thumbnail'."
  (interactive)
  (message "Clearing `media-thumbnail'...")
  (setq media-thumbnail--queue nil)
  (setq media-thumbnail--handled-files nil)
  (when (file-exists-p media-thumbnail-cache-dir)
    (message "Deleting %s directory." media-thumbnail-cache-dir)
    (delete-directory media-thumbnail-cache-dir t t)))

;;
;; (@* "Minor Mode" )
;;

(define-minor-mode media-thumbnail-mode
  "Toggle `media-thumbnail-mode'.
Interactively with no argument, this command toggles the mode.
A positive prefix argument enables the mode, any other prefix
argument disables it.  From Lisp, argument omitted or nil enables
the mode, `toggle' toggles the state.

When Hungry mode is enabled, the control delete key
gobbles all preceding whitespace except the last.
See the command \\[hungry-electric-delete]."
  :lighter " Media Thumbnails"
  (if media-thumbnail-mode
      (progn
        (setq-local media-thumbnail--timer
                    (run-with-timer 0 0.25 #'media-thumbnail--convert))
        (add-hook 'dired-after-readin-hook
                  'media-thumbnail--dired-display :append :local))
    (setq-local media-thumbnail--timer nil)
    (remove-hook 'dired-after-readin-hook
                 'media-thumbnail--dired-display :local)))

(provide 'media-thumbnail)
;;; media-thumbnail.el ends here
