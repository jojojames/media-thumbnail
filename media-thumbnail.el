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
(defconst media-thumbnail-audio-exts '("ape" "stm" "s3m" "ra" "rm" "ram" "wma" "wax" "m3u" "med" "669" "mtm" "m15" "uni" "ult" "mka" "flac" "axa" "kar" "midi" "mid" "s1m" "smp" "smp3" "rip" "multitrack" "ecelp9600" "ecelp7470" "ecelp4800" "vbk" "pya" "lvp" "plj" "dtshd" "dts" "mlp" "eol" "uvva" "uva" "koz" "xhe" "loas" "sofa" "smv" "qcp" "psid" "sid" "spx" "opus" "ogg" "oga" "mp1" "mpga" "m4a" "mxmf" "mhas" "l16" "lbc" "evw" "enw" "evb" "evc" "dls" "omg" "aa3" "at3" "atx" "aal" "acn" "awb" "amr" "ac3" "ass" "aac" "adts" "726" "abs" "aif" "aifc" "aiff" "au" "mp2" "mp3" "mp2a" "mpa" "mpa2" "mpega" "snd" "vox" "wav"))
(defconst media-thumbnail-video-exts '("f4v" "rmvb" "wvx" "wmx" "wmv" "wm" "asx" "mk3d" "mkv" "fxm" "flv" "axv" "webm" "viv" "yt" "s1q" "smo" "smov" "ssw" "sswf" "s14" "s11" "smpg" "smk" "bk2" "bik" "nim" "pyv" "m4u" "mxu" "fvt" "dvb" "uvvv" "uvv" "uvvs" "uvs" "uvvp" "uvp" "uvvu" "uvu" "uvvm" "uvm" "uvvh" "uvh" "ogv" "m2v" "m1v" "m4v" "mpg4" "mp4" "mjp2" "mj2" "m4s" "3gpp2" "3g2" "3gpp" "3gp" "avi" "mov" "movie" "mpe" "mpeg" "mpegv" "mpg" "mpv" "qt" "vbs"))

;;
;; (@* "Customizations" )
;;

(defcustom media-thumbnail-size 1000
  "The size of the icon when creating an icon."
  :type 'number
  :group 'media-thumbnail)

(defcustom media-thumbnail-cache-dir "~/.emacs.d/cache/"
  ""
  :type 'string
  :group 'media-thumbnail)

(defcustom media-thumbnail-image-scale .1
  ""
  :type 'float
  :group 'media-thumbnail)

(defcustom media-thumbnail-max-processes 20
  ""
  :type 'int
  :group 'media-thumbnail)

;;
;; (@* "Variables" )
;;

(defvar media-thumbnail-handled-files '())
(defvar media-thumbnail-queue '())

(defun media-thumbnail-get-cache-path (file)
  "Returns the cached image thumbnail for FILE."
  (format "%s%s.jpg" (expand-file-name media-thumbnail-cache-dir)
          (media-thumbnail-basefile-with-extension file)))

(defun media-thumbnail-ffmpegthumbnailer-cmd (file)
  (mapconcat
   (lambda (x) x)
   `("ffmpegthumbnailer"
     "-m"
     "-i"
     ,(format "\"%s\"" file)
     "-o"
     ,(format "\"%s\""
              (media-thumbnail-get-cache-path file))
     "-s"
     ,(number-to-string media-thumbnail-size))
   " "))

(defun media-thumbnail-for-file (file)
  ""
  (unless (file-exists-p media-thumbnail-cache-dir)
    (make-directory media-thumbnail-cache-dir))
  (when (and (member (file-name-extension file) '("mkv" "mp4" "flv" "MP4"))
             (not (string-match-p "^._" (file-name-base file))))
    (if (file-exists-p (media-thumbnail-get-cache-path file))
        (media-thumbnail-create-image (media-thumbnail-get-cache-path file))
      (if (member file media-thumbnail-handled-files)
          nil
        (push file media-thumbnail-handled-files)
        (let ((command
               (media-thumbnail-ffmpegthumbnailer-cmd file)))
          (add-to-list 'media-thumbnail-queue command :append)
          nil)))))

(defun media-thumbnail-create-image (filename)
  "Helper method to create and return an image given FILENAME."
  (create-image filename 'jpeg nil
                :scale media-thumbnail-image-scale
                :ascent 'center))

(defun media-thumbnail--convert ()
  "Pop and run tasks in `media-thumbnail-queue'."
  (when (and media-thumbnail-queue
             (< (length (process-list))
                media-thumbnail-max-processes))
    (let ((command (pop media-thumbnail-queue)))
      (call-process-shell-command command nil 0)
      (message "Calling: %s" command))))

(setq media-thumbnail--timer
      (run-with-timer 0 0.25 #'media-thumbnail--convert))


(defun media-thumbnail-clear-all ()
  ""
  (interactive)
  (setq media-thumbnail-handled-files nil)
  (setq dired-update--timer nil)
  (delete-directory media-thumbnail-cache-dir t))
;;
;; (@* "Dired" )
;;

(advice-add 'media-thumbnail--convert :after 'media-thumbnail-set-up-timer)

(defvar dired-update--timer nil)

(defun media-thumbnail-set-up-timer ()
  (unless dired-update--timer
    (setq dired-update--timer
          (run-with-idle-timer 2 nil 'media-thumbnail-dired-update))))

(defun media-thumbnail-dired-update ()
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (derived-mode-p 'dired-mode)
        (dired-revert))))
  (setq dired-update--timer nil))

(add-hook 'dired-after-readin-hook
          'media-thumbnail-dired-display :append)

(defun media-thumbnail-dired-display ()
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
;; (@* "Util" )
;;

(defun media-thumbnail-basefile-with-extension (file)
  "Return base filename with extension given FILE.

: ~/a/b.json -> b.json

If there is no extension, just return the base file name."
  (let ((base (file-name-base file))
        (ext (file-name-extension file)))
    (if (and base ext)
        (format "%s.%s" base ext)
      base)))

(provide 'media-thumbnail)
;;; media-thumbnail.el ends here
