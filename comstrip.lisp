;; comstrip.lisp
;; Author: Timothy C. Quinn
;; Home: https://github.com/JavaScriptDude/EmacsComStrip
;; Usage:
;; cd <path containing comstrip.lisp>
;; emacs --file=<path_to_source_code> --quick --batch --eval '(load "./comstrip.lisp")'
;; # This tool will read in <source_code>, use emacs to strip comments out and the write to /tmp/<file_name>_comstrip
;; TODO:
;; [.] Validate args. If no file is passed in, exit
;; [.] Cleanly exist on error conditions
;; [.] Write a wrapper for this that will do futher stripping (eg. trailing whitespace)
;; [.] 

(defun main (args)
  (pc "main() called")

  ;; process args
  ;; (pc (format "args: %s" args))

  (setq in_file_full (buffer-file-name))
  ;; (pc (format "in_file_full: %s" in_file_full))

  (pc (format "Buffer size: %d" (point-max)))

  (setq in_dir (file-name-directory in_file_full))
  (setq in_file (file-name-nondirectory in_file_full))
  ;; (setq in_file_ext (file-name-sans-extension in_file))
  ;; (setq in_file_base (substring in_file 0 (- (length in_file) (length in_file_ext))))
  (pc (format "in_dir: %s, in_file: %s" in_dir in_file))

  (setq _file_out (format "/tmp/%s_comstrip" in_file))
  (pc (format "_file_out: %s" _file_out))
  

  ;; Tooggle comments
  ;; (hide/show-comments 'hide (point-min) (point-max))
  (hide/show-comments 'hide (point-min) (point-max))

  ;; (pc "Comments toggled")
  
  (scrape_comments (point-min) (point-max) _file_out)

  ;; (hide/show-comments-copy (point-min) (point-max))

  ;; (pc "Program ended\n")

)


(defun scrape_comments (beg end file_out)
    "Copy the entire buffer or selected."
    (interactive (list (point-min) (point-max)))
    (let ((result ""))
      (while (/= beg end)
        (when (get-char-property beg 'invisible)
          (setq beg (next-single-char-property-change beg 'invisible nil end)))

        (let ((next (next-single-char-property-change beg 'invisible nil end)))
          (setq result (concat result (buffer-substring beg next)))
          (setq beg next))
      )
      ;; (kill-new result)
      ;; (pc "=======================")
      ;; (princ result #'external-debugging-output)
      ;; (pc "=======================")
      ;; (pc (format "suppress out to %s" file_out))

      ;; (pc (format "Output written to %s" file_out))

      ;; Write code to new file
      (generate-new-buffer "nocomm")
      (set-buffer "nocomm")
      (insert result)
      (set-visited-file-name file_out)
      (save-buffer)
      (pc (format "See %s for results." file_out))

    )
)

;; eg (pc "foobar")
;;    (pc (format "foobar %s" "bazbar"))
(defun pc (s)
    "Print to console"
    (princ (format "%s\n" s) #'external-debugging-output))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Start hide-comnt - https://www.emacswiki.org/emacs/HideOrIgnoreComments
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; hide-comnt.el --- Hide/show comments in code.
;;
;; Filename: hide-comnt.el
;; Description: Hide/show comments in code.
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 2011-2012, Drew Adams, all rights reserved.
;; Created: Wed May 11 07:11:30 2011 (-0700)
;; Version:
;; Last-Updated: Thu Aug 23 13:37:49 2012 (-0700)
;;           By: dradams
;;     Update #: 40
;; URL: http://www.emacswiki.org/cgi-bin/wiki/hide-comnt.el
;; Doc URL: http://www.emacswiki.org/emacs/HideOrIgnoreComments
;; Keywords: comment, hide, show
;; Compatibility: GNU Emacs: 21.x, 22.x, 23.x
;;
;; Features that might be required by this library:
;;
;;   None
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;  Hide/show comments in code.
;;
;;
;;  Macros defined here:
;;
;;    `with-comments-hidden'.
;;
;;  Commands defined here:
;;
;;    `hide/show-comments', `hide/show-comments-toggle'.
;;
;;  User options defined here:
;;
;;    `ignore-comments-flag'.
;;
;;
;;  Put this in your init file (`~/.emacs'):
;;
;;   (require 'hide-comnt)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;; 2012/05/10 dadams
;;     Added: hide/show-comments-toggle.  Thx to Denny Zhang for the suggestion.
;; 2011/11/23 dadams
;;     hide/show-comments: Bug fix - ensure CEND is not past eob.
;; 2011/05/11 dadams
;;     Created: moved code here from thing-cmds.el.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:


;;;###autoload
(defcustom ignore-comments-flag t
  "Non-nil means macro `with-comments-hidden' hides comments."
  :type 'boolean :group 'matching)


(defmacro with-comments-hidden (start end &rest body)
  "Evaluate the forms in BODY while comments are hidden from START to END.
But if `ignore-comments-flag' is nil, just evaluate BODY,
without hiding comments.  Show comments again when BODY is finished.

See `hide/show-comments', which is used to hide and show the comments.
Note that prior to Emacs 21, this never hides comments."
  (let ((result  (make-symbol "result"))
        (ostart  (make-symbol "ostart"))
        (oend    (make-symbol "oend")))
    `(let ((,ostart  ,start)
           (,oend    ,end)
           ,result)
      (unwind-protect
           (setq ,result  (progn (when ignore-comments-flag
                                   (hide/show-comments 'hide ,ostart ,oend))
                                 ,@body))
        (when ignore-comments-flag (hide/show-comments 'show ,ostart ,oend))
        ,result))))

;;;###autoload
(defun hide/show-comments (&optional hide/show start end)
  "Hide or show comments from START to END.
Interactively, hide comments, or show them if you use a prefix arg.
Interactively, START and END default to the region limits, if active.
Otherwise, including non-interactively, they default to `point-min'
and `point-max'.

Uses `save-excursion', restoring point.

Be aware that using this command to show invisible text shows *all*
such text, regardless of how it was hidden.  IOW, it does not just
show invisible text that you previously hid using this command.

From Lisp, a HIDE/SHOW value of `hide' hides comments.  Other values
show them.

This function does nothing in Emacs versions prior to Emacs 21,
because it needs `comment-search-forward'."

  (interactive
   (cons (if current-prefix-arg 'show 'hide)
         (if (or (not mark-active) (null (mark)) (= (point) (mark)))
             (list (point-min) (point-max))
           (if (< (point) (mark)) (list (point) (mark)) (list (mark) (point))))))

  (when (require 'newcomment nil t)     ; `comment-search-forward', Emacs 21+.
    (unless start (setq start  (point-min)))
    (unless end   (setq end    (point-max)))
    (unless (<= start end) (setq start  (prog1 end (setq end  start))))

    (let ((bufmodp           (buffer-modified-p))
          (buffer-read-only  nil)
          cbeg cend)
      (unwind-protect
           (save-excursion
             (goto-char start)
             (while (and (< start end) (setq cbeg  (comment-search-forward end 'NOERROR)))
               (setq cend  (if (string= "" comment-end)
                               (min (1+ (line-end-position)) (point-max))
                             (search-forward comment-end end 'NOERROR)))
               (when (and cbeg cend)
                 (if (eq 'hide hide/show)
                     (put-text-property cbeg cend 'invisible t)
                   (put-text-property cbeg cend 'invisible nil)))))
        (set-buffer-modified-p bufmodp)))))

(defun hide/show-comments-toggle (&optional start end)
  "Toggle hiding/showing of comments in the active region or whole buffer.
If the region is active then toggle in the region.  Otherwise, in the
whole buffer.

Interactively, START and END default to the region limits, if active.
Otherwise, including non-interactively, they default to `point-min'
and `point-max'.

See `hide/show-comments' for more information."
  (interactive (if (or (not mark-active)  (null (mark))  (= (point) (mark)))
                   (list (point-min) (point-max))
                 (if (< (point) (mark)) (list (point) (mark)) (list (mark) (point)))))
  (if (save-excursion (goto-char start) (and (comment-search-forward end 'NOERROR)
                                             (get-text-property (point) 'invisible)))
      (hide/show-comments 'show start end)
    (hide/show-comments 'hide start end)))

;;;;;;;;;;;;;;;;;;;;;;;;
;; commenting out provides as its redundant here
;; (provide 'hide-comnt)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; hide-comnt.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(main command-line-args-left)
