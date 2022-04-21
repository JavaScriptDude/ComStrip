;; comstrip.el
;; Author: Timothy C. Quinn
;; Home: https://github.com/JavaScriptDude/EmacsComStrip
;; Usage:
;; emacs --file=<your_source_code> --quick --batch --eval '(load "<path_to_file>/comstrip.el")' 
;; eg emacs --file=~/_t/ffr.py --quick --batch --eval '(load "/dpool/vcmain/dev/lisp/comstrip/comstrip.el")'
;; Options:
;; --cs-suffix <suffix> - (opt) Add Suffix to file name before <timestamp>.<ext>
;; # This tool will read in <source_code>, use emacs to strip comments out
;;   and write to /tmp/cs_<file_base>_[<suffix>_]<timestamp>.<ext>
;; TODO:
;; [~] Write a wrapper for this that will do futher stripping (eg. trailing whitespace)

;; /usr/local/bin/emacs --file="./comstrip.py" --quick --eval '(load "/dpool/vcmain/dev/lisp/comstrip/comstrip.el")'

(defun main (args)
  ;; (pc (format "main() called args = %s" args))

  (setq _suffix nil)

  ;; Process cli args
  (while command-line-args-left 
    (setq k (car command-line-args-left))
    (setq command-line-args-left (cdr command-line-args-left))
    (setq command-line-args (delete k command-line-args))
    (cond
      ((string-equal k "--cs-suffix")
        (setq v (intern (car command-line-args-left)))
        (setq command-line-args-left (cdr command-line-args-left))
        (setq command-line-args (delete v command-line-args))
        (setq _suffix v)
      )
    )
  )

  ;; Normalize CLI args
  (if (or (not _suffix) (string-equal _suffix ""))
      (setq _suffix "")
      (setq _suffix (format "_%s" _suffix)))

  (setq in_file_full (buffer-file-name))
  (unless in_file_full
    (error "No --file specified. Please put --file=<file> at beginning of arguments"))

  (setq in_filename (file-name-nondirectory in_file_full))
  (setq in_file_ext (file-name-extension in_filename))
  (setq in_file_base (file-name-sans-extension in_filename))

  (setq file_out_full (format "/tmp/cs_%s%s_%s.%s" in_file_base _suffix (format-time-string "%H%M%S%3N") in_file_ext))

  ;; (error (format "file_out_full: %s" file_out_full))
  
  ;; (setq suffix (if (and (= (length args) 2) (= (nth 0 args) "--suffix"))
  ;;   (nth 1 args)
  ;;   nil
  ;; ))

  ;; (pc (format "suffix: %s" (if suffix suffix "-")))


  ;; try:
    (condition-case err
      (let ()

        
        ;; (pc (format "Buffer size: %d" (point-max)))

        ; Check buffer size
        (if (= (point-max) 1)
          (let ()
            (if (not (file-exists-p in_file_full))(error "File not found"))
            (error "Empty file or unreadable")
          )
        )

        ;; Toggle comments
        (hide/show-comments 'hide (point-min) (point-max))

        ;; Save code without comments to new file    
        (scrape_comments (point-min) (point-max) file_out_full)
      )

  ;; catch:
    (error     
      (setq msg (error-message-string err))         
      (setq msg (s-replace "\"" "\\\"" msg))
      (setq msg (s-replace "\n" "\\\n" msg))
      (setq msg (s-replace "\r" "" msg))
      (setq msg (s-replace "\t" " " msg))
      (setq file_in (buffer-file-name))
      (if (not file_in) (setq file_in "-"))
      (error ".:CSMSG_S:.failed\t%s\t%s}.:CSMSG_E:." file_in msg)))

  ;; Clean exit  
  (pc (format ".:CSMSG_S:.ok\t%s\t%s.:CSMSG_E:." in_file_full file_out_full))

)


(defun scrape_comments (beg end file_out_full)
    "Save code without comments to output file."
    (interactive (list (point-min) (point-max)))
    (let ((result ""))
      (while (/= beg end)
        (when (get-char-property beg 'invisible)
          (setq beg (next-single-char-property-change beg 'invisible nil end)))

        (let ((next (next-single-char-property-change beg 'invisible nil end)))
          (setq block (buffer-substring beg next))
          ;;(pc (format "Line: '%s'" block))
          (setq result (concat result block))
          (setq beg next))
      )
      ;; (pc "=======================")
      ;; (princ result #'external-debugging-output)
      ;; (pc "=======================")

      ;; Write code to new file
      (generate-new-buffer "nocomm")
      (set-buffer "nocomm")
      (if (and (length result) (string-equal (substring result 0 1) "\n"))
        ;;then
        (setq result (substring result 1))
      )
      (insert result)
      (set-visited-file-name file_out_full)
      (save-buffer)

      ;; (pc (format "See %s for results." file_out_full))
    )
)

;; eg (pc "foobar")
;;    (pc (format "foobar %s" "bazbar"))
;;    (pc (list "foo" (require 'newcomment nil t)))
(defun pc (s)
    "Print to console"
    (princ (format "%s\n" s) #'external-debugging-output))

;; eg (pc "foobar")
;;    (pc (format "foobar %s" "bazbar"))
(defun _exit (&optional s)
  "Print and exit"
  (if s
     ;;then
     (error (format "_exit called - %s\n" s)) 
     ::else
     (error "_exit called")
  )
)

(defun s-replace (old new s)
  "Replaces OLD with NEW in S."
  (declare (pure t) (side-effect-free t))
  (replace-regexp-in-string (regexp-quote old) new s t t))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; hide-comnt v40 - https://www.emacswiki.org/emacs/HideOrIgnoreComments
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defcustom ignore-comments-flag t
  "*Non-nil means macro `with-comments-hidden' hides comments."
  :type 'boolean :group 'matching)

;;;###autoload
(defcustom hide-whitespace-before-comment-flag t
  "*Non-nil means `hide/show-comments' hides whitespace preceding a comment.
It does not hide empty lines (newline chars), however."
  :type 'boolean :group 'matching)

;;;###autoload
(defcustom show-invisible-comments-shows-all nil
  "Non-nil means `(hide/show-comments 'show ...)' shows all invisible text.
The default value, nil, means it shows only text that was made
invisible by `(hide/show-comments 'hide ...)'."
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
      (unwind-protect (setq ,result  (progn (when ignore-comments-flag
                                              (hide/show-comments 'hide ,ostart ,oend))
                                            ,@body))
        (when ignore-comments-flag (hide/show-comments 'show ,ostart ,oend))
        ,result))))

;;;###autoload
(defun hide/show-comments (&optional hide/show start end)
  "Hide or show comments from START to END.
Interactively, hide comments, or show them if you use a prefix arg.
\(This is thus *NOT* a toggle command.)

If option `hide-whitespace-before-comment-flag' is non-nil, then hide
also any whitespace preceding a comment.

Interactively, START and END default to the region limits, if active.
Otherwise, including non-interactively, they default to `point-min'
and `point-max'.

Uses `save-excursion', restoring point.

Option `show-invisible-comments-shows-all':

* If non-nil then using this command to show invisible text shows
  *ALL* such text, regardless of how it was hidden.  IOW, it does not
  just show invisible text that you previously hid using this command.

* If nil (the default value) then using this command to show invisible
  text makes visible only such text that was previously hidden by this
  command.  (More precisely, it makes visible only text whose
  `invisible' property has value `hide-comment'.)

From Lisp, a HIDE/SHOW value of `hide' hides comments.  Other values
show them.

This command does nothing in Emacs versions prior to Emacs 21, because
it needs `comment-search-forward'."

  (interactive
   (cons (if current-prefix-arg 'show 'hide)
         (if (or (not mark-active)  (null (mark))  (= (point) (mark)))
             (list (point-min) (point-max))
           (if (< (point) (mark)) (list (point) (mark)) (list (mark) (point))))))
  (when (and comment-start              ; No-op if no comment syntax defined.
             (require 'newcomment nil t)) ; `comment-search-forward', Emacs 21+.
    (comment-normalize-vars 'NO-ERROR)  ; Must call this first.
    (unless start (setq start  (point-min)))
    (unless end   (setq end    (point-max)))
    (unless (<= start end) (setq start  (prog1 end (setq end  start)))) ; Swap.
    (if (fboundp 'with-silent-modifications)
        (with-silent-modifications      ; Emacs 23+.
            (restore-buffer-modified-p nil) (hide/show-comments-1 hide/show start end))
      (let ((bufmodp           (buffer-modified-p)) ; Emacs < 23.
            (buffer-read-only  nil)
            (buffer-file-name  nil))    ; Inhibit `ask-user-about-supersession-threat'.
        (set-buffer-modified-p nil)
        (unwind-protect (hide/show-comments-1 hide/show start end)
          (set-buffer-modified-p bufmodp))))))

;; Used only so that we can use `hide/show-comments' with older Emacs releases that do not
;; have macro `with-silent-modifications' and built-in `restore-buffer-modified-p', which
;; it uses.
(defun hide/show-comments-1 (hide/show start end)
  "Helper for `hide/show-comments'."
  (let (cbeg cend)
    (if (eq 'hide hide/show)
        (add-to-invisibility-spec 'hide-comment)
      (remove-from-invisibility-spec 'hide-comment))
    (save-excursion
      (goto-char start)
      (while (and (< start end)  (save-excursion
                                   (setq cbeg  (comment-search-forward end 'NOERROR))))
        (goto-char cbeg)
        (save-excursion
          (setq cend  (cond ((fboundp 'comment-forward) ; Emacs 22+
                             (if (comment-forward 1)
                                 (if (= (char-before) ?\n) (1- (point)) (point))
                               end))
                            ((string= "" comment-end) (min (line-end-position) end))
                            (t (search-forward comment-end end 'NOERROR)))))
        (when hide-whitespace-before-comment-flag ; Hide preceding whitespace.
          (if (fboundp 'looking-back)   ; Emacs 22+
              (when (looking-back "\n?\\s-*" nil 'GREEDY)
                (setq cbeg  (match-beginning 0)))
            (while (memq (char-before cbeg) '(?\   ?\t ?\f)) (setq cbeg  (1- cbeg)))
            (when (eq (char-before cbeg) ?\n) (setq cbeg  (1- cbeg))))
          ;; First line: Hide newline after comment.
          (when (and (= cbeg (point-min))  (= (char-after cend) ?\n))
            (setq cend  (min (1+ cend) end))))
        (when (and cbeg  cend)
          (if show-invisible-comments-shows-all
              (put-text-property cbeg cend 'invisible (and (eq 'hide hide/show)
                                                           'hide-comment))
            (while (< cbeg cend)
              ;; Do nothing to text that is already invisible for some other reason.
              (unless (and (get-text-property cbeg 'invisible)
                           (not (eq 'hide-comment (get-text-property cbeg 'invisible))))
                (put-text-property cbeg (1+ cbeg) 'invisible (and (eq 'hide hide/show)
                                                                  'hide-comment)))
              (setq cbeg  (1+ cbeg)))))
        (goto-char (setq start  (or cend  end)))))))

(defun hide/show-comments-toggle (&optional start end)
  "Toggle hiding/showing of comments in the active region or whole buffer.
If the region is active then toggle in the region.  Otherwise, in the
whole buffer.

This command does nothing in Emacs versions prior to Emacs 21, because
it needs `comment-search-forward'.

Interactively, START and END default to the region limits, if active.
Otherwise, including non-interactively, they default to `point-min'
and `point-max'.

See `hide/show-comments' for more information."
  (interactive (if (or (not mark-active)  (null (mark))  (= (point) (mark)))
                   (list (point-min) (point-max))
                 (if (< (point) (mark)) (list (point) (mark)) (list (mark) (point)))))
  (when (require 'newcomment nil t)     ; `comment-search-forward', Emacs 21+.
    (comment-normalize-vars)            ; Must call this first.
    (if (save-excursion
          (goto-char start)
          (and (comment-search-forward end 'NOERROR)
               (if show-invisible-comments-shows-all
                   (get-text-property (point) 'invisible)
                 (eq 'hide-comment (get-text-property (point) 'invisible)))))
        (hide/show-comments 'show start end)
      (hide/show-comments 'hide start end))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; END CODE SNIP from hide-comnt
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(main command-line-args-left)
