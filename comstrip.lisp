;; comstrip.lisp
;; Author: Timothy C. Quinn
;; Home: https://github.com/JavaScriptDude/EmacsComStrip
;; Usage:
;; emacs --file=<your_source_code> --quick --batch --eval '(load "<path_to_file>/comstrip.lisp")' 
;; eg emacs --file=~/_t/ffr.py --quick --batch --eval '(load "/dpool/vcmain/dev/lisp/comstrip/comstrip.lisp")'
;; Options:
;; --cs-suffix <suffix> - (opt) Add Suffix to file name before <timestamp>.<ext>
;; # This tool will read in <source_code>, use emacs to strip comments out
;;   and write to /tmp/cs_<file_base>_[<suffix>_]<timestamp>.<ext>
;; TODO:
;; [~] Write a python wrapper for this that will do futher stripping (eg. trailing whitespace) and automation

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
          (setq result (concat result (buffer-substring beg next)))
          (setq beg next))
      )
      ;; (pc "=======================")
      ;; (princ result #'external-debugging-output)
      ;; (pc "=======================")

      ;; Write code to new file
      (generate-new-buffer "nocomm")
      (set-buffer "nocomm")
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
;; CODE SNIP from hide-comnt - https://www.emacswiki.org/emacs/HideOrIgnoreComments
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun hide/show-comments (&optional hide/show start end)
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; END CODE SNIP from hide-comnt
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(main command-line-args-left)
