;(***********************************************************************)
;(*                                                                     *)
;(*                           Objective Caml                            *)
;(*                                                                     *)
;(*                   Xavier Leroy and Jacques Garrigue                 *)
;(*                                                                     *)
;(*  Copyright 1997 Institut National de Recherche en Informatique et   *)
;(*  en Automatique.  All rights reserved.  This file is distributed    *)
;(*  under the terms of the GNU General Public License.                 *)
;(*                                                                     *)
;(***********************************************************************)

;(* $Id: inf-rml.el,v 1.2 2005/06/21 16:26:49 mandel Exp $ *)

;;; inf-caml.el --- run the Caml toplevel in an Emacs buffer

;; Xavier Leroy, july 1993.

;; modified by Jacques Garrigue, july 1997.

;; indentation code adapted for ReactiveML by Sarah Maarek,
;; july 2004.

(require 'comint)
(require 'rml)

;; User modifiable variables

;; Whether you want the output buffer to be diplayed when you send a phrase

(defvar rml-display-when-eval t
  "*If true, display the inferior rml buffer when evaluating expressions.")


;; End of User modifiable variables


(defvar inferior-rml-mode-map nil)
(if inferior-rml-mode-map nil
  (setq inferior-rml-mode-map
        (copy-keymap comint-mode-map)))

;; Augment Rml mode, so you can process Rml code in the source files.

(defvar inferior-rml-program "rmltop"
  "*Program name for invoking an inferior Rml from Emacs.")

(defun inferior-rml-mode ()
  "Major mode for interacting with an inferior Rml process.
Runs a Rml toplevel as a subprocess of Emacs, with I/O through an
Emacs buffer. A history of input phrases is maintained. Phrases can
be sent from another buffer in Rml mode.

\\{inferior-rml-mode-map}"
  (interactive)
  (comint-mode)
  (setq comint-prompt-regexp "^# ?")
  (setq major-mode 'inferior-rml-mode)
  (setq mode-name "Inferior Rml")
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat "^$\\|" page-delimiter))
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
  (make-local-variable 'paragraph-ignore-fill-prefix)
  (setq paragraph-ignore-fill-prefix t)
  (make-local-variable 'require-final-newline)
  (setq require-final-newline t)
  (make-local-variable 'comment-start)
  (setq comment-start "(*")
  (make-local-variable 'comment-end)
  (setq comment-end "*)")
  (make-local-variable 'comment-column)
  (setq comment-column 40)
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "(\\*+ *")
  (make-local-variable 'parse-sexp-ignore-comments)
  (setq parse-sexp-ignore-comments nil)
  (setq comint-scroll-to-bottom-on-output t)
  (use-local-map inferior-rml-mode-map)
  (run-hooks 'inferior-rml-mode-hooks))


(defconst inferior-rml-buffer-subname "inferior-rml")
(defconst inferior-rml-buffer-name
  (concat "*" inferior-rml-buffer-subname "*"))

;; for compatibility with xemacs

(defun rml-sit-for (second &optional mili redisplay)
   (if (and (boundp 'running-xemacs) running-xemacs)
       (sit-for (if mili (+ second (* mili 0.001)) second) redisplay)
     (sit-for second mili redisplay)))

;; To show result of evaluation at toplevel

(defvar inferior-rml-output nil)
(defun inferior-rml-signal-output (s)
  (if (string-match "[^ ]" s) (setq inferior-rml-output t)))

(defun inferior-rml-mode-output-hook ()
  (setq comint-output-filter-functions
        (list (function inferior-rml-signal-output))))
(add-hook 'inferior-rml-mode-hooks 'inferior-rml-mode-output-hook)

;; To launch rml whenever needed

(defun rml-run-process-if-needed (&optional cmd)
  (if (comint-check-proc inferior-rml-buffer-name) nil
    (if (not cmd)
        (if (comint-check-proc inferior-rml-buffer-name)
            (setq cmd inferior-rml-program)
          (setq cmd (read-from-minibuffer "Rml toplevel to run: "
                                          inferior-rml-program))))
    (setq inferior-rml-program cmd)
    (let ((cmdlist (inferior-rml-args-to-list cmd))
          (process-connection-type nil))
      (set-buffer (apply (function make-comint)
                         inferior-rml-buffer-subname
                         (car cmdlist) nil (cdr cmdlist)))
      (inferior-rml-mode)
      (display-buffer inferior-rml-buffer-name)
      t)
    (setq rml-shell-active t)
    ))

;; patched to from original run-rml sharing code with
;;  rml-run-process-when-needed

(defun run-rml (&optional cmd)
  "Run an inferior Rml process.
Input and output via buffer `*inferior-rml*'."
  (interactive
   (list (if (not (comint-check-proc inferior-rml-buffer-name))
             (read-from-minibuffer "Rml toplevel to run: "
                                   inferior-rml-program))))
  (rml-run-process-if-needed cmd)
  (switch-to-buffer-other-window inferior-rml-buffer-name))


(defun inferior-rml-args-to-list (string)
  (let ((where (string-match "[ \t]" string)))
    (cond ((null where) (list string))
          ((not (= where 0))
           (cons (substring string 0 where)
                 (inferior-rml-args-to-list (substring string (+ 1 where)
                                                        (length string)))))
          (t (let ((pos (string-match "[^ \t]" string)))
               (if (null pos)
                   nil
                 (inferior-rml-args-to-list (substring string pos
                                                        (length string)))))))))

(defun inferior-rml-show-subshell ()
  (interactive)
  (rml-run-process-if-needed)
  (display-buffer inferior-rml-buffer-name)
  ; Added by Didier to move the point of inferior-rml to end of buffer
  (let ((buf (current-buffer))
        (rml-buf  (get-buffer inferior-rml-buffer-name))
        (count 0))
    (while
        (and (< count 10)
             (not (equal (buffer-name (current-buffer))
                         inferior-rml-buffer-name)))
      (next-multiframe-window)
      (setq count (+ count 1)))
    (if  (equal (buffer-name (current-buffer))
                inferior-rml-buffer-name)
        (end-of-buffer))
    (while
        (> count 0)
      (previous-multiframe-window)
      (setq count (- count 1)))
    )
)

;; patched by Didier to move cursor after evaluation

(defun inferior-rml-eval-region (start end)
  "Send the current region to the inferior Rml process."
  (interactive "r")
  (save-excursion (rml-run-process-if-needed))
  (save-excursion
    (goto-char end)
    (rml-skip-comments-backward)
    (comint-send-region inferior-rml-buffer-name start (point))
    ;; normally, ";;" are part of the region
    (if (and (>= (point) 2)
             (prog2 (backward-char 2) (looking-at ";;")))
        (comint-send-string inferior-rml-buffer-name "\n")
      (comint-send-string inferior-rml-buffer-name ";;\n"))
    ;; the user may not want to see the output buffer
    (if rml-display-when-eval
        (display-buffer inferior-rml-buffer-name t))))

;; jump to errors produced by rml compiler

(defun inferior-rml-goto-error (start end)
  "Jump to the location of the last error as indicated by inferior toplevel."
  (interactive "r")
  (let ((loc (+ start
                (save-excursion
                  (set-buffer (get-buffer inferior-rml-buffer-name))
                  (re-search-backward
                   (concat comint-prompt-regexp
                           "[ \t]*Characters[ \t]+\\([0-9]+\\)-[0-9]+:$"))
                  (string-to-int (match-string 1))))))
    (goto-char loc)))


;;; orgininal inf-rml.el ended here

;; as eval-phrase, but ignores errors.

(defun inferior-rml-just-eval-phrase (arg &optional min max)
  "Send the phrase containing the point to the RML process.
With prefix-arg send as many phrases as its numeric value,
ignoring possible errors during evaluation.

Optional arguments min max defines a region within which the phrase
should lies."
  (interactive "p")
  (let ((beg))
    (while (> arg 0)
      (setq arg (- arg 1))
      (setq beg  (rml-find-phrase min max))
      (rml-eval-region beg (point)))
    beg))

(defvar rml-previous-output nil
  "tells the beginning of output in the shell-output buffer, so that the
output can be retreived later, asynchronously.")

;; enriched version of eval-phrase, to repport errors.

(defun inferior-rml-eval-phrase (arg &optional min max)
  "Send the phrase containing the point to the RML process.
With prefix-arg send as many phrases as its numeric value,
If an error occurs during evalutaion, stop at this phrase and
repport the error.

Return nil if noerror and position of error if any.

If arg's numeric value is zero or negative, evaluate the current phrase
or as many as prefix arg, ignoring evaluation errors.
This allows to jump other erroneous phrases.

Optional arguments min max defines a region within which the phrase
should lies."
  (interactive "p")
  (if (save-excursion (rml-run-process-if-needed))
      (progn
        (setq inferior-rml-output nil)
        (rml-wait-output 10 1)))
  (if (< arg 1) (inferior-rml-just-eval-phrase (max 1 (- 0 arg)) min max)
    (let ((proc (get-buffer-process inferior-rml-buffer-name))
          (buf (current-buffer))
          previous-output orig beg end err
	  inf-point)
      (save-window-excursion
        (while (and (> arg 0) (not err))
          (setq previous-output (marker-position (process-mark proc)))
          (setq rml-previous-output previous-output)
          (setq inferior-rml-output nil)
          (setq orig (inferior-rml-just-eval-phrase 1 min max))
          (rml-wait-output)
          (switch-to-buffer inferior-rml-buffer-name  nil)
          (goto-char previous-output)
          (cond ((re-search-forward
                  " *Characters \\([01-9][01-9]*\\)-\\([1-9][01-9]*\\):\n[^W]"
                  (point-max) t)
                 (setq beg (string-to-int (rml-match-string 1)))
                 (setq end (string-to-int (rml-match-string 2)))
                 (switch-to-buffer buf)
                 (goto-char orig)
                 (forward-byte end)
                 (setq end (point))
                 (goto-char orig)
                 (forward-byte beg)
                 (setq beg (point))
                 (setq err beg)
                 )
                ((looking-at
                  "Toplevel input:\n[>]\\([^\n]*\\)\n[>]\\(\\( *\\)^*\\)\n")
                 (let ((expr (rml-match-string 1))
                       (column (-   (match-end 3) (match-beginning 3)))
                       (width (-   (match-end 2) (match-end 3))))
                   (if (string-match  "^\\(.*\\)[<]EOF[>]$" expr)
                       (setq expr (substring expr (match-beginning 1) (match-end 1))))
                   (switch-to-buffer buf)
                   (re-search-backward
                    (concat "^" (regexp-quote expr) "$")
                    (- orig 10))
                   (goto-char (+ (match-beginning 0) column))
                   (setq end (+ (point) width)))
                 (setq err beg))
                ((looking-at
                  "Toplevel input:\n>[.]*\\([^.].*\n\\)\\([>].*\n\\)*[>]\\(.*[^.]\\)[.]*\n")
                 (let* ((e1 (rml-match-string 1))
                        (e2 (rml-match-string 3))
                        (expr
                         (concat
                          (regexp-quote e1) "\\(.*\n\\)*" (regexp-quote e2))))
                   (switch-to-buffer buf)
                   (re-search-backward expr orig 'move)
                   (setq end (match-end 0)))
                 (setq err beg))
                (t
                 (switch-to-buffer buf)))
          (setq arg (- arg 1)))
        (pop-to-buffer inferior-rml-buffer-name)
        (if err
            (goto-char (point-max))
          (goto-char previous-output)
          (goto-char (point-max)))
        (pop-to-buffer buf)
	(with-current-buffer inferior-rml-buffer-name
	  (setq inf-point (point))))
      (let ((owindow (selected-window)))
	(dolist (w (get-buffer-window-list inferior-rml-buffer-name nil t))
	  (select-window w)
	  (goto-char inf-point))
	(select-window owindow))
      (if err (progn (beep) (rml-overlay-region (point) end))
        (if inferior-rml-output
            (message "No error")
          (message "No output yet...")
          ))
      err)))

(defun rml-overlay-region (beg end &optional wait)
  (interactive "%r")
  (cond ((fboundp 'make-overlay)
         (if rml-error-overlay ()
           (setq rml-error-overlay (make-overlay 1 1))
           (overlay-put rml-error-overlay 'face 'region))
         (unwind-protect
             (progn
               (move-overlay rml-error-overlay beg end (current-buffer))
               (beep) (if wait (read-event) (rml-sit-for 60)))
           (delete-overlay rml-error-overlay)))))

;; wait some amount for ouput, that is, until inferior-rml-output is set
;; to true. Hence, interleaves sitting for shorts delays and checking the
;; flag. Give up after some time. Typing into the source buffer will cancel
;; waiting, i.e. may report 'No result yet'

(defun rml-wait-output (&optional before after)
  (let ((c 1))
    (rml-sit-for 0 (or before 1))
    (let ((c 1))
      (while (and (not inferior-rml-output) (< c 99) (rml-sit-for 0 c t))
        (setq c (+ c 1))))
    (rml-sit-for (or after 0) 1)))

;; To insert the last output from rml at point
(defun rml-insert-last-output ()
  "Insert the result of the evaluation of previous phrase"
  (interactive)
  (let ((pos (process-mark (get-buffer-process inferior-rml-buffer-name))))
  (insert-buffer-substring inferior-rml-buffer-name
                           rml-previous-output (- pos 2))))

;; additional bindings

;(let ((map (lookup-key rml-mode-map [menu-bar rml])))
;  (define-key map [indent-buffer] '("Indent buffer" . rml-indent-buffer))
;  (define-key map [eval-buffer] '("Eval buffer" . rml-eval-buffer))
;)
;(define-key rml-mode-map "\C-c\C-b" 'rml-eval-buffer)


(provide 'inf-rml)
