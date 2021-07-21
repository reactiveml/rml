;(***********************************************************************)
;(*                                                                     *)
;(*                           ReactiveML                                *)
;(*                                                                     *)
;(*          Damien Doligez, projet Moscova, INRIA Rocquencourt         *)
;(*                                                                     *)
;(*  Copyright 2003 Institut National de Recherche en Informatique et   *)
;(*  en Automatique.  All rights reserved.  This file is distributed    *)
;(*  under the terms of the Q Public License version 1.0.               *)
;(*                                                                     *)
;(***********************************************************************)

;(* $Id: rml-types.el,v 1.2 2005/03/14 10:00:56 mandel Exp $ *)

; An emacs-lisp complement to the "-dtypes" option of rmlc and rmlopt.

;; XEmacs compatibility

(eval-and-compile
  (if (and (boundp 'running-xemacs) running-xemacs)
      (require 'rml-xemacs)
    (require 'rml-emacs)))



(defvar rml-types-location-re nil "Regexp to parse *.tannot files.

Annotation files *.tannot may be generated with the \"-dtypes\" option
of rmlc and rmlopt.

Their format is:

  file ::= block *
  block ::= position <SP> position <LF> annotation *
  position ::= filename <SP> num <SP> num <SP> num
  annotation ::= keyword open-paren <LF> <SP> <SP> data <LF> close-paren

  <SP> is a space character (ASCII 0x20)
  <LF> is a line-feed character (ASCII 0x0A)
  num is a sequence of decimal digits
  filename is a string with the lexical conventions of ReactiveML
  open-paren is an open parenthesis (ASCII 0x28)
  close-paren is a closed parenthesis (ASCII 0x29)
  data is any sequence of characters where <LF> is always followed by
       at least two space characters.

- in each block, the two positions are respectively the start and the
- end of the range described by the block.
- in a position, the filename is the name of the file, the first num
  is the line number, the second num is the offset of the beginning
  of the line, the third num is the offset of the position itself.
- the char number within the line is the difference between the third
  and second nums.

For the moment, the only possible keyword is \"type\"."
)

(let* ((rml-types-filename-re "\"\\(\\([^\\\"]\\|\\\\.\\)*\\)\"")
       (rml-types-number-re "\\([0-9]*\\)")
       (rml-types-position-re
        (concat rml-types-filename-re " "
                rml-types-number-re " "
                rml-types-number-re " "
                rml-types-number-re)))
  (setq rml-types-location-re
        (concat "^" rml-types-position-re " " rml-types-position-re)))

(defvar rml-types-expr-ovl (make-overlay 1 1))

(make-face 'rml-types-face)
(set-face-doc-string 'rml-types-face
                     "face for hilighting expressions and types")
(if (not (face-differs-from-default-p 'rml-types-face))
    (set-face-background 'rml-types-face "#88FF44"))

(defvar rml-types-typed-ovl (make-overlay 1 1))

(make-face 'rml-types-typed-face)
(set-face-doc-string 'rml-types-typed-face
                     "face for hilighting typed expressions")
(if (not (face-differs-from-default-p 'rml-types-typed-face))
    (set-face-background 'rml-types-typed-face "#FF8844"))

(overlay-put rml-types-expr-ovl 'face 'rml-types-face)
(overlay-put rml-types-typed-ovl 'face 'rml-types-typed-face)


(defvar rml-types-annotation-tree nil)
(defvar rml-types-annotation-date nil)
(make-variable-buffer-local 'rml-types-annotation-tree)
(make-variable-buffer-local 'rml-types-annotation-date)

(defvar rml-types-buffer-name "*rml-types*"
  "Name of buffer for diplaying rml types")
(defvar rml-types-buffer nil
  "buffer for diplaying rml types")

(defun rml-types-show-type (arg)
  "Show the type of expression or pattern at point.
   The smallest expression or pattern that contains point is
   temporarily highlighted.  Its type is highlighted in the .tannot
   file and the mark is set to the beginning of the type.
   The type is also displayed in the mini-buffer.

   Hints on using the type display:
   . If you want the type of an identifier, put point within any
     occurrence of this identifier.
   . If you want the result type of a function application, put point
     at the first space after the function name.
   . If you want the type of a list, put point on a bracket, on a
     semicolon, or on the :: constructor.
   . Even if type checking fails, you can still look at the types
     in the file, up to where the type checker failed.

Types are also diplayed in the buffer *rml-types*, which buffer is
display when the commande is called with Prefix argument 4.

See also `rml-types-explore' for exploration by mouse dragging.
See `rml-types-location-re' for annotation file format.
"
  (interactive "p")
  (let* ((target-buf (current-buffer))
         (target-file (file-name-nondirectory (buffer-file-name)))
         (target-line (1+ (count-lines (point-min)
                                       (rml-line-beginning-position))))
         (target-bol (rml-line-beginning-position))
         (target-cnum (point))
         (type-file (concat (file-name-sans-extension (buffer-file-name))
                            ".tannot")))
    (rml-types-preprocess type-file)
    (unless rml-types-buffer
      (setq rml-types-buffer (get-buffer-create rml-types-buffer-name)))
    (let* ((targ-loc (vector target-file target-line target-bol target-cnum))
           (node (rml-types-find-location targ-loc ()
                                           rml-types-annotation-tree)))
      (cond
       ((null node)
        (delete-overlay rml-types-expr-ovl)
        (message "Point is not within a typechecked expression or pattern.")
        ; (with-current-buffer type-buf (narrow-to-region 1 1))
        )
       (t
        (let ((left (rml-types-get-pos target-buf (elt node 0)))
              (right (rml-types-get-pos target-buf (elt node 1)))
              (type (elt node 2)))
          (move-overlay rml-types-expr-ovl left right target-buf)
          (with-current-buffer rml-types-buffer
            (erase-buffer)
            (insert type)
            (message (format "type: %s" type)))
          ))))
    (if (and (= arg 4)
             (not (window-live-p (get-buffer-window rml-types-buffer))))
        (display-buffer rml-types-buffer))
    (unwind-protect
        (sit-for 60)
      (delete-overlay rml-types-expr-ovl)
      )))

(defun rml-types-preprocess (type-file)
  (let* ((type-date (nth 5 (file-attributes type-file)))
         (target-file (file-name-nondirectory (buffer-file-name)))
         (target-date (nth 5 (file-attributes target-file))))
    (unless (and rml-types-annotation-tree
                 type-date
                 rml-types-annotation-date
                 (not (rml-types-date< rml-types-annotation-date type-date)))
      (if (and type-date target-date (rml-types-date< type-date target-date))
          (error (format "%s is more recent than %s" target-file type-file)))
      (message "Reading annotation file...")
      (let* ((type-buf (rml-types-find-file type-file))
             (tree (with-current-buffer type-buf
                    (widen)
                    (goto-char (point-min))
                    (rml-types-build-tree target-file))))
        (setq rml-types-annotation-tree tree
              rml-types-annotation-date type-date)
        (kill-buffer type-buf)
        (message ""))
      )))

(defun rml-types-date< (date1 date2)
  (or (< (car date1) (car date2))
      (and (= (car date1) (car date2))
           (< (nth 1 date1) (nth 1 date2)))))


; we use an obarray for hash-consing the strings within each tree

(defun rml-types-make-hash-table ()
  (make-vector 255 0))

(defun rml-types-hcons (elem table)
  (symbol-name (intern elem table)))


; tree of intervals
; each node is a vector
; [ pos-left pos-right type-info child child child... ]
; type-info =
;  () if this node does not correspond to an annotated interval
;  (type-start . type-end)  address of the annotation in the .tannot file

(defun rml-types-build-tree (target-file)
  (let ((stack ())
        (accu ())
        (table (rml-types-make-hash-table))
        (type-info ()))
    (while (re-search-forward rml-types-location-re () t)
      (let ((l-file (file-name-nondirectory (match-string 1)))
            (l-line (string-to-int (match-string 3)))
            (l-bol (string-to-int (match-string 4)))
            (l-cnum (string-to-int (match-string 5)))
            (r-file (file-name-nondirectory (match-string 6)))
            (r-line (string-to-int (match-string 8)))
            (r-bol (string-to-int (match-string 9)))
            (r-cnum (string-to-int (match-string 10))))
        (unless (rml-types-not-in-file l-file r-file target-file)
          (while (and (re-search-forward "^" () t)
                      (not (looking-at "type"))
                      (not (looking-at "\\\"")))
            (forward-char 1))
          (setq type-info
                (if (looking-at
                     "^type(\n\\(  \\([^\n)]\\|.)\\|\n[^)]\\)*\\)\n)")
                    (rml-types-hcons (match-string 1) table)))
          (setq accu ())
          (while (and stack
                      (rml-types-pos-contains l-cnum r-cnum (car stack)))
            (setq accu (cons (car stack) accu))
            (setq stack (cdr stack)))
          (let* ((left-pos (vector l-file l-line l-bol l-cnum))
                 (right-pos (vector r-file r-line r-bol r-cnum))
                 (node (rml-types-make-node left-pos right-pos type-info
                                             accu)))
            (setq stack (cons node stack))))))
    (if (null stack)
        (error "no annotations found for this source file")
      (let* ((left-pos (elt (car (last stack)) 0))
             (right-pos (elt (car stack) 1)))
        (if (null (cdr stack))
            (car stack)
          (rml-types-make-node left-pos right-pos () (nreverse stack)))))))

(defun rml-types-not-in-file (l-file r-file target-file)
  (or (and (not (string= l-file target-file))
           (not (string= l-file "")))
      (and (not (string= r-file target-file))
           (not (string= r-file "")))))

(defun rml-types-make-node (left-pos right-pos type-info children)
  (let ((result (make-vector (+ 3 (length children)) ()))
        (i 3))
    (aset result 0 left-pos)
    (aset result 1 right-pos)
    (aset result 2 type-info)
    (while children
      (aset result i (car children))
      (setq children (cdr children))
      (setq i (1+ i)))
    result))

(defun rml-types-pos-contains (l-cnum r-cnum node)
  (and (<= l-cnum (elt (elt node 0) 3))
       (>= r-cnum (elt (elt node 1) 3))))

(defun rml-types-find-location (targ-pos curr node)
  (if (not (rml-types-pos-inside targ-pos node))
      curr
    (if (elt node 2)
        (setq curr node))
    (let ((i (rml-types-search node targ-pos)))
      (if (and (> i 3)
               (rml-types-pos-inside targ-pos (elt node (1- i))))
          (rml-types-find-location targ-pos curr (elt node (1- i)))
        curr))))

; trouve le premier fils qui commence apres la position
; ou (length node) si tous commencent avant
(defun rml-types-search (node pos)
  (let ((min 3)
        (max (length node))
        med)
    (while (< min max)
      (setq med (/ (+ min max) 2))
      (if (rml-types-pos<= (elt (elt node med) 0) pos)
          (setq min (1+ med))
        (setq max med)))
    min))

(defun rml-types-pos-inside (pos node)
  (let ((left-pos (elt node 0))
        (right-pos (elt node 1)))
    (and (rml-types-pos<= left-pos pos)
         (rml-types-pos> right-pos pos))))

(defun rml-types-find-interval (buf targ-pos node)
  (let ((nleft (elt node 0))
        (nright (elt node 1))
        (left ())
        (right ())
        i)
    (cond
     ((not (rml-types-pos-inside targ-pos node))
      (if (not (rml-types-pos<= nleft targ-pos))
          (setq right nleft))
      (if (not (rml-types-pos> nright targ-pos))
          (setq left nright)))
     (t
      (setq left nleft
            right nright)
      (setq i (rml-types-search node targ-pos))
      (if (< i (length node))
          (setq right (elt (elt node i) 0)))
      (if (> i 3)
          (setq left (elt (elt node (1- i)) 1)))))
     (cons (if left
               (rml-types-get-pos buf left)
             (with-current-buffer buf (point-min)))
           (if right
               (rml-types-get-pos buf right)
             (with-current-buffer buf (point-max))))))


;; Warning: these comparison functions are not symmetric.
;; The first argument determines the format:
;; when its file component is empty, only the cnum is compared.

(defun rml-types-pos<= (pos1 pos2)
  (let ((file1 (elt pos1 0))
        (line1 (elt pos1 1))
        (bol1 (elt pos1 2))
        (cnum1 (elt pos1 3))
        (file2 (elt pos2 0))
        (line2 (elt pos2 1))
        (bol2 (elt pos2 2))
        (cnum2 (elt pos2 3)))
    (if (string= file1 "")
        (<= cnum1 cnum2)
      (and (string= file1 file2)
           (or (< line1 line2)
               (and (= line1 line2)
                    (<= (- cnum1 bol1) (- cnum2 bol2))))))))

(defun rml-types-pos> (pos1 pos2)
  (let ((file1 (elt pos1 0))
        (line1 (elt pos1 1))
        (bol1 (elt pos1 2))
        (cnum1 (elt pos1 3))
        (file2 (elt pos2 0))
        (line2 (elt pos2 1))
        (bol2 (elt pos2 2))
        (cnum2 (elt pos2 3)))
    (if (string= file1 "")
        (> cnum1 cnum2)
      (and (string= file1 file2)
           (or (> line1 line2)
               (and (= line1 line2)
                    (> (- cnum1 bol1) (- cnum2 bol2))))))))

(defun rml-types-get-pos (buf pos)
  (save-excursion
    (set-buffer buf)
    (goto-line (elt pos 1))
    (forward-char (- (elt pos 3) (elt pos 2)))
    (point)))

; find-file-read-only-noselect seems to be missing from emacs...
(defun rml-types-find-file (name)
  (let (buf)
  (cond
   ((setq buf (get-file-buffer name))
    (unless (verify-visited-file-modtime buf)
      (if (buffer-modified-p buf)
          (find-file-noselect name)
        (with-current-buffer buf (revert-buffer t t)))
      ))
   ((and (file-readable-p name)
         (setq buf (find-file-noselect name)))
     (with-current-buffer buf (toggle-read-only 1))
     )
   (t
    (error "No annotation file. You should compile with option \"-dtypes\"."))
    )
  buf))

(defun rml-types-mouse-ignore (event)
  (interactive "e")
  nil)

(defun rml-types-explore (event)
  "Explore type annotations by mouse dragging.

The expression under the mouse is highlighted
and its type is displayed in the minibuffer, until the move is released."
  (interactive "e")
  (set-buffer (window-buffer (rml-event-window event)))
  (let* ((target-buf (current-buffer))
         (target-file (file-name-nondirectory (buffer-file-name)))
         (type-file (concat (file-name-sans-extension (buffer-file-name))
                            ".tannot"))
         (target-line) (target-bol)
         target-pos
         Left Right limits cnum node mes type
         region
         target-tree
         )
    (unwind-protect
        (progn
          (rml-types-preprocess type-file)
          (setq target-tree rml-types-annotation-tree)
          (unless rml-types-buffer
            (setq rml-types-buffer
                  (get-buffer-create rml-types-buffer-name)))
          ;; (message "Drag the mouse to explore types")
          (unwind-protect
              (rml-track-mouse
               (setq region
                     (rml-types-typed-make-overlay
                      target-buf (rml-event-point-start event)))
               (while (and event
                           (integer-or-marker-p
                            (setq cnum (rml-event-point-end event))))
                 (if (and region (<= (car region) cnum) (< cnum (cdr region)))
                     (if (and limits
                              (>= cnum (car limits)) (< cnum (cdr limits)))
                         (message mes)
                       (setq target-bol
                             (save-excursion
                               (goto-char cnum) (rml-line-beginning-position))
                             target-line (1+ (count-lines (point-min)
                                                          target-bol))
                             target-pos
                             (vector target-file target-line target-bol cnum))
                       (save-excursion
                         (setq node (rml-types-find-location
                                     target-pos () target-tree))
                         (set-buffer rml-types-buffer)
                         (erase-buffer)
                         (cond
                          (node
                           (setq Left
                                 (rml-types-get-pos target-buf (elt node 0))
                                 Right
                                 (rml-types-get-pos target-buf (elt node 1)))
                           (move-overlay
                            rml-types-expr-ovl Left Right target-buf)
                           (setq limits
                                 (rml-types-find-interval target-buf
                                                           target-pos node)
                                 type (elt node 2))
                           )
                          (t
                           (delete-overlay rml-types-expr-ovl)
                           (setq type "*no type information*")
                           (setq limits
                                 (rml-types-find-interval
                                  target-buf target-pos target-tree))
                           ))
                         (message (setq mes (format "type: %s" type)))
                         (insert type)
                         )))
                 (setq event (rml-read-event))
                 (unless (mouse-movement-p event) (setq event nil))
                 )
               )
            (delete-overlay rml-types-expr-ovl)
            (delete-overlay rml-types-typed-ovl)
            ))
      ;; the mouse is down. One should prevent against mouse release,
      ;; which could do something undesirable.
      ;; In most common cases, next event will be mouse release.
      ;; However, it could also be a key stroke before mouse release.
      ;; Will then execute the action for mouse release (if bound).
      ;; Emacs does not allow to test whether mouse is up or down.
      ;; Same problem may happen above while exploring
      (if (and event (rml-read-event)))
      )))

(defun rml-types-typed-make-overlay (target-buf pos)
  (interactive "p")
  (let ((start pos) (end pos) len node left right)
      (setq len (length rml-types-annotation-tree))
      (while (> len 3)
        (setq len (- len 1))
        (setq node (aref rml-types-annotation-tree len))
        (if (and (equal target-buf (current-buffer))
                 (setq left (rml-types-get-pos target-buf (elt node 0))
                       right (rml-types-get-pos target-buf (elt node 1)))
                 (<= left pos) (> right pos)
                 )
            (setq start (min start left)
                  end (max end right))
             ))
      (move-overlay rml-types-typed-ovl
                    (max (point-min) (- start 1))
                    (min (point-max) (+ end 1)) target-buf)
    (cons start end)))

(provide 'rml-types)
