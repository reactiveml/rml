;; useful colors

(cond
 ((x-display-color-p)
  (cond
   ((not (memq 'font-lock-type-face (face-list)))
    ; make the necessary faces
    (make-face 'Firebrick)
    (set-face-foreground 'Firebrick "Firebrick")
    (make-face 'RosyBrown)
    (set-face-foreground 'RosyBrown "RosyBrown")
    (make-face 'Purple)
    (set-face-foreground 'Purple "Purple")
    (make-face 'Magenta)
    (set-face-foreground 'Magenta "Magenta")
    (make-face 'MidnightBlue)
    (set-face-foreground 'MidnightBlue "MidnightBlue")
    (make-face 'DarkGoldenRod)
    (set-face-foreground 'DarkGoldenRod "DarkGoldenRod")
    (make-face 'DarkOliveGreen)
    (set-face-foreground 'DarkOliveGreen "DarkOliveGreen4")
    (make-face 'CadetBlue)
    (set-face-foreground 'CadetBlue "CadetBlue")
    ; assign them as standard faces
    (setq font-lock-comment-face 'Firebrick)
    (setq font-lock-string-face 'RosyBrown)
    (setq font-lock-keyword-face 'Purple)
    (setq font-lock-builtin-face 'Magenta)
    (setq font-lock-function-name-face 'MidnightBlue)
    (setq font-lock-variable-name-face 'DarkGoldenRod)
    (setq font-lock-type-face 'DarkOliveGreen)
    (setq font-lock-reference-face 'CadetBlue)))
  ; extra faces for documention
  (make-face 'Stop)
  (set-face-foreground 'Stop "White")
  (set-face-background 'Stop "Red")
  (make-face 'Doc)
  (set-face-foreground 'Doc "Red")
  (setq font-lock-stop-face 'Stop)
  (setq font-lock-doccomment-face 'Doc)
))

; The same definition is in rml.el:
; we don't know in which order they will be loaded.
(defvar rml-quote-char "'"
  "*Quote for character constants. \"'\" for Objective Rml, \"`\" for Rml-Light.")

(defconst rml-font-lock-keywords
  (list
;stop special comments
   '("\\(^\\|[^\"]\\)\\((\\*\\*/\\*\\*)\\)"
     2 font-lock-stop-face)
;doccomments
   '("\\(^\\|[^\"]\\)\\((\\*\\*[^*]*\\([^)*][^*]*\\*+\\)*)\\)"
     2 font-lock-doccomment-face)
;comments
   '("\\(^\\|[^\"]\\)\\((\\*[^*]*\\*+\\([^)*][^*]*\\*+\\)*)\\)"
     2 font-lock-comment-face)
;character literals
   (cons (concat rml-quote-char "\\(\\\\\\([ntbr" rml-quote-char "\\]\\|"
                 "[0-9][0-9][0-9]\\)\\|.\\)" rml-quote-char
                 "\\|\"[^\"\\]*\\(\\\\\\(.\\|\n\\)[^\"\\]*\\)*\"")
         'font-lock-string-face)
;modules and constructors
   '("`?\\<[A-Z][A-Za-z0-9_']*\\>" . font-lock-function-name-face)
;definition
   (cons (concat
          "\\<\\(a\\(nd\\|s\\|wait\\)\\|c\\(onstraint\\|lass\\)"
          "\\|default\\|ex\\(ception\\|ternal\\)\\|fun\\(ct\\(ion\\|or\\)\\)?"
          "\\|gather\\|i\\(n\\(herit\\|itializer\\)?\\|mmediate\\)\\|let"
          "\\|m\\(ethod\\|utable\\|odule\\)"
          "\\|o\\(f\\|ne\\)\\|p\\(arser\\|r\\(ivate\\|oc\\(ess\\)?\\)\\)"
          "\\|rec\\|signal\\|type"
          "\\|v\\(al\\|irtual\\)\\)\\>")
         'font-lock-type-face)
;builtin
   '("\\<\\(emit\\|last\\|nothing\\|p\\(ause\\|re\\)\\|halt\\|run\\)\\>"
    . font-lock-builtin-face)
;blocking
   '("\\<\\(begin\\|loop\\|end\\|object\\|s\\(ig\\|truct\\)\\)\\>"
     . font-lock-keyword-face)
;control
   (cons (concat
          "\\<\\(control\\|do\\(ne\\|par\\|wnto\\)?\\|else"
	  "\\|for\\|i\\(f\\|gnore\\)"
          "\\|lazy\\|match\\|new\\|or\\|present\\|t\\(hen\\|o\\|ry\\)"
          "\\|until\\|w\\(h\\(en\\|ile\\)\\|ith\\)\\)\\>"
          "\\|\|\\|->\\|&\\|#")
         'font-lock-reference-face)
   '("\\<raise\\>" . font-lock-comment-face)
;labels (and open)
   '("\\(\\([~?]\\|\\<\\)[a-z][a-zA-Z0-9_']*:\\)[^:=]" 1
     font-lock-variable-name-face)
   '("\\<\\(assert\\|open\\|include\\)\\>\\|[~?][ (]*[a-z][a-zA-Z0-9_']*"
     . font-lock-variable-name-face)))

(defconst inferior-rml-font-lock-keywords
  (append
   (list
;inferior
    '("^[#-]" . font-lock-comment-face))
   rml-font-lock-keywords))

;; font-lock commands are similar for rml-mode and inferior-rml-mode
(add-hook 'rml-mode-hook
      '(lambda ()
         (cond
          ((fboundp 'global-font-lock-mode)
           (make-local-variable 'font-lock-defaults)
           (setq font-lock-defaults
                 '(rml-font-lock-keywords nil nil ((?' . "w") (?_ . "w")))))
          (t
           (setq font-lock-keywords rml-font-lock-keywords)))
         (make-local-variable 'font-lock-keywords-only)
         (setq font-lock-keywords-only t)
         (font-lock-mode 1)))

(defun inferior-rml-mode-font-hook ()
  (cond
   ((fboundp 'global-font-lock-mode)
    (make-local-variable 'font-lock-defaults)
    (setq font-lock-defaults
          '(inferior-rml-font-lock-keywords
            nil nil ((?' . "w") (?_ . "w")))))
   (t
    (setq font-lock-keywords inferior-rml-font-lock-keywords)))
  (make-local-variable 'font-lock-keywords-only)
  (setq font-lock-keywords-only t)
  (font-lock-mode 1))

(add-hook 'inferior-rml-mode-hooks 'inferior-rml-mode-font-hook)

(provide 'rml-font)
