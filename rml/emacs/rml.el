;;; rml.el --- ReactiveML code editing commands for Emacs

;; Xavier Leroy, july 1993.

;;indentation code is Copyright (C) 1996 by Ian T Zimmerman <itz@rahul.net>
;;copying: covered by the current FSF General Public License.

;; indentation code adapted for Objective Caml by Jacques Garrigue,
;; july 1997. <garrigue@kurims.kyoto-u.ac.jp>

;; indentation code adapted for ReactiveML by Sarah Maarek,
;; july 2004.

;;user customizable variables
(defvar rml-quote-char "'"
  "*Quote for character constants. \"'\" for  ReactiveML??, \"`\" for Caml-Light.")

(defvar rml-imenu-enable nil
  "*Enable Imenu support.")

(defvar rml-mode-indentation 2
  "*Used for \\[rml-unindent-command].")

(defvar rml-lookback-limit 5000
  "*How far to look back for syntax things in rml mode.")

(defvar rml-max-indent-priority 8
  "*Bounds priority of operators permitted to affect rml indentation.

Priorities are assigned to `interesting' rml operators as follows:

        all keywords 0 to 7     8
        type, val, ... + 0      7
        :: ^                    6
        @                       5
        := <-                   4
        if                      3
        fun, let, match ...     2
        module                  1
        opening keywords        0.")

(defvar rml-apply-extra-indent 2
  "*How many spaces to add to indentation for an application in rml mode.")
(make-variable-buffer-local 'rml-apply-extra-indent)

(defvar rml-begin-indent 2
  "*How many spaces to indent from a begin keyword in rml mode.")
(make-variable-buffer-local 'rml-begin-indent)

(defvar rml-loop-indent 2
  "*How many spaces to indent from a loop keyword in rml mode.")
(make-variable-buffer-local 'rml-loop-indent)

(defvar rml-class-indent 2
  "*How many spaces to indent from a class keyword in rml mode.")
(make-variable-buffer-local 'rml-class-indent)

(defvar rml-control-indent 2
  "*How many spaces to indent from a control keyword in rml mode.")
(make-variable-buffer-local 'rml-control-indent)

(defvar rml-exception-indent 2
  "*How many spaces to indent from a exception keyword in rml mode.")
(make-variable-buffer-local 'rml-exception-indent)

(defvar rml-await-indent 2
  "*How many spaces to indent from a await keyword in rml mode.")
(make-variable-buffer-local 'rml-await-indent)

(defvar rml-await-in-indent 0
  "*How many spaces to indent from a await .. in keyword in rml mode.")
(make-variable-buffer-local 'rml-await-in-indent)

(defvar rml-do-indent 2
  "*How many spaces to indent from a do keyword in rml mode.")
(make-variable-buffer-local 'rml-do-indent)

(defvar rml-for-indent 2
  "*How many spaces to indent from a for keyword in rml mode.")
(make-variable-buffer-local 'rml-for-indent)

(defvar rml-fun-indent 2
  "*How many spaces to indent from a fun keyword in rml mode.")
(make-variable-buffer-local 'rml-fun-indent)

(defvar rml-function-indent 4
  "*How many spaces to indent from a function keyword in rml mode.")
(make-variable-buffer-local 'rml-function-indent)

(defvar rml-if-indent  2
  "*How many spaces to indent from a if keyword in rml mode.")
(make-variable-buffer-local 'rml-if-indent)

(defvar rml-if-else-indent 0
  "*How many spaces to indent from an if .. else line in rml mode.")
(make-variable-buffer-local 'rml-if-else-indent)

(defvar rml-inherit-indent 2
  "*How many spaces to indent from a inherit keyword in rml mode.")
(make-variable-buffer-local 'rml-inherit-indent)

(defvar rml-initializer-indent 2
  "*How many spaces to indent from a initializer keyword in rml mode.")
(make-variable-buffer-local 'rml-initializer-indent)

(defvar rml-include-indent 2
  "*How many spaces to indent from a include keyword in rml mode.")
(make-variable-buffer-local 'rml-include-indent)

(defvar rml-let-indent 2
  "*How many spaces to indent from a let keyword in rml mode.")
(make-variable-buffer-local 'rml-let-indent)

(defvar rml-let-in-indent 0
  "*How many spaces to indent from a let .. in keyword in rml mode.")
(make-variable-buffer-local 'rml-let-in-indent)

(defvar rml-match-indent 2
  "*How many spaces to indent from a match keyword in rml mode.")
(make-variable-buffer-local 'rml-match-indent)

(defvar rml-method-indent 2
  "*How many spaces to indent from a method keyword in rml mode.")
(make-variable-buffer-local 'rml-method-indent)

(defvar rml-module-indent 2
  "*How many spaces to indent from a module keyword in rml mode.")
(make-variable-buffer-local 'rml-module-indent)

(defvar rml-object-indent 2
  "*How many spaces to indent from a object keyword in rml mode.")
(make-variable-buffer-local 'rml-object-indent)

(defvar rml-of-indent 2
  "*How many spaces to indent from a of keyword in rml mode.")
(make-variable-buffer-local 'rml-of-indent)

(defvar rml-parser-indent 4
  "*How many spaces to indent from a parser keyword in rml mode.")
(make-variable-buffer-local 'rml-parser-indent)

(defvar rml-present-indent  2
  "*How many spaces to indent from a present keyword in rml mode.")
(make-variable-buffer-local 'rml-present-indent)

(defvar rml-present-else-indent 0
  "*How many spaces to indent from an present .. else line in rml mode.")
(make-variable-buffer-local 'rml-present-else-indent)

(defvar rml-sig-indent 2
  "*How many spaces to indent from a sig keyword in rml mode.")
(make-variable-buffer-local 'rml-sig-indent)

(defvar rml-signal-indent 2
  "*How many spaces to indent from a signal keyword in rml mode.")
(make-variable-buffer-local 'rml-signal-indent)

(defvar rml-signal-in-indent 0
  "*How many spaces to indent from a signal .. in keyword in rml mode.")
(make-variable-buffer-local 'rml-signal-in-indent)

(defvar rml-struct-indent 2
  "*How many spaces to indent from a struct keyword in rml mode.")
(make-variable-buffer-local 'rml-struct-indent)

(defvar rml-try-indent 2
  "*How many spaces to indent from a try keyword in rml mode.")
(make-variable-buffer-local 'rml-try-indent)

(defvar rml-type-indent 4
  "*How many spaces to indent from a type keyword in rml mode.")
(make-variable-buffer-local 'rml-type-indent)

(defvar rml-val-indent 2
  "*How many spaces to indent from a val keyword in rml mode.")
(make-variable-buffer-local 'rml-val-indent)

(defvar rml-while-indent 2
  "*How many spaces to indent from a while keyword in rml mode.")
(make-variable-buffer-local 'rml-while-indent)

(defvar rml-::-indent  2
  "*How many spaces to indent from a :: operator in rml mode.")
(make-variable-buffer-local 'rml-::-indent)

(defvar rml-@-indent   2
  "*How many spaces to indent from a @ operator in rml mode.")
(make-variable-buffer-local 'rml-@-indent)

(defvar rml-:=-indent  2
  "*How many spaces to indent from a := operator in rml mode.")
(make-variable-buffer-local 'rml-:=-indent)

(defvar rml-<--indent  2
  "*How many spaces to indent from a <- operator in rml mode.")
(make-variable-buffer-local 'rml-<--indent)

(defvar rml-->-indent  2
  "*How many spaces to indent from a -> operator in rml mode.")
(make-variable-buffer-local 'rml-->-indent)

(defvar rml-lb-indent 2
  "*How many spaces to indent from a \[ operator in rml mode.")
(make-variable-buffer-local 'rml-lb-indent)

(defvar rml-lc-indent 2
  "*How many spaces to indent from a \{ operator in rml mode.")
(make-variable-buffer-local 'rml-lc-indent)

(defvar rml-lp-indent  1
  "*How many spaces to indent from a \( operator in rml mode.")
(make-variable-buffer-local 'rml-lp-indent)

(defvar rml-and-extra-indent nil
  "*Extra indent for rml lines starting with the and keyword.
Usually negative. nil is align on master.")
(make-variable-buffer-local 'rml-and-extra-indent)

(defvar rml-do-extra-indent nil
  "*Extra indent for rml lines starting with the do keyword.
Usually negative. nil is align on master.")
(make-variable-buffer-local 'rml-do-extra-indent)

(defvar rml-dopar-extra-indent nil
  "*Extra indent for rml lines starting with the dopar keyword.
Usually negative. nil is align on master.")
(make-variable-buffer-local 'rml-dopar-extra-indent)

(defvar rml-done-extra-indent nil
  "*Extra indent for rml lines starting with the done keyword.
Usually negative. nil is align on master.")
(make-variable-buffer-local 'rml-done-extra-indent)

(defvar rml-else-extra-indent nil
  "*Extra indent for rml lines starting with the else keyword.
Usually negative. nil is align on master.")
(make-variable-buffer-local 'rml-else-extra-indent)

(defvar rml-end-extra-indent nil
  "*Extra indent for rml lines starting with the end keyword.
Usually negative. nil is align on master.")
(make-variable-buffer-local 'rml-end-extra-indent)

(defvar rml-in-extra-indent nil
  "*Extra indent for rml lines starting with the in keyword.
Usually negative. nil is align on master.")
(make-variable-buffer-local 'rml-in-extra-indent)

(defvar rml-then-extra-indent nil
  "*Extra indent for rml lines starting with the then keyword.
Usually negative. nil is align on master.")
(make-variable-buffer-local 'rml-then-extra-indent)

(defvar rml-to-extra-indent -1
  "*Extra indent for rml lines starting with the to keyword.
Usually negative. nil is align on master.")
(make-variable-buffer-local 'rml-to-extra-indent)

(defvar rml-until-extra-indent nil
  "*Extra indent for rml lines starting with the until keyword.
Usually negative. nil is align on master.")
(make-variable-buffer-local 'rml-until-extra-indent)

(defvar rml-when-extra-indent nil
  "*Extra indent for rml lines starting with the when keyword.
Usually negative. nil is align on master.")
(make-variable-buffer-local 'rml-when-extra-indent)

(defvar rml-with-extra-indent nil
  "*Extra indent for rml lines starting with the with keyword.
Usually negative. nil is align on master.")
(make-variable-buffer-local 'rml-with-extra-indent)

(defvar rml-comment-indent 3
  "*Indent inside comments.")
(make-variable-buffer-local 'rml-comment-indent)

(defvar rml-|-extra-indent -2
  "*Extra indent for rml lines starting with the | operator.
Usually negative. nil is align on master.")
(make-variable-buffer-local 'rml-|-extra-indent)

(defvar rml-rb-extra-indent -2
  "*Extra indent for rml lines statring with ].
Usually negative. nil is align on master.")

(defvar rml-rc-extra-indent -2
  "*Extra indent for rml lines starting with }.
Usually negative. nil is align on master.")

(defvar rml-rp-extra-indent -1
  "*Extra indent for rml lines starting with ).
Usually negative. nil is align on master.")

(defvar rml-electric-indent t
  "*Non-nil means electrically indent lines starting with |, ] or }.

Many people find eletric keys irritating, so you can disable them if
you are one.")

(defvar rml-electric-close-vector t
  "*Non-nil means electrically insert a | before a vector-closing ].

Many people find eletric keys irritating, so you can disable them if
you are one. You should probably have this on, though, if you also
have rml-electric-indent on, which see.")

;;code
(if (or (not (fboundp 'indent-line-to))
        (not (fboundp 'buffer-substring-no-properties)))
    (require 'rml-compat))

(defvar rml-shell-active nil
  "Non nil when a subshell is running.")

(defvar running-xemacs  (string-match "XEmacs" emacs-version)
  "Non-nil if we are running in the XEmacs environment.")

(defvar rml-mode-map nil
  "Keymap used in Rml mode.")
(if rml-mode-map
    ()
  (setq rml-mode-map (make-sparse-keymap))
  (define-key rml-mode-map "|" 'rml-electric-pipe)
  (define-key rml-mode-map "}" 'rml-electric-pipe)
  (define-key rml-mode-map "]" 'rml-electric-rb)
  (define-key rml-mode-map "\t" 'rml-indent-command)
  (define-key rml-mode-map [backtab] 'rml-unindent-command)

;itz 04-21-96 instead of defining a new function, use defadvice
;that way we get out effect even when we do \C-x` in compilation buffer
;  (define-key rml-mode-map "\C-x`" 'rml-next-error)

  (if running-xemacs
      (define-key rml-mode-map 'backspace 'backward-delete-char-untabify)
    (define-key rml-mode-map "\177" 'backward-delete-char-untabify))

  ;; rml-types
  (define-key rml-mode-map [?\C-c?\C-t] 'rml-types-show-type)
  ;; rml-static
  (define-key rml-mode-map [?\C-c?\C-s] 'rml-static-show-type)
  ;; to prevent misbehavior in case of error during exploration.
;  (define-key rml-mode-map [mouse-2] 'rml-types-mouse-ignore)
;  (define-key rml-mode-map [down-mouse-2] 'rml-types-explore)
  ;; rml-help
  (define-key rml-mode-map [?\C-c?i] 'rml-add-path)
  (define-key rml-mode-map [?\C-c?]] 'rml-close-module)
  (define-key rml-mode-map [?\C-c?[] 'rml-open-module)
  (define-key rml-mode-map [?\C-c?\C-h] 'rml-help)
  (define-key rml-mode-map [?\C-c?\t] 'rml-complete)
  ;; others
  (define-key rml-mode-map "\C-cb" 'rml-insert-begin-form)
  (define-key rml-mode-map "\C-cc" 'rml-insert-control-form)
  (define-key rml-mode-map "\C-co" 'rml-insert-loop-form)
  (define-key rml-mode-map "\C-ca" 'rml-insert-await-form)
  (define-key rml-mode-map "\C-cu" 'rml-insert-do-until-form)
  (define-key rml-mode-map "\C-ce" 'rml-insert-do-when-form)
  (define-key rml-mode-map "\C-cf" 'rml-insert-for-form)
  (define-key rml-mode-map "\C-ci" 'rml-insert-if-form)
  (define-key rml-mode-map "\C-cl" 'rml-insert-let-form)
  (define-key rml-mode-map "\C-cm" 'rml-insert-match-form)
  (define-key rml-mode-map "\C-cp" 'rml-insert-present-form)
  (define-key rml-mode-map "\C-cs" 'rml-insert-signal-form)
  (define-key rml-mode-map "\C-ct" 'rml-insert-try-form)
  (define-key rml-mode-map "\C-cw" 'rml-insert-while-form)
  (define-key rml-mode-map "\C-c\C-a" 'rml-find-alternate-file)
  (define-key rml-mode-map "\C-c\C-c" 'compile)
  (define-key rml-mode-map "\C-c\C-\[" 'rml-backward-to-less-indent)
  (define-key rml-mode-map "\C-c\C-\]" 'rml-forward-to-less-indent)
  (define-key rml-mode-map "\C-c\C-q" 'rml-indent-phrase)
  (define-key rml-mode-map "\C-c\C-r" 'rml-eval-region)
;  (define-key rml-mode-map "\C-c\C-s" 'rml-show-subshell)
  (define-key rml-mode-map "\M-\C-h" 'rml-mark-phrase)
  (define-key rml-mode-map "\M-\C-q" 'rml-indent-phrase)
  (define-key rml-mode-map "\M-\C-x" 'rml-eval-phrase)

  (if running-xemacs nil ; if not running xemacs
    (let ((map (make-sparse-keymap "ReactiveML"))
          (forms (make-sparse-keymap "Forms")))
      (define-key rml-mode-map "\C-c\C-d" 'rml-show-imenu)
      (define-key rml-mode-map [menu-bar] (make-sparse-keymap))
      (define-key rml-mode-map [menu-bar rml] (cons "ReactiveML" map))
      ;; rml-help

      (define-key map [open] '("Open add path" . rml-add-path ))
      (define-key map [close]
         '("Close module for help" . rml-close-module))
      (define-key map [open] '("Open module for help" . rml-open-module))
      (define-key map [help] '("Help for identifier" . rml-help))
      (define-key map [complete] '("Complete identifier" . rml-complete))
      (define-key map [separator-help] '("---"))

      ;; rml-types
      (define-key map [show-type]
          '("Show type at point" . rml-types-show-type ))
;      (define-key map [separator-types] '("---"))

      ;; rml-static
      (define-key map [show-static]
          '("Show static at point" . rml-static-show-type ))
      (define-key map [separator-types] '("---"))

      ;; others
      (define-key map [run-rml] '("Start subshell..." . run-rml))
      (define-key map [compile] '("Compile..." . compile))
      (define-key map [switch-view]
        '("Switch view" . rml-find-alternate-file))
      (define-key map [separator-format] '("--"))
      (define-key map [forms] (cons "Forms" forms))
      (define-key map [show-imenu] '("Show index" . rml-show-imenu))
      (put 'rml-show-imenu 'menu-enable '(not rml-imenu-shown))
      (define-key map [show-subshell] '("Show subshell" . rml-show-subshell))
      (put 'rml-show-subshell 'menu-enable 'rml-shell-active)
      (define-key map [eval-phrase] '("Eval phrase" . rml-eval-phrase))
      (put 'rml-eval-phrase 'menu-enable 'rml-shell-active)
      (define-key map [indent-phrase] '("Indent phrase" . rml-indent-phrase))
      (define-key forms [while]
        '("while .. do .. done" . rml-insert-while-form))
      (define-key forms [control] '("control .. with .." . rml-insert-control-form))
      (define-key forms [try] '("try .. with .." . rml-insert-try-form))
      (define-key forms [until] '("do .. until .." . rml-insert-do-until-form))
      (define-key forms [when] '("do .. when .." . rml-insert-do-when-form))
      (define-key forms [match] '("match .. with .." . rml-insert-match-form))
      (define-key forms [await] '("await .. in .." . rml-insert-await-form))
      (define-key forms [let] '("let .. in .." . rml-insert-let-form))
      (define-key forms [signal] '("signal .. in .." . rml-insert-signal-form))
      (define-key forms [if] '("if .. then .. else .." . rml-insert-if-form))
      (define-key forms [present]
	'("present .. then .. else .." . rml-insert-present-form))
      (define-key forms [begin] '("for .. do .. done" . rml-insert-for-form))
      (define-key forms [begin] '("begin .. end" . rml-insert-begin-form))
      (define-key forms [loop] '("loop .. end" . rml-insert-loop-form)))))

(defvar rml-mode-xemacs-menu
  (if running-xemacs
      '("ReactiveML"
        [ "Indent phrase" rml-indent-phrase :keys "C-M-q" ]
        [ "Eval phrase" rml-eval-phrase
          :active rml-shell-active :keys "C-M-x" ]
        [ "Show subshell" rml-show-subshell rml-shell-active ]
        ("Forms"
         [ "while .. do .. done" rml-insert-while-form t]
         [ "control .. with .." rml-insert-control-form t ]
	 [ "try .. with .." rml-insert-try-form t ]
         [ "do .. until .." rml-insert-do-until-form t ]
         [ "do .. when .." rml-insert-do-when-form t ]
         [ "match .. with .." rml-insert-match-form t ]
         [ "await .. in .." rml-insert-await-form t ]
         [ "let .. in .." rml-insert-let-form t ]
	 [ "signal .. in .." rml-insert-signal-form t ]
         [ "if .. then .. else .." rml-insert-if-form t ]
         [ "present .. then .. else .." rml-insert-present-form t ]
         [ "for .. do .. done" rml-insert-for-form t ]
         [ "begin .. end" rml-insert-begin-form t ]
	 [ "loop .. end" rml-insert-loop-form t ])
        "---"
        [ "Switch view" rml-find-alternate-file t ]
        [ "Compile..." compile t ]
        [ "Start subshell..." run-rml t ]
        "---"
        [ "Show type at point" rml-types-show-type t ]
        [ "Show static at point" rml-static-show-type t ]
        "---"
        [ "Complete identifier" rml-complete t ]
        [ "Help for identifier" rml-help t ]
        [ "Add path for documentation" rml-add-path t ]
        [ "Open module for documentation" rml-open t ]
        [ "Close module for documentation" rml-close t ]
        ))
  "Menu to add to the menubar when running Xemacs")

(defvar rml-mode-syntax-table nil
  "Syntax table in use in Rml mode buffers.")
(if rml-mode-syntax-table
    ()
  (setq rml-mode-syntax-table (make-syntax-table))
  ; backslash is an escape sequence
  (modify-syntax-entry ?\\ "\\" rml-mode-syntax-table)
  ; ( is first character of comment start
  (modify-syntax-entry ?\( "()1" rml-mode-syntax-table)
  ; * is second character of comment start,
  ; and first character of comment end
  (modify-syntax-entry ?*  ". 23" rml-mode-syntax-table)
  ; ) is last character of comment end
  (modify-syntax-entry ?\) ")(4" rml-mode-syntax-table)
  ; backquote was a string-like delimiter (for character literals)
  ; (modify-syntax-entry ?` "\"" rml-mode-syntax-table)
  ; quote and underscore are part of words
  (modify-syntax-entry ?' "w" rml-mode-syntax-table)
  (modify-syntax-entry ?_ "w" rml-mode-syntax-table)
  ; ISO-latin accented letters and EUC kanjis are part of words
  (let ((i 160))
    (while (< i 256)
      (modify-syntax-entry i "w" rml-mode-syntax-table)
      (setq i (1+ i)))))

(defvar rml-mode-abbrev-table nil
  "Abbrev table used for Rml mode buffers.")
(if rml-mode-abbrev-table nil
  (setq rml-mode-abbrev-table (make-abbrev-table))
  (define-abbrev rml-mode-abbrev-table "and" "and" 'rml-abbrev-hook)
  (define-abbrev rml-mode-abbrev-table "do" "do" 'rml-abbrev-hook)
  (define-abbrev rml-mode-abbrev-table "dopar" "dopar" 'rml-abbrev-hook)
  (define-abbrev rml-mode-abbrev-table "done" "done" 'rml-abbrev-hook)
  (define-abbrev rml-mode-abbrev-table "else" "else" 'rml-abbrev-hook)
  (define-abbrev rml-mode-abbrev-table "end" "end" 'rml-abbrev-hook)
  (define-abbrev rml-mode-abbrev-table "in" "in" 'rml-abbrev-hook)
  (define-abbrev rml-mode-abbrev-table "then" "then" 'rml-abbrev-hook)
  (define-abbrev rml-mode-abbrev-table "until" "until" 'rml-abbrev-hook)
  (define-abbrev rml-mode-abbrev-table "when" "when" 'rml-abbrev-hook)
  (define-abbrev rml-mode-abbrev-table "with" "with" 'rml-abbrev-hook))

;; Other internal variables

(defvar rml-last-noncomment-pos nil
  "Caches last buffer position determined not inside a rml comment.")
(make-variable-buffer-local 'rml-last-noncomment-pos)

;;last-noncomment-pos can be a simple position, because we nil it
;;anyway whenever buffer changes upstream. last-comment-start and -end
;;have to be markers, because we preserve them when the changes' end
;;doesn't overlap with the comment's start.

(defvar rml-last-comment-start nil
  "A marker caching last determined rml comment start.")
(make-variable-buffer-local 'rml-last-comment-start)

(defvar rml-last-comment-end nil
  "A marker caching last determined rml comment end.")
(make-variable-buffer-local 'rml-last-comment-end)

(make-variable-buffer-local 'before-change-function)

(defvar rml-imenu-shown nil
  "True if we have computed definition list.")
(make-variable-buffer-local 'rml-imenu-shown)

(defconst rml-imenu-search-regexp
  (concat "\\<in\\>\\|"
          "^[ \t]*\\(await immediate\\|await\\|class"
	  "\\|let\\|signal\\|type\\|m\\(odule\\|ethod\\)"
          "\\|functor\\|and\\|val\\)[ \t]+"
          "\\(\\('[a-zA-Z0-9]+\\|([^)]+)"
          "\\|mutable\\|private\\|process\\|rec process"
	  "\\|rec\\|type\\)[ \t]+\\)?"
          "\\([a-zA-Z][a-zA-Z0-9_']*\\)"))

;;; The major mode


;;
(defvar rml-mode-hook nil
  "Hook for rml-mode")

(defun rml-mode ()
  "Major mode for editing Rml code.

\\{rml-mode-map}"

  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'rml-mode)
  (setq mode-name "ReactiveML")
  (use-local-map rml-mode-map)
  (set-syntax-table rml-mode-syntax-table)
  (setq local-abbrev-table rml-mode-abbrev-table)
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
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'rml-indent-command)
  ;itz Fri Sep 25 13:23:49 PDT 1998
  (make-local-variable 'add-log-current-defun-function)
  (setq add-log-current-defun-function 'rml-current-defun)
  ;itz 03-25-96
  (setq before-change-function 'rml-before-change-function)
  (setq rml-last-noncomment-pos nil)
  (setq rml-last-comment-start (make-marker))
  (setq rml-last-comment-end (make-marker))
  ;garrigue 27-11-96
  (setq case-fold-search nil)
  ;garrigue july 97
  (if running-xemacs ; from Xemacs lisp mode
      (if (and (featurep 'menubar)
               current-menubar)
          (progn
            ;; make a local copy of the menubar, so our modes don't
            ;; change the global menubar
            (set-buffer-menubar current-menubar)
            (add-submenu nil rml-mode-xemacs-menu)))
    ;imenu support (not for Xemacs)
    (make-local-variable 'imenu-create-index-function)
    (setq imenu-create-index-function 'rml-create-index-function)
    (make-local-variable 'imenu-generic-expression)
    (setq imenu-generic-expression rml-imenu-search-regexp)
    (if (and rml-imenu-enable (< (buffer-size) 10000))
        (rml-show-imenu)))
  (run-hooks 'rml-mode-hook))

(defun rml-set-compile-command ()
  "Hook to set compile-command locally, unless there is a Makefile in the
   current directory."
  (interactive)
  (unless (or (null buffer-file-name)
              (file-exists-p "makefile")
              (file-exists-p "Makefile"))
    (let* ((filename (file-name-nondirectory buffer-file-name))
           (basename (file-name-sans-extension filename))
           (command nil))
      (cond
       ((string-match ".*\\.rmli\$" filename)
        (setq command "rmlc -c"))
       ((string-match ".*\\.rml\$" filename)
        (setq command "rmlc")
        )
       )
      (if command
          (progn
            (make-local-variable 'compile-command)
            (setq compile-command (concat command " " filename))))
      )))

(add-hook 'rml-mode-hook 'rml-set-compile-command)

;;; Auxiliary function. Garrigue 96-11-01.

(defun rml-find-alternate-file ()
  (interactive)
  (let ((name (buffer-file-name)))
    (if (string-match "^\\(.*\\)\\.\\(rml\\|rmli\\)$" name)
        (find-file
         (concat
          (rml-match-string 1 name)
          (if (string= "rml" (rml-match-string 2 name)) ".rmli" ".rml"))))))

;;; subshell support

(defun rml-eval-region (start end)
  "Send the current region to the inferior Rml process."
  (interactive"r")
  (require 'inf-rml)
  (inferior-rml-eval-region start end))

;; old version ---to be deleted later
;
; (defun rml-eval-phrase ()
;   "Send the current Rml phrase to the inferior Rml process."
;   (interactive)
;   (save-excursion
;     (let ((bounds (rml-mark-phrase)))
;     (inferior-rml-eval-region (car bounds) (cdr bounds)))))

(defun rml-eval-phrase (arg &optional min max)
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
  (require 'inf-rml)
  (inferior-rml-eval-phrase arg min max))

(defun rml-eval-buffer (arg)
  "Evaluate the buffer from the beginning to the phrase under the point.
With prefix arg, evaluate past the whole buffer, no stopping at
the current point."
  (interactive "p")
  (let ((here (point)) err)
    (goto-char (point-min))
    (setq err
          (rml-eval-phrase 500 (point-min) (if arg (point-max) here)))
    (if err (set-mark err))
    (goto-char here)))

(defun rml-show-subshell ()
  (interactive)
  (require 'inf-rml)
  (inferior-rml-show-subshell))



;;; Imenu support
(defun rml-show-imenu ()
  (interactive)
  (require 'imenu)
  (switch-to-buffer (current-buffer))
  (imenu-add-to-menubar "Defs")
  (setq rml-imenu-shown t))

(defun rml-prev-index-position-function ()
  (let (found found2 data pos)
    (while (and (setq found
                      (re-search-backward rml-imenu-search-regexp nil 'move))
		(progn (setq data (match-data)) t)
                (or (rml-in-literal-p)
                    (rml-in-comment-p)
                    (if (looking-at "in") (rml-find-in-match)))))
    (set-match-data data)
    found))
(defun rml-create-index-function ()
  (let (value-alist
        type-alist
        class-alist
        method-alist
        module-alist
        and-alist
        all-alist
        menu-alist
        (prev-pos (point-max))
        index)
    (goto-char prev-pos)
    (imenu-progress-message prev-pos 0 t)
    ;; collect definitions
    (while (rml-prev-index-position-function)
      (setq index (cons (rml-match-string 5) (point)))
      (imenu-progress-message prev-pos nil t)
      (setq all-alist (cons index all-alist))
      (cond
       ((looking-at "[ \t]*and")
        (setq and-alist (cons index and-alist)))
       ((looking-at "[ \t]*let")
        (setq value-alist (cons index (append and-alist value-alist)))
        (setq and-alist nil))
       ((looking-at "[ \t]*type")
        (setq type-alist (cons index (append and-alist type-alist)))
        (setq and-alist nil))
       ((looking-at "[ \t]*class")
        (setq class-alist (cons index (append and-alist class-alist)))
        (setq and-alist nil))
       ((looking-at "[ \t]*val")
        (setq value-alist (cons index value-alist)))
       ((looking-at "[ \t]*\\(module\\|functor\\)")
        (setq module-alist (cons index module-alist)))
       ((looking-at "[ \t]*method")
        (setq method-alist (cons index method-alist)))))
    ;; build menu
    (mapcar
     '(lambda (pair)
        (if (symbol-value (cdr pair))
            (setq menu-alist
                  (cons
                   (cons (car pair)
                         (sort (symbol-value (cdr pair)) 'imenu--sort-by-name))
                   menu-alist))))
     '(("Values" . value-alist)
       ("Types" . type-alist)
       ("Modules" . module-alist)
       ("Methods" . method-alist)
       ("Classes" . class-alist)))
    (if all-alist (setq menu-alist (cons (cons "Index" all-alist) menu-alist)))
    (imenu-progress-message prev-pos 100 t)
    menu-alist))

;;; Indentation stuff

(defun rml-in-indentation ()
  "Tests whether all characters between beginning of line and point
are blanks."
  (save-excursion
    (skip-chars-backward " \t")
    (bolp)))

;;; The command
;;; Sorry, I didn't like the previous behaviour... Garrigue 96/11/01

(defun rml-indent-command (&optional p)
  "Indent the current line in Rml mode.

Compute new indentation based on rml syntax. If prefixed, indent
the line all the way to where point is."

  (interactive "*p")
  (cond
   ((and p (> p 1)) (indent-line-to (current-column)))
   ((rml-in-indentation) (indent-line-to (rml-compute-final-indent)))
   (t (save-excursion
        (indent-line-to
         (rml-compute-final-indent))))))

(defun rml-unindent-command ()

  "Decrease indentation by one level in Rml mode.

Works only if the point is at the beginning of an indented line
\(i.e. all characters between beginning of line and point are
blanks\).  Does nothing otherwise. The unindent size is given by the
variable rml-mode-indentation."

  (interactive "*")
  (let* ((begline
          (save-excursion
            (beginning-of-line)
            (point)))
         (current-offset
          (- (point) begline)))
    (if (and (>= current-offset rml-mode-indentation)
             (rml-in-indentation))
        (backward-delete-char-untabify rml-mode-indentation))))

;;;
;;; Error processing
;;;

;; Error positions are given in bytes, not in characters
;; This function switches to monobyte mode

(if (not (fboundp 'char-bytes))
    (defalias 'forward-byte 'forward-char)
  (defun rml-char-bytes (ch)
    (let ((l (char-bytes ch)))
      (if (> l 1) (- l 1) l)))
  (defun forward-byte (count)
    (if (> count 0)
        (while (> count 0)
          (let ((char (char-after)))
            (if (null char)
                (setq count 0)
              (setq count (- count (rml-char-bytes (char-after))))
              (forward-char))))
      (while (< count 0)
        (let ((char (char-after)))
          (if (null char)
              (setq count 0)
            (setq count (+ count (rml-char-bytes (char-before))))
            (backward-char))))
    )))

(require 'compile)

;; In Emacs 19, the regexps in compilation-error-regexp-alist do not
;; match the error messages when the language is not English.
;; Hence we add a regexp.

(defconst rml-error-regexp
  "^[A-\377]+ \"\\([^\"\n]+\\)\", [A-\377]+ \\([0-9]+\\)[-,:]"
  "Regular expression matching the error messages produced by rmlc.")

(if (boundp 'compilation-error-regexp-alist)
    (or (assoc rml-error-regexp
               compilation-error-regexp-alist)
        (setq compilation-error-regexp-alist
              (cons (list rml-error-regexp 1 2)
               compilation-error-regexp-alist))))

;; A regexp to extract the range info

(defconst rml-error-chars-regexp
  ".*, .*, [A-\377]+ \\([0-9]+\\)-\\([0-9]+\\):"
  "Regular expression extracting the character numbers
from an error message produced by rmlc.")

;; Wrapper around next-error.

(defvar rml-error-overlay nil)

;;itz 04-21-96 somebody didn't get the documetation for next-error
;;right. When the optional argument is a number n, it should move
;;forward n errors, not reparse.

;itz 04-21-96 instead of defining a new function, use defadvice
;that way we get our effect even when we do \C-x` in compilation buffer

(defadvice next-error (after rml-next-error activate)
 "Reads the extra positional information provided by the Rml compiler.

Puts the point and the mark exactly around the erroneous program
fragment. The erroneous fragment is also temporarily highlighted if
possible."

 (if (eq major-mode 'rml-mode)
     (let (bol beg end)
       (save-excursion
         (set-buffer
          (if (boundp 'compilation-last-buffer)
              compilation-last-buffer   ;Emacs 19
            "*compilation*"))           ;Emacs 18
         (save-excursion
           (goto-char (window-point (get-buffer-window (current-buffer))))
           (if (looking-at rml-error-chars-regexp)
               (setq beg
                     (string-to-int
                      (buffer-substring (match-beginning 1) (match-end 1)))
                     end
                     (string-to-int
                      (buffer-substring (match-beginning 2) (match-end 2)))))))
       (cond (beg
              (setq end (- end beg))
              (beginning-of-line)
              (forward-byte beg)
              (setq beg (point))
              (forward-byte end)
              (setq end (point))
              (goto-char beg)
              (push-mark end t)
              (cond ((fboundp 'make-overlay)
                     (if rml-error-overlay ()
                       (setq rml-error-overlay (make-overlay 1 1))
                       (overlay-put rml-error-overlay 'face 'region))
                     (unwind-protect
                         (progn
                           (move-overlay rml-error-overlay
                                         beg end (current-buffer))
                           (sit-for 60))
                       (delete-overlay rml-error-overlay)))))))))

;; Usual match-string doesn't work properly with font-lock-mode
;; on some emacs.

(defun rml-match-string (num &optional string)

  "Return string of text matched by last search, without properties.

NUM specifies which parenthesized expression in the last regexp.
Value is nil if NUMth pair didn't match, or there were less than NUM
pairs.  Zero means the entire text matched by the whole regexp or
whole string."

  (let* ((data (match-data))
         (begin (nth (* 2 num) data))
         (end (nth (1+ (* 2 num)) data)))
    (if string (substring string begin end)
      (buffer-substring-no-properties begin end))))


;;; Phrases

;itz the heuristics used to see if we're `between two phrases'
;didn't seem right to me.

(defconst rml-phrase-start-keywords
  (concat "\\<\\(class\\|ex\\(ternal\\|ception\\)\\|functor"
          "\\|await\\|let\\|signal\\|module\\|open\\|type\\|val\\)\\>")
  "Keywords starting phrases in files")

;; a phrase starts when a toplevel keyword is at the beginning of a line
(defun rml-at-phrase-start-p ()
  (and (bolp)
       (or (looking-at "#")
           (looking-at rml-phrase-start-keywords))))

(defun rml-skip-comments-forward ()
  (skip-chars-forward " \n\t")
  (while (or (looking-at comment-start-skip) (rml-in-comment-p))
    (if (= (following-char) ?\)) (forward-char)
      (search-forward comment-end))
    (skip-chars-forward " \n\t")))

(defun rml-skip-comments-backward ()
  (skip-chars-backward " \n\t")
  (while (and (eq (preceding-char) ?\)) (eq (char-after (- (point) 2)) ?*))
    (backward-char)
    (while (rml-in-comment-p) (search-backward comment-start))
    (skip-chars-backward " \n\t")))

(defconst rml-phrase-sep-keywords (concat ";;\\|" rml-phrase-start-keywords))

(defun rml-find-phrase (&optional min-pos max-pos)
  "Find the RML phrase containing the point.
Return the position of the beginning of the phrase, and move point
to the end.
"
  (interactive)
  (if (not min-pos) (setq min-pos (point-min)))
  (if (not max-pos) (setq max-pos (point-max)))
  (let (beg end use-semi kwop)
    ;(rml-skip-comments-backward)
    (cond
     ; shall we have special processing for semicolons?
     ;((and (eq (char-before (- (point) 1)) ?\;) (eq (char-before) ?\;))
     ; (forward-char)
     ; (rml-skip-comments-forward)
     ; (setq beg (point))
     ; (while (and (search-forward ";;" max-pos 'move)
     ;    (or (rml-in-comment-p) (rml-in-literal-p)))))
     (t
      (rml-skip-comments-forward)
      (if (rml-at-phrase-start-p) (forward-char))
      (while (and (cond
                   ((re-search-forward rml-phrase-sep-keywords max-pos 'move)
                    (goto-char (match-beginning 0)) t))
                  (or (not (or (bolp) (looking-at ";;")))
                      (rml-in-comment-p)
                      (rml-in-literal-p)))
        (forward-char))
      (setq end (+ (point) (if (looking-at ";;") 2 0)))
      (while (and
              (setq kwop (rml-find-kwop rml-phrase-sep-keywords min-pos))
              (not (string= kwop ";;"))
              (not (bolp))))
      (if (string= kwop ";;") (forward-char 2))
      (if (not kwop) (goto-char min-pos))
      (rml-skip-comments-forward)
      (setq beg (point))
      (if (>= beg end) (error "no phrase before point"))
      (goto-char end)))
    (rml-skip-comments-forward)
    beg))

(defun rml-mark-phrase (&optional min-pos max-pos)
  "Put mark at end of this Rml phrase, point at beginning.
"
  (interactive)
  (let* ((beg (rml-find-phrase min-pos max-pos)) (end (point)))
    (push-mark)
    (goto-char beg)
    (cons beg end)))

;;itz Fri Sep 25 12:58:13 PDT 1998 support for adding change-log entries
(defun rml-current-defun ()
  (save-excursion
    (rml-mark-phrase)
    (if (not (looking-at rml-phrase-start-keywords)) nil
      (re-search-forward rml-phrase-start-keywords)
      (let ((done nil))
        (while (not done)
          (cond
           ((looking-at "\\s ")
            (skip-syntax-forward " "))
           ((char-equal (following-char) ?\( )
            (forward-sexp 1))
           ((char-equal (following-char) ?')
            (skip-syntax-forward "w_"))
           (t (setq done t)))))
      (re-search-forward "\\(\\sw\\|\\s_\\)+")
      (match-string 0))))

(defun rml-overlap (b1 e1 b2 e2)
  (<= (max b1 b2) (min e1 e2)))

;this clears the last comment cache if necessary
(defun rml-before-change-function (begin end)
  (if (and rml-last-noncomment-pos
           (> rml-last-noncomment-pos begin))
      (setq rml-last-noncomment-pos nil))
  (if (and (marker-position rml-last-comment-start)
           (marker-position rml-last-comment-end)
           (rml-overlap begin end
                         rml-last-comment-start
                         rml-last-comment-end))
      (prog2
          (set-marker rml-last-comment-start nil)
          (set-marker rml-last-comment-end nil)))
  (let ((orig-function (default-value 'before-change-function)))
    (if orig-function (funcall orig-function begin end))))

(defun rml-in-literal-p ()
  "Returns non-nil if point is inside a rml literal."
  (let* ((start-literal (concat "[\"" rml-quote-char "]"))
         (char-literal
          (concat "\\([^\\]\\|\\\\\\.\\|\\\\[0-9][0-9][0-9]\\)"
                  rml-quote-char))
         (pos (point))
         (eol (progn (end-of-line 1) (point)))
         state in-str)
    (beginning-of-line 1)
    (while (and (not state)
                (re-search-forward start-literal eol t)
                (<= (point) pos))
      (cond
       ((string= (rml-match-string 0) "\"")
        (setq in-str t)
        (while (and in-str (not state)
                    (re-search-forward "\"\\|\\\\\"" eol t))
          (if (> (point) pos) (setq state t))
          (if (string= (rml-match-string 0) "\"") (setq in-str nil)))
        (if in-str (setq state t)))
       ((looking-at char-literal)
        (if (and (>= pos (match-beginning 0)) (< pos (match-end 0)))
            (setq state t)
          (goto-char (match-end 0))))))
    (goto-char pos)
    state))

(defun rml-forward-comment ()
  "Skip one (eventually nested) comment."
  (let ((count 1) match)
    (while (> count 0)
      (if (not (re-search-forward "(\\*\\|\\*)" nil 'move))
          (setq count -1)
        (setq match (rml-match-string 0))
        (cond
         ((rml-in-literal-p)
          nil)
         ((string= match comment-start)
          (setq count (1+ count)))
         (t
          (setq count (1- count))))))
    (= count 0)))

(defun rml-backward-comment ()
  "Skip one (eventually nested) comment."
  (let ((count 1) match)
    (while (> count 0)
      (if (not (re-search-backward "(\\*\\|\\*)" nil 'move))
          (setq count -1)
        (setq match (rml-match-string 0))
        (cond
         ((rml-in-literal-p)
          nil)
         ((string= match comment-start)
          (setq count (1- count)))
         (t
          (setq count (1+ count))))))
    (= count 0)))

(defun rml-in-comment-p ()
  "Returns non-nil if point is inside a rml comment.
Returns nil for the parenthesis openning a comment."
  ;;we look for comments differently than literals. there are two
  ;;reasons for this. first, rml has nested comments and it is not so
  ;;clear that parse-partial-sexp supports them; second, if proper
  ;;style is used, literals are never split across lines, so we don't
  ;;have to worry about bogus phrase breaks inside literals, while we
  ;;have to account for that possibility in comments.
  (if rml-last-comment-start
      (save-excursion
        (let* ((cached-pos rml-last-noncomment-pos)
               (cached-begin (marker-position rml-last-comment-start))
               (cached-end (marker-position rml-last-comment-end)))
          (cond
           ((and cached-begin cached-end
                 (< cached-begin (point)) (< (point) cached-end)) t)
           ((and cached-pos (= cached-pos (point))) nil)
           ((and cached-pos (> cached-pos (point))
                 (< (abs (- cached-pos (point))) rml-lookback-limit))
            (let (end found (here (point)))
                                        ; go back to somewhere sure
              (goto-char cached-pos)
              (while (> (point) here)
                                        ; look for the end of a comment
                (while (and (if (search-backward comment-end (1- here) 'move)
                                (setq end (match-end 0))
                              (setq end nil))
                            (rml-in-literal-p)))
                (if end (setq found (rml-backward-comment))))
              (if (and found (= (point) here)) (setq end nil))
              (if (not end)
                  (setq rml-last-noncomment-pos here)
                (set-marker rml-last-comment-start (point))
                (set-marker rml-last-comment-end end))
              end))
           (t
            (let (begin found (here (point)))
            ;; go back to somewhere sure (or far enough)
              (goto-char
               (if cached-pos cached-pos (- (point) rml-lookback-limit)))
              (while (< (point) here)
                ;; look for the beginning of a comment
                (while (and (if (search-forward comment-start (1+ here) 'move)
                                (setq begin (match-beginning 0))
                              (setq begin nil))
                            (rml-in-literal-p)))
                (if begin (setq found (rml-forward-comment))))
              (if (and found (= (point) here)) (setq begin nil))
              (if (not begin)
                  (setq rml-last-noncomment-pos here)
                (set-marker rml-last-comment-start begin)
                (set-marker rml-last-comment-end (point)))
              begin)))))))

;; Various constants and regexps

(defconst rml-before-expr-prefix
  (concat "\\<\\(asr\\|begin\\|loop\\|class\\|do\\(par\\|wnto\\)?"
	  "\\|else\\|i\\(f\\|n\\(herit\\|itializer\\)?\\)"
          "\\|f\\(or\\|un\\(ct\\(ion\\|or\\)\\)?\\)"
          "\\|l\\(and\\|or\\|s[lr]\\|xor\\)\\|m\\(atch\\|od\\)"
	  "\\|pr\\(esent\\|oc\\)\\|control"
          "\\|o[fr]\\|parser\\|s\\(ig\\|truct\\)\\|t\\(hen\\|o\\|ry\\)"
          "\\|until\\|w\\(h\\(en\\|ile\\)\\|ith\\)\\)\\>\\|:begin\\>"
          "\\|[=<>@^|&+-*/$%][!$%*+-./:<=>?@^|~]*\\|:[:=]\\|[[({,;]")

  "Keywords that may appear immediately before an expression.
Used to distinguish it from toplevel let construct.")

(defconst rml-matching-kw-regexp
  (concat
   "\\<\\(and\\|do\\(ne\\|par\\)?\\|e\\(lse\\|nd\\)\\|in\\|t\\(hen\\|o\\)"
   "\\|until\\|when\\|with\\)\\>\\|[^[|]|[^|]")
  "Regexp used in rml mode for skipping back over nested blocks.")

(defconst rml-matching-kw-alist
  '(("|" . rml-find-pipe-match)
    (";" . rml-find-semi-match)
    ("," . rml-find-comma-match)
    ("end" . rml-find-end-match)
    ("done" . rml-find-done-match)
    ("in"  . rml-find-in-match)
    ("until"  . rml-find-until-match)
    ("when"  . rml-find-when-match)
    ("with" . rml-find-with-match)
    ("else" . rml-find-else-match)
    ("then" . rml-find-then-match)
    ("to" . rml-find-done-match)
    ("do" . rml-find-done-match)
    ("dopar" . rml-find-done-match)
    ("and" . rml-find-and-match))

  "Association list used in rml mode for skipping back over nested blocks.")

(defconst rml-kwop-regexps (make-vector 9 nil)
  "Array of regexps representing rml keywords of different priorities.")

(defun rml-in-expr-p ()
  (let ((pos (point)) (in-expr t))
    (rml-find-kwop
     (concat rml-before-expr-prefix "\\|"
             rml-matching-kw-regexp "\\|"
             (aref rml-kwop-regexps rml-max-indent-priority)))
    (cond
     ; special case for ;;
     ((and (> (point) 1) (= (preceding-char) ?\;) (= (following-char) ?\;))
      (setq in-expr nil))
     ((looking-at rml-before-expr-prefix)
      (if (not (looking-at "(\\*")) (goto-char (match-end 0)))
      (skip-chars-forward " \t\n")
      (while (looking-at "(\\*")
        (forward-char)
        (rml-forward-comment)
        (skip-chars-forward " \t\n"))

      (if (string= (match-string 0) "when")
	  (progn
	    (let ((pos2 (- (point) 5)))
	      (goto-char pos2)
	      (if (= (rml-verif-boucle pos) 1)
		  ;not first statement : not in-expr
		  (setq in-expr nil)

		))
	    (goto-char pos)))
      ;(goto-char pos)

      (if (<= pos (point)) (setq in-expr nil))
))
    (goto-char pos)
    in-expr))

(defun rml-at-sexp-close-p ()
  (or (char-equal ?\) (following-char))
      (char-equal ?\] (following-char))
      (char-equal ?} (following-char))))

(defun rml-find-kwop (kwop-regexp &optional min-pos)
  "Look back for a rml keyword or operator matching KWOP-REGEXP.
Second optional argument MIN-POS bounds the search.

Ignore occurences inside literals. If found, return a list of two
values: the actual text of the keyword or operator, and a boolean
indicating whether the keyword was one we looked for explicitly
{non-nil}, or on the other hand one of the block-terminating
keywords."

  (let ((start-literal (concat "[\"" rml-quote-char "]"))
        found kwop)
    (while (and (> (point) 1) (not found)
                (re-search-backward kwop-regexp min-pos 'move))
      (setq kwop (rml-match-string 0))
      (cond
       ((looking-at "(\\*")
        (if (> (point) 1) (backward-char)))
       ((rml-in-comment-p)
        (search-backward "(" min-pos 'move))
       ((looking-at start-literal))
       ((rml-in-literal-p)
        (re-search-backward start-literal min-pos 'move))  ;ugly hack
       ((setq found t))))
    (if found
        (if (not (string-match "\\`[^|[]|[^]|]?\\'" kwop)) ;arrrrgh!!
            kwop
          (forward-char 1) "|") nil)))

;  Association list of indentation values based on governing keywords.
;
;Each element is of the form (KEYWORD OP-TYPE PRIO INDENT). OP-TYPE is
;non-nil for operator-type nodes, which affect indentation in a
;different way from keywords: subsequent lines are indented to the
;actual occurrence of an operator, but relative to the indentation of
;the line where the governing keyword occurs.

(defconst rml-no-indent 0)

(defconst rml-kwop-alist
  '(("begin"            nil     6       rml-begin-indent)
    ("loop"            nil     6       rml-loop-indent)
    (":begin"           nil     6       rml-begin-indent) ; hack
    ("class"            nil     0       rml-class-indent)
    ("constraint"       nil     0       rml-val-indent)
    ("sig"              nil     1       rml-sig-indent)
    ("struct"           nil     1       rml-struct-indent)
    ("exception"        nil     0       rml-exception-indent)
    ("await"              nil     6       rml-await-indent)
    ("await-in"           nil     6       rml-await-in-indent)
    ("control"              nil     6       rml-control-indent)
    ("do"              nil     6       rml-do-indent)
    ("for"              nil     6       rml-for-indent)
    ("fun"              nil     3       rml-fun-indent)
    ("function"         nil     3       rml-function-indent)
    ("if"               nil     6       rml-if-indent)
    ("if-else"          nil     6       rml-if-else-indent)
    ("include"          nil     0       rml-include-indent)
    ("inherit"          nil     0       rml-inherit-indent)
    ("initializer"      nil     0       rml-initializer-indent)
    ("let"              nil     6       rml-let-indent)
    ("let-in"           nil     6       rml-let-in-indent)
    ("match"            nil     6       rml-match-indent)
    ("method"           nil     0       rml-method-indent)
    ("module"           nil     0       rml-module-indent)
    ("object"           nil     6       rml-object-indent)
    ("of"               nil     7       rml-of-indent)
    ("open"             nil     0       rml-no-indent)
    ("parser"           nil     3       rml-parser-indent)
    ("present"               nil     6       rml-present-indent)
    ("present-else"          nil     6       rml-present-else-indent)
    ("proc"              nil     3       rml-fun-indent)
    ("signal"              nil     6       rml-signal-indent)
    ("signal-in"           nil     6       rml-signal-in-indent)
    ("try"              nil     6       rml-try-indent)
    ("type"             nil     0       rml-type-indent)
    ("val"              nil     0       rml-val-indent)
    ("when"             nil     2       rml-if-indent)
    ("while"            nil     6       rml-while-indent)
    ("::"               t       5       rml-::-indent)
    ("@"                t       4       rml-@-indent)
    ("^"                t       4       rml-@-indent)
    (":="               nil     3       rml-:=-indent)
    ("<-"               nil     3       rml-<--indent)
    ("->"               nil     2       rml-->-indent)
    ("\["               t       8       rml-lb-indent)
    ("{"                t       8       rml-lc-indent)
    ("\("               t       8       rml-lp-indent)
    ("||"                nil    6       rml-no-indent)
    ("|"                nil     2       rml-no-indent)
    (";;"               nil     0       rml-no-indent))
; if-else and let-in are not keywords but idioms
; "|" is not in the regexps
; all these 3 values correspond to hard-coded names

"Association list of indentation values based on governing keywords.

Each element is of the form (KEYWORD OP-TYPE PRIO INDENT). OP-TYPE is
non-nil for operator-type nodes, which affect indentation in a
different way from keywords: subsequent lines are indented to the
actual occurrence of an operator, but relative to the indentation of
the line where the governing keyword occurs.")

;;Originally, we had rml-kwop-regexp create these at runtime, from an
;;additional field in rml-kwop-alist. That proved way too slow,
;;although I still can't understand why. itz

(aset rml-kwop-regexps 0
      (concat
       "\\<\\(begin\\|loop\\|object\\|for\\|do\\|s\\(ig\\|truct\\)"
       "\\|while\\)\\>\\|:begin\\>\\|[[({]\\|;;\\|\\(|\\)\\(|\\)"))
(aset rml-kwop-regexps 1
      (concat (aref rml-kwop-regexps 0) "\\|\\<\\(class\\|module\\)\\>"))
(aset rml-kwop-regexps 2
      (concat
       (aref rml-kwop-regexps 1)
       "\\|\\<\\(fun\\(ction\\)?\\|initializer\\|await\\|let\\|signal"
       "\\|m\\(atch\\|ethod\\)\\|p\\(arser\\|roc\\)\\|try\\|control\\|val\\)\\>\\|->"))
(aset rml-kwop-regexps 3
      (concat (aref rml-kwop-regexps 2)
	      "\\|\\<if\\|present\\|when\\>\\|\\(|\\)\\(|\\)"))
(aset rml-kwop-regexps 4
      (concat (aref rml-kwop-regexps 3) "\\|:=\\|<-"))
(aset rml-kwop-regexps 5
      (concat (aref rml-kwop-regexps 4) "\\|@"))
(aset rml-kwop-regexps 6
      (concat (aref rml-kwop-regexps 5) "\\|::\\|\\^"))
(aset rml-kwop-regexps 7
      (concat
       (aref rml-kwop-regexps 0)
       "\\|\\<\\(constraint\\|exception\\|in\\(herit\\|clude\\)"
       "\\|o\\(f\\|pen\\)\\|type\\|val\\)\\>"))
(aset rml-kwop-regexps 8
      (concat (aref rml-kwop-regexps 6)
       "\\|\\<\\(constraint\\|exception\\|in\\(herit\\|clude\\)"
       "\\|o\\(f\\|pen\\)\\|type\\)\\>"))

(defun rml-find-done-match ()
  (let ((unbalanced 1) (kwop t))
    (while (and (not (= 0 unbalanced)) kwop)
      (setq kwop (rml-find-kwop "\\<\\(do\\(ne\\)?\\|for\\|wh\\(en\\|ile\\)\\)\\>"))
      (cond
       ((not kwop))

       ((string= kwop "when")
	(let ((beg (point)) (prec (rml-find-when-match)))
	  (if prec
	      prec
	    (goto-char beg))))

       ((string= kwop "do")
	(let ((beg (point))
	      (retour (search-backward-regexp "\\<\\(for\\|while\\)\\>"
				       (point-min)
				       t)))
	  (if retour
	      (progn
		(let ((for-point (point)))
		(search-forward-regexp "do[ \n\t]\\|dopar[ \n\t]" (point-max) t)
		(if (string= (rml-match-string 0) "downto")
		    (search-forward "do" (point-max) t)
		  (if (> (point) beg)
		    ;same do : in a for form
		    (progn
		      (setq kwop "for")
		      (goto-char for-point)
		      (setq unbalanced (1- unbalanced)))
		  (goto-char beg)
		  (setq unbalanced (1- unbalanced))))))
	    (goto-char beg)
	    (setq unbalanced (1- unbalanced)))))

        ((string= kwop "done") (setq unbalanced (1+ unbalanced)))
       (t (setq unbalanced (1- unbalanced)))))
    kwop))

(defun rml-find-end-match ()
  (let ((unbalanced 1) (kwop t))
    (while (and (not (= 0 unbalanced)) kwop)
      (setq kwop
            (rml-find-kwop
             "\\<\\(end\\|begin\\|loop\\|object\\|s\\(ig\\|truct\\)\\)\\>\\|:begin\\>\\|;;"))
      (cond
       ((not kwop))
       ((string= kwop ";;") (setq kwop nil) (forward-line 1))
       ((string= kwop "end") (setq unbalanced (1+ unbalanced)))
       ( t (setq unbalanced (1- unbalanced)))))
    (if (string= kwop ":begin") "begin"
      kwop)))

(defun rml-find-in-match ()
  (let ((unbalanced 1) (kwop t))
    (while (and (not (= 0 unbalanced)) kwop)
      (setq kwop (rml-find-kwop "\\<\\(in\\|await\\|let\\|signal\\|end\\)\\>"))
      (let (fin end (pos (point)))
	(end-of-line)
	(setq end (point))
	(setq fin (preceding-char))
	(goto-char pos)
	(cond
	 ((not kwop))
	 ((string= kwop "end") (rml-find-end-match))
	 ((string= kwop "in")
	  (setq unbalanced (1+ unbalanced)))
	 ((= fin ?\;)
	  (goto-char end)
	  (if (re-search-backward "{" pos t)
	      (if (not (re-search-forward "}" end t))
		  (setq unbalanced (1- unbalanced)))
	    )
	  (goto-char pos))
	   ;par ex dans le cas await s;
	  ;dans le cas let { ...;
	  ;                   ...}
	  (t (setq unbalanced (1- unbalanced))))))
    kwop))

(defun rml-find-until-match ()
  (let ((unbalanced 1) (kwop t))
    (while (and (not (= 0 unbalanced)) kwop)
      (setq kwop (rml-find-kwop "\\<\\(until\\|do\\|when\\)\\>"))
      (cond
       ((not kwop))
       ((string= kwop "when") (rml-find-when-match))
       ((string= kwop "until") (setq unbalanced (1+ unbalanced)))

       (t (let ((beg (point))
(retour (search-backward-regexp "\\<\\(for\\|while\\)\\>"
				       (point-min)
				       t)))
;(retour (search-backward "for" (point-min) t)))
	    (if retour
		(progn
		  (let ((for-point (point)))
;		    (search-forward "\\<\\(do[^w]\\)\\>" (point-max) t)
		    (search-forward-regexp "do[ \n\t]\\|dopar" (point-max) t)
		    (if (string= (rml-match-string 0) "downto")
			(search-forward "do" (point-max) t)
		      (if (> (point) beg)
		      ;same do
			  (goto-char for-point)
			(goto-char beg)
			(setq unbalanced (1- unbalanced))))))
	      (goto-char beg)
	      (setq unbalanced (1- unbalanced)))))

))
    kwop))

(defun rml-find-when-match ()
  (let ((unbalanced 1) (kwop t))
    (while (and (not (= 0 unbalanced)) kwop)
      (setq kwop (rml-find-kwop "\\<\\(do\\(ne\\)?\\|when\\)\\>"))
      (cond
       ((not kwop))
       ;((string= kwop "until") (rml-find-until-match))
       ((string= kwop "done") (rml-find-done-match))
       ((string= kwop "when") (setq unbalanced (1+ unbalanced)))

       (t (let ((beg (point))
(retour (search-backward-regexp "\\<\\(for\\|while\\)\\>"
				       (point-min)
				       t)))
;(retour (search-backward "for" (point-min) t)))
	    (if retour
		(progn
		  (let ((for-point (point)))
;		    (search-forward "\\<\\(do[^w]\\)\\>" (point-max) t)
		    (search-forward-regexp "do[ \n\t]\\|dopar" (point-max) t)
		    (if (string= (rml-match-string 0) "downto")
			(search-forward "do" (point-max) t)
		      (if (> (point) beg)
		      ;same do
			  (goto-char for-point)
			(goto-char beg)
			(setq unbalanced (1- unbalanced))))))
	      (goto-char beg)
	      (setq unbalanced (1- unbalanced)))))
))
   kwop))

(defun rml-find-with-match ()
  (let ((unbalanced 1) (kwop t))
    (while (and (not (= 0 unbalanced)) kwop)
      (setq kwop
        (rml-find-kwop
         "\\<\\(with\\|try\\|control\\|m\\(atch\\|odule\\)\\|functor\\)\\>\\|[{}()]"))
      (cond
       ((not kwop))
       ((rml-at-sexp-close-p)
        (rml-find-paren-match (following-char)))
       ((string= kwop "with")
        (setq unbalanced (1+ unbalanced)))
       ((or (string= kwop "module")
            (string= kwop "functor")
            (string= kwop "{")
            (string= kwop "("))
        (setq unbalanced 0))
       (t (setq unbalanced (1- unbalanced)))))
    kwop))

(defun rml-find-paren-match (close)
  (let ((unbalanced 1)
        (regexp (cond ((= close ?\)) "[()]")
                      ((= close ?\]) "[][]")
                      ((= close ?\}) "[{}]"))))
    (while (and (> unbalanced 0)
                (rml-find-kwop regexp))
      (if (= close (following-char))
          (setq unbalanced (1+ unbalanced))
        (setq unbalanced (1- unbalanced))))))

(defun rml-find-then-match (&optional from-else)
  (let ((bol (if from-else
                 (save-excursion
                   (progn (beginning-of-line) (point)))))
        kwop done matching-fun)
    (while (not done)
      (setq kwop
            (rml-find-kwop
             (concat "\\<\\(e\\(nd\\|lse\\)\\|done\\|then"
		     "\\|if\\|present\\|with\\)\\>\\|[])};]")))
      (cond
       ((not kwop) (setq done t))
       ((rml-at-sexp-close-p)
        (rml-find-paren-match (following-char)))
       ((string= kwop "if") (setq done t))
       ((string= kwop "present") (setq done t))
       ((string= kwop "then")
        (if (not from-else) (setq kwop (rml-find-then-match))))
       ((setq matching-fun (cdr-safe (assoc kwop rml-matching-kw-alist)))
        (setq kwop (funcall matching-fun)))))
    (if (and bol (>= (point) bol))
        "if-else"
      kwop)))

(defun rml-find-pipe-match ()
  (let ((done nil) (kwop)
        (re (concat
             "\\<\\(try\\|match\\|with\\|function\\|parser\\|type"
             "\\|e\\(nd\\|lse\\)\\|done\\|then\\|until\\|when\\|in\\)\\>"
             "\\|[^[|]|\\|[])}]")))
    (while (not done)
      (setq kwop (rml-find-kwop re))
      (cond
       ((not kwop) (setq done t))
       ((looking-at "[^[|]\\(|\\)")
        (goto-char (match-beginning 1))
        (setq kwop "|")
        (setq done t))
       ((rml-at-sexp-close-p)
        (rml-find-paren-match (following-char)))
       ((string= kwop "with")
        (setq kwop (rml-find-with-match))
        (setq done t))
       ((string= kwop "parser")
        (if (re-search-backward "\\<with\\>" (- (point) 5) t)
            (setq kwop (rml-find-with-match)))
        (setq done t))
       ((string= kwop "until")
	(setq kwop (rml-find-until-match))
	(setq done t))
       ((string= kwop "when")
	(setq kwop (rml-find-when-match))
	(setq done t))
       ((string= kwop "done") (rml-find-done-match))
       ((string= kwop "end") (rml-find-end-match))
       ((string= kwop "then") (rml-find-then-match))
       ((string= kwop "else") (rml-find-else-match))
       ((string= kwop "in") (rml-find-in-match))
       (t (setq done t))))
    kwop))

(defun rml-find-and-match ()
  (let ((done nil) (kwop))
    (while (not done)
      (setq kwop (rml-find-kwop
                  (concat "\\<\\(object\\|exception\\|await\\|let"
			  "\\|signal\\|type\\|end\\|in\\)\\>")))
      (cond
       ((not kwop) (setq done t))
       ((string= kwop "end") (rml-find-end-match))
       ((string= kwop "in") (rml-find-in-match))
       (t (setq done t))))
    kwop))

(defun rml-find-else-match ()
  (rml-find-then-match t))

(defun rml-find-semi-match ()
  (rml-find-kwop-skipping-blocks 2))

(defun rml-find-comma-match ()
  (rml-find-kwop-skipping-blocks 3))

(defun rml-find-kwop-skipping-blocks (prio)
  "Look back for a rml keyword matching rml-kwop-regexps [PRIO].

 Skip nested blocks."
  (let ((done nil) (kwop nil) (matching-fun)(pos (point))
        (kwop-list (aref rml-kwop-regexps prio)))
    (while (not done)
      (setq kwop (rml-find-kwop
                  (concat rml-matching-kw-regexp
                          (cond ((> prio 3) "\\|[])},;]\\|")
                                ((> prio 2) "\\|[])};]\\|")
                                (t "\\|[])}]\\|"))
                          kwop-list)))
      (cond
       ((not kwop) (setq done t))
       ((rml-at-sexp-close-p)
        (rml-find-paren-match (following-char)))
       ((and (< prio 1) (string= kwop "||"))
	)
       ((string= kwop "||")

	(setq done t))
       ((or (string= kwop ";;")
	    (and (string= kwop ";") (= (preceding-char) ?\;)))
	(forward-line 1)
	(setq kwop ";;")
        (setq done t))
       ((and (>= prio 2) (string= kwop "|")) (setq done t))
       ((string= kwop "end") (rml-find-end-match))
       ((string= kwop "done") (rml-find-done-match))
       ((string= kwop "in")
	(let ((mot (rml-find-in-match)))
	  (cond ((and mot (>= prio 2))
		 (cond ((string= "let" mot)
			(setq kwop "let-in"))
		       ((string= "signal" mot)
			(setq kwop "signal-in"))
		       ((string= "await" mot)
			(setq kwop "await-in")))
		 (setq done t)))))
       ((and (string= kwop "parser") (>= prio 2)
             (re-search-backward "\\<with\\>" (- (point) 5) t))
        (setq kwop (rml-find-with-match))
        (setq done t))
       ((and (looking-at "do[ \t\n]")
	     (not (looking-at "done"))
	     (not (looking-at "dopar")))
	(let ((beg (point)) ;(prec (rml-find-done-match)))
(retour (search-backward-regexp "\\<\\(for\\|while\\)\\>"
				       (point-min)
				       t)))
;	      (retour (search-backward "for" (point-min) t)))
	  (if retour
	      (progn
		(let ((for-point (point)))
;		  (search-forward "\\<\\(do[^w]\\)\\>" (point-max) t)
		  (search-forward-regexp "do[ \n\t]\\|dopar" (point-max) t)
		  (if (string= (rml-match-string 0) "downto")
		      (search-forward "do" (point-max) t)
		    (if (> (point) beg)
;		    (setq kwop prec)
			(progn
			  (goto-char beg)
			  (setq matching-fun (cdr-safe (assoc kwop rml-matching-kw-alist))))

		      (setq done t)
		      (goto-char beg)
		      (setq kwop-list
			    (aref rml-kwop-regexps 6))
		      (setq done t)))))
	    (goto-char beg)
	    (setq kwop-list
		  (aref rml-kwop-regexps 6))
	    (setq done t)
)))

       ((and (looking-at "when")
	     (let ((beg (point)) (prec (rml-find-when-match)))
	       (goto-char beg)
	       (if prec
		   t    ;if do-when form
		 nil)))
	(setq matching-fun (cdr-safe (assoc kwop rml-matching-kw-alist)))
	(if (= (rml-verif-boucle pos) 1) ;if not first statement
	    (rml-find-when-match)
	  (setq kwop (funcall matching-fun))
	  (if (looking-at kwop-list) (setq done t))))

       ((looking-at "until")
	(setq matching-fun (cdr-safe (assoc kwop rml-matching-kw-alist)))
	(let ((beg (point)))
	(if (= (rml-verif-boucle (point)) 0) ;if first statement
	    (progn
	      (goto-char beg)
	      (rml-find-until-match))
	  (setq kwop (funcall matching-fun))
	  (if (looking-at kwop-list) (setq done t)))))
       ((and
	 (not (string= kwop "do"))
	 (not (string= kwop "done"))
;	 (not (string= kwop "dopar"))
	 (setq matching-fun (cdr-safe (assoc kwop rml-matching-kw-alist))))
	 (setq kwop (funcall matching-fun))
	 (if (looking-at kwop-list) (setq done t))
)

       ((and (looking-at "when")
	    (< prio 3))
	(setq kwop nil))
       (t
	(if ;(or
	    (string= kwop "await")
		;(string= kwop "let"))
	    (progn
	      (let ((pos (point)))
		(end-of-line)
		(skip-chars-backward " \t")
		;; (while (or (= (preceding-char) "\t")
;; 			   (= (preceding-char) " "))
;; 		  (goto-char (- (point) 1)))
		(if (= (preceding-char) ?\;)
		;(goto-char (- (point) 2))
		;(if (string= (rml-match-string 0) ";;")
		    (goto-char pos)
		  (goto-char pos)
		  (setq done t))))
	  (let* ((kwop-info (assoc kwop rml-kwop-alist))
                 (is-op (and (nth 1 kwop-info)
			  ; check that we are not at beginning of line
                             (let ((pos (point)) bti)
                               (back-to-indentation)
                               (setq bti (point))
                               (goto-char pos)
                               (< bti pos)))))
            (if (and is-op (looking-at
                            (concat (regexp-quote kwop)
                                    "|?[ \t]*\\(\n\\|(\\*\\)")))
                (setq kwop-list
                      (aref rml-kwop-regexps (nth 2 kwop-info)))
              (setq done t)))))))
    kwop))

(defun rml-compute-basic-indent (prio)
  "Compute indent of current rml line, ignoring leading keywords.

Find the `governing node' for current line. Compute desired
indentation based on the node and the indentation alists.
Assumes point is exactly at line indentation.
Does not preserve point."

  (let* (in-expr
	 (kwop (cond
                ((looking-at ";;")
                 (beginning-of-line 1))
                ((looking-at "|\\([^]|]\\|\\'\\)")
                 (rml-find-pipe-match))
                ((and (looking-at rml-phrase-start-keywords)
                      (rml-in-expr-p))
                 (rml-find-end-match))

		((and (looking-at "do[ \t\n]")
		      (not (looking-at "done"))
		      (not (looking-at "dopar")))
		 (let ((beg (point)) ;(prec (rml-find-done-match)))
(retour (search-backward-regexp "\\<\\(for\\|while\\)\\>"
				       (point-min)
				       t)))
;		       (retour (search-backward "for" (point-min) t)))
		   (if retour
		       (progn
			 (let ((for-point (point)))
			   (search-forward-regexp
			    "\\<\\(do\\|dopar\\)\\>" (point-max) t)
			   (if (string= (rml-match-string 0) "downto")
			       (search-forward "do" (point-max) t)
			     (if (> (point) beg)
			     ;same do : for-form
				 (progn
				   (search-backward-regexp
				    "do" (point-min) t)
				   (rml-find-done-match)
				   )
			   ;do-form
			       (goto-char beg)
			       (rml-find-kwop-skipping-blocks 6)))))
		     (goto-char beg)
		     (rml-find-kwop-skipping-blocks 6))))

		((looking-at "when")
		 (let ((beg (point)) (prec (rml-find-when-match)))
		   (if prec
		       prec		;if not do-when form
		     (goto-char beg)
		     (rml-find-kwop-skipping-blocks 2))))
		((and (looking-at rml-matching-kw-regexp)
                      (assoc (rml-match-string 0) rml-matching-kw-alist))
                 (funcall (cdr-safe (assoc (rml-match-string 0)
					   rml-matching-kw-alist))))
                ((looking-at
                  (aref rml-kwop-regexps rml-max-indent-priority))
                 (let* ((kwop (rml-match-string 0))
                        (kwop-info (assoc kwop rml-kwop-alist))
                        (prio (if kwop-info (nth 2 kwop-info)
                                rml-max-indent-priority)))
		   (if (and (looking-at (aref rml-kwop-regexps 0))
                            (not (looking-at "object"))
			    (not (looking-at "||"))
			    (rml-in-expr-p))
		       (setq in-expr t))
		   (rml-find-kwop-skipping-blocks prio)))
                (t
		 (if (and (= prio rml-max-indent-priority) (rml-in-expr-p))
		     (setq in-expr t))
		 (rml-find-kwop-skipping-blocks prio))))
         (kwop-info (assoc kwop rml-kwop-alist))
         (indent-diff
          (cond
           ((not kwop-info) (beginning-of-line 1) 0)
           ((looking-at "[[({][|<]?[ \t]*")
            (length (rml-match-string 0)))
           ((nth 1 kwop-info) (symbol-value (nth 3 kwop-info)))
           (t
            (let ((pos (point)))
              (back-to-indentation)
	      (if (looking-at "\n")
		  (beginning-of-line))
              (- (symbol-value (nth 3 kwop-info))
                 (if (and (looking-at "|") (not (looking-at "||")))
		     (progn rml-|-extra-indent)
		    0))))))
         (extra (if in-expr rml-apply-extra-indent 0)))
    (+ indent-diff extra (current-column))))

(defun rml-verif-boucle (pos)
  "Look after the word to see if the line to indent is the first
statement after the keyword (return 0) or not (return 1)"

   (let ((pos2 (+ (point) 5)))
     (goto-char pos2)
     (let ((d (skip-chars-forward " \t\n" pos)))
       (if (= d (- pos pos2))
		   (progn (goto-char (- pos2 5)) 0)
		 (goto-char (- pos2 5))
		 1))))

(defconst rml-leading-kwops-regexp
  (concat
   "\\<\\(and\\|do\\(ne\\|par\\)?\\|e\\(lse\\|nd\\)\\|in"
   "\\|t\\(hen\\|o\\)\\|until\\|when\\|with\\)\\>\\|[]|})]")

  "Regexp matching rml keywords which need special indentation.")

(defconst rml-leading-kwops-alist
  '(("and" rml-and-extra-indent 2)
    ("do" rml-do-extra-indent 0)
    ("dopar" rml-dopar-extra-indent 0)
    ("done" rml-done-extra-indent 0)
    ("else" rml-else-extra-indent 3)
    ("end" rml-end-extra-indent 0)
    ("in" rml-in-extra-indent 2)
    ("then" rml-then-extra-indent 3)
    ("to" rml-to-extra-indent 0)
    ("until" rml-until-extra-indent 2)
    ("when" rml-when-extra-indent 2)
    ("with" rml-with-extra-indent 2)
    ("|" rml-|-extra-indent 2)
    ("]" rml-rb-extra-indent 0)
    ("}" rml-rc-extra-indent 0)
    (")" rml-rp-extra-indent 0))

  "Association list of special rml keyword indent values.

Each member is of the form (KEYWORD EXTRA-INDENT PRIO) where
EXTRA-INDENT is the variable holding extra indentation amount for
KEYWORD (usually negative) and PRIO is upper bound on priority of
matching nodes to determine KEYWORD's final indentation.")

(defun rml-compute-final-indent ()
  (save-excursion
    (back-to-indentation)
    (cond
     ((and (bolp) (looking-at comment-start-skip)) (current-column))
     ((rml-in-comment-p)
      (let ((closing (looking-at "\\*)"))
            (comment-mark (looking-at "\\*")))
        (rml-backward-comment)
        (looking-at comment-start-skip)
        (+ (current-column)
           (cond
            (closing 1)
            (comment-mark 1)
            (t rml-comment-indent)))))
     ((and (looking-at "do[ \n\t]")
	   (not (looking-at "done"))
	   (not (looking-at "dopar")))
      (let ((beg (point)) ;(prec (rml-find-done-match)))
(retour (search-backward-regexp "\\<\\(for\\|while\\)\\>"
				       (point-min)
				       t)))
;	    (retour (search-backward "for" (point-min) t)))
	(if retour
	    (progn
	      (let ((for-point (point)))
;		(search-forward "\\<\\(do[^w]\\)\\>" (point-max) t)
		(search-forward-regexp "do[ \n\t]\\|dopar" (point-max) t)
		(if (string= (rml-match-string 0) "downto")
		    (search-forward "do" (point-max) t)
		  (if (> (point) beg)
		  ;same do : for-form
		      (progn
			(goto-char beg)
			(let ((basic (rml-compute-basic-indent 0)))
			  (max 0 (current-column))))
		    (goto-char beg)
		    (let ((basic (rml-compute-basic-indent rml-max-indent-priority)))
		      (max 0 basic))))))
	  (goto-char beg)
	  (let ((basic (rml-compute-basic-indent rml-max-indent-priority)))
		(max 0 basic)))))

     ((looking-at "when")
      (let ((beg (point)) (prec (rml-find-when-match)))
	(if prec
	    (progn
	      (goto-char beg)
	      (let ((basic (rml-compute-basic-indent 0)))
		(max 0 (current-column))))
	  (goto-char beg)  ;rml-find-when-match a modife la pos
	  (let ((basic (rml-compute-basic-indent rml-max-indent-priority)))
	    (max 0 basic)))))
     (t (let* ((leading (and (looking-at rml-leading-kwops-regexp)
					(not (looking-at "||"))))
               (assoc-val (if leading (assoc (rml-match-string 0)
                                             rml-leading-kwops-alist)))
               (extra (if leading (symbol-value (nth 1 assoc-val)) 0))
               (prio (if leading (nth 2 assoc-val)
                       rml-max-indent-priority))
               (basic (rml-compute-basic-indent prio)))
	  (max 0 (if extra (+ extra basic) (current-column))))))))


(defun rml-split-string ()
  "Called whenever a line is broken inside a rml string literal."
  (insert-before-markers "\"^\"")
  (backward-char 1))

(defadvice indent-new-comment-line (around
                                    rml-indent-new-comment-line
                                    activate)

  "Handle multi-line strings in rml mode."

;this advice doesn't make sense in other modes. I wish there were a
;cleaner way to do this: I haven't found one.

  (let ((hooked (and (eq major-mode 'rml-mode) (rml-in-literal-p)))
        (split-mark))
    (if (not hooked) nil
      (setq split-mark (set-marker (make-marker) (point)))
      (rml-split-string))
    ad-do-it
    (if (not hooked) nil
      (goto-char split-mark)
      (set-marker split-mark nil))))

(defadvice newline-and-indent (around
                               rml-newline-and-indent
                               activate)

  "Handle multi-line strings in rml mode."

    (let ((hooked (and (eq major-mode 'rml-mode) (rml-in-literal-p)))
        (split-mark))
    (if (not hooked) nil
      (setq split-mark (set-marker (make-marker) (point)))
      (rml-split-string))
    ad-do-it
    (if (not hooked) nil
      (goto-char split-mark)
      (set-marker split-mark nil))))

(defun rml-electric-pipe ()
  "If inserting a | or } operator at beginning of line, reindent the line.

Unfortunately there is a situation where this mechanism gets
confused. It's when | is the first character of a |] sequence. This is
a misfeature of rml syntax and cannot be fixed, however, as a
workaround, the electric ] inserts | itself if the matching [ is
followed by |."

  (interactive "*")
  (let ((electric (and rml-electric-indent
                       (rml-in-indentation)
                       (not (rml-in-comment-p)))))
    (self-insert-command 1)
    (if electric (save-excursion (rml-indent-command)))))

(defun rml-electric-rb ()
  "If inserting a ] operator at beginning of line, reindent the line.

Also, if the matching [ is followed by a | and this ] is not preceded
by |, insert one."

  (interactive "*")
  (let* ((prec (preceding-char))
         (use-pipe (and rml-electric-close-vector
                        (not (rml-in-comment-p))
                        (not (rml-in-literal-p))
                        (or (not (numberp prec))
                            (not (char-equal ?| prec)))))
         (electric (and rml-electric-indent
                        (rml-in-indentation)
                        (not (rml-in-comment-p)))))
    (self-insert-command 1)
    (if electric (save-excursion (rml-indent-command)))
    (if (and use-pipe
             (save-excursion
               (condition-case nil
                   (prog2
                       (backward-list 1)
                       (looking-at "\\[|"))
                 (error ""))))
        (save-excursion
          (backward-char 1)
          (insert "|")))))

(defun rml-abbrev-hook ()
  "If inserting a leading keyword at beginning of line, reindent the line."
  ;itz unfortunately we need a special case
  (if (and (not (rml-in-comment-p)) (not (= last-command-event ?_)))
      (let* ((bol (save-excursion (beginning-of-line) (point)))
             (kw (save-excursion
                   (and (re-search-backward "^[ \t]*\\(\\sw+\\)\\=" bol t)
                        (rml-match-string 1)))))
        (if kw
            (let ((indent (save-excursion
                            (goto-char (match-beginning 1))
                            (rml-indent-command)
                            (current-column)))
                  (abbrev-correct (if (= last-command-event ?\ ) 1 0)))
              (indent-to (- indent
                            (or
                             (symbol-value
                              (nth 1
                                   (assoc kw rml-leading-kwops-alist)))
                             0)
                            abbrev-correct)))))))

; (defun rml-indent-phrase ()
;   (interactive "*")
;   (let ((bounds (rml-mark-phrase)))
;     (indent-region (car bounds) (cdr bounds) nil)))

;;; Additional commands by Didier to report errors in toplevel mode

(defun rml-skip-blank-forward ()
  (if (looking-at "[ \t\n]*\\((\\*\\([^*]\\|[^(]\\*[^)]\\)*\\*)[ \t\n]*\\)*")
      (goto-char (match-end 0))))

;; to mark phrases, so that repeated calls will take several of them
;; knows little about Rml appart literals and comments, so it should work
;; with other dialects as long as ;; marks the end of phrase.

(defun rml-indent-phrase (arg)
  "Indent current phrase
with prefix arg, indent that many phrases starting with the current phrase."
  (interactive "p")
  (save-excursion
    (let ((beg (rml-find-phrase)))
    (while (progn (setq arg (- arg 1)) (> arg 0)) (rml-find-phrase))
    (indent-region beg (point) nil))))

(defun rml-indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max) nil))

(defun rml-backward-to-less-indent (&optional n)
  "Move cursor back  N lines with less or same indentation."
  (interactive "p")
  (beginning-of-line 1)
  (if (< n 0) (rml-forward-to-less-indent (- n))
    (while (> n 0)
      (let ((i (current-indentation)))
        (forward-line -1)
        (while (or (> (current-indentation) i)
                   (rml-in-comment-p)
                   (looking-at
                    (concat "[ \t]*\\(\n\\|" comment-start-skip "\\)")))
          (forward-line -1)))
      (setq n (1- n))))
  (back-to-indentation))

(defun rml-forward-to-less-indent (&optional n)
  "Move cursor back N lines with less or same indentation."
  (interactive "p")
  (beginning-of-line 1)
  (if (< n 0) (rml-backward-to-less-indent (- n))
    (while (> n 0)
      (let ((i (current-indentation)))
        (forward-line 1)
        (while (or (> (current-indentation) i)
                   (rml-in-comment-p)
                   (looking-at
                    (concat "[ \t]*\\(\n\\|" comment-start-skip "\\)")))
          (forward-line 1)))
      (setq n (1- n))))
  (back-to-indentation))

(defun rml-insert-begin-form ()
  "Inserts a nicely formatted begin-end form, leaving a mark after end."
  (interactive "*")
  (let ((prec (preceding-char)))
    (if (and (numberp prec) (not (char-equal ?\  (char-syntax prec))))
        (insert " ")))
  (let* ((c (current-indentation)) (i (+ rml-begin-indent c)))
    (insert "begin\n\nend")
    (push-mark)
    (indent-line-to c)
    (forward-line -1)
    (indent-line-to i)))

(defun rml-insert-loop-form ()
  "Inserts a nicely formatted loop-end form, leaving a mark after end."
  (interactive "*")
  (let ((prec (preceding-char)))
    (if (and (numberp prec) (not (char-equal ?\  (char-syntax prec))))
        (insert " ")))
  (let* ((c (current-indentation)) (i (+ rml-begin-indent c)))
    (insert "loop\n\nend")
    (push-mark)
    (indent-line-to c)
    (forward-line -1)
    (indent-line-to i)))

(defun rml-insert-for-form ()
  "Inserts a nicely formatted for-do-done form, leaving a mark after do(ne)."
  (interactive "*")
  (let ((prec (preceding-char)))
    (if (and (numberp prec) (not (char-equal ?\  (char-syntax prec))))
        (insert " ")))
  (let* ((c (current-indentation)) (i (+ rml-for-indent c)))
    (insert "for  do\n\ndone")
    (push-mark)
    (indent-line-to c)
    (forward-line -1)
    (indent-line-to i)
    (push-mark)
    (beginning-of-line 1)
    (backward-char 4)))

(defun rml-insert-if-form ()
  "Insert nicely formatted if-then-else form leaving mark after then, else."
  (interactive "*")
  (let ((prec (preceding-char)))
    (if (and (numberp prec) (not (char-equal ?\  (char-syntax prec))))
        (insert " ")))
  (let* ((c (current-indentation)) (i (+ rml-if-indent c)))
    (insert "if\n\nthen\n\nelse\n")
    (indent-line-to i)
    (push-mark)
    (forward-line -1)
    (indent-line-to c)
    (forward-line -1)
    (indent-line-to i)
    (push-mark)
    (forward-line -1)
    (indent-line-to c)
    (forward-line -1)
    (indent-line-to i)))

(defun rml-insert-present-form ()
  "Insert nicely formatted present-then-else form leaving mark after then, else."
  (interactive "*")
  (let ((prec (preceding-char)))
    (if (and (numberp prec) (not (char-equal ?\  (char-syntax prec))))
        (insert " ")))
  (let* ((c (current-indentation)) (i (+ rml-present-indent c)))
    (insert "present\n\nthen\n\nelse\n")
    (indent-line-to i)
    (push-mark)
    (forward-line -1)
    (indent-line-to c)
    (forward-line -1)
    (indent-line-to i)
    (push-mark)
    (forward-line -1)
    (indent-line-to c)
    (forward-line -1)
    (indent-line-to i)))



(defun rml-insert-match-form ()
  "Insert nicely formatted match-with form leaving mark after with."
  (interactive "*")
  (let ((prec (preceding-char)))
    (if (and (numberp prec) (not (char-equal ?\  (char-syntax prec))))
        (insert " ")))
  (let* ((c (current-indentation)) (i (+ rml-match-indent c)))
    (insert "match\n\nwith\n")
    (indent-line-to i)
    (push-mark)
    (forward-line -1)
    (indent-line-to c)
    (forward-line -1)
    (indent-line-to i)))

(defun rml-insert-await-form ()
  "Insert nicely formatted await-in form leaving mark after in."
  (interactive "*")
  (let ((prec (preceding-char)))
    (if (and (numberp prec) (not (char-equal ?\  (char-syntax prec))))
        (insert " ")))
  (let* ((c (current-indentation)))
    (insert "await  in\n")
    (indent-line-to c)
    (push-mark)
    (forward-line -1)
    (forward-char (+ c 7))))

(defun rml-insert-let-form ()
  "Insert nicely formatted let-in form leaving mark after in."
  (interactive "*")
  (let ((prec (preceding-char)))
    (if (and (numberp prec) (not (char-equal ?\  (char-syntax prec))))
        (insert " ")))
  (let* ((c (current-indentation)))
    (insert "let  in\n")
    (indent-line-to c)
    (push-mark)
    (forward-line -1)
    (forward-char (+ c 4))))

(defun rml-insert-signal-form ()
  "Insert nicely formatted signal-in form leaving mark after in."
  (interactive "*")
  (let ((prec (preceding-char)))
    (if (and (numberp prec) (not (char-equal ?\  (char-syntax prec))))
        (insert " ")))
  (let* ((c (current-indentation)))
    (insert "signal  in\n")
    (indent-line-to c)
    (push-mark)
    (forward-line -1)
    (forward-char (+ c 7))))

(defun rml-insert-try-form ()
  "Insert nicely formatted try-with form leaving mark after with."
  (interactive "*")
  (let ((prec (preceding-char)))
    (if (and (numberp prec) (not (char-equal ?\  (char-syntax prec))))
        (insert " ")))
  (let* ((c (current-indentation)) (i (+ rml-try-indent c)))
    (insert "try\n\nwith\n")
    (indent-line-to i)
    (push-mark)
    (forward-line -1)
    (indent-line-to c)
    (forward-line -1)
    (indent-line-to i)))

(defun rml-insert-control-form ()
  "Insert nicely formatted control-with form leaving mark after with."
  (interactive "*")
  (let ((prec (preceding-char)))
    (if (and (numberp prec) (not (char-equal ?\  (char-syntax prec))))
        (insert " ")))
  (let* ((c (current-indentation)) (i (+ rml-control-indent c)))
    (insert "control\n\nwith\n")
    (indent-line-to i)
    (push-mark)
    (forward-line -1)
    (indent-line-to c)
    (forward-line -1)
    (indent-line-to i)))

(defun rml-insert-do-until-form ()
  "Insert nicely formatted do-until form leaving mark after do, done."
  (interactive "*")
  (let ((prec (preceding-char)))
    (if (and (numberp prec) (not (char-equal ?\  (char-syntax prec))))
        (insert " ")))
  (let* ((c (current-indentation)) (i (+ rml-do-indent c)))
    (insert "do\n\nuntil\n")
    (indent-line-to i)
    (push-mark)
    (forward-line -1)
    (indent-line-to c)
    (forward-line -1)
    (indent-line-to i)))

(defun rml-insert-do-when-form ()
  "Insert nicely formatted do-when form leaving mark after do, done."
  (interactive "*")
  (let ((prec (preceding-char)))
    (if (and (numberp prec) (not (char-equal ?\  (char-syntax prec))))
        (insert " ")))
  (let* ((c (current-indentation)) (i (+ rml-do-indent c)))
    (insert "do\n\nwhen\n")
    (indent-line-to i)
    (push-mark)
    (forward-line -1)
    (indent-line-to c)
    (forward-line -1)
    (indent-line-to i)))

(defun rml-insert-while-form ()
  "Insert nicely formatted while-do-done form leaving mark after do, done."
  (interactive "*")
  (let ((prec (preceding-char)))
    (if (and (numberp prec) (not (char-equal ?\  (char-syntax prec))))
        (insert " ")))
  (let* ((c (current-indentation)) (i (+ rml-if-indent c)))
    (insert "while  do\n\ndone")
    (push-mark)
    (indent-line-to c)
    (forward-line -1)
    (indent-line-to i)
    (push-mark)
    (beginning-of-line 1)
    (backward-char 4)))

(autoload 'rml-types-show-type "rml-types"
  "Show the type of expression or pattern at point." t)
(autoload 'rml-types-explore "rml-types"
  "Explore type annotations by mouse dragging." t)

(autoload 'rml-static-show-type "rml-static"
  "Show the type of expression or pattern at point." t)
(autoload 'rml-static-explore "rml-static"
  "Explore type annotations by mouse dragging." t)

(autoload 'rml-help "rml-help"
  "Show documentation for qualilifed Rml identifier." t)
(autoload 'rml-complete "rml-help"
  "Does completion for documented qualified Rml identifier." t)
(autoload 'rml-open-module "rml-help"
  "Add module in documentation search path." t)
(autoload 'rml-close-module "rml-help"
  "Remove module from documentation search path." t)
(autoload 'rml-add-path "rml-help"
  "Add search path for documentation." t)

;;; rml.el ends here

(provide 'rml)
