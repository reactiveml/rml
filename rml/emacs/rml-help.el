;; rml-info.el --- contextual completion and help to rml-mode

;; Didier Remy, November 2001.

;; This provides two functions completion and help
;; look for rml-complete and rml-help

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  This is a preliminary version.
;;
;;  Possible improvements?
;;   - dump some databaes: Info, Lib, ...
;;   - accept a search path for local libraries instead of current dir
;;     (then distinguish between different modules lying in different
;;     directories)
;;   - improve the construction for info files.
;;
;;  Abstract over
;;   - the viewing method and the database, so that the documentation for
;;     and identifier could be search in
;;       * info / html / man / rmli's sources
;;       * viewed in emacs or using an external previewer.
;;
;;  Take all identifiers (labels, Constructors, exceptions, etc.)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(eval-and-compile
  (if (and (boundp 'running-xemacs) running-xemacs)
      (require 'rml-xemacs)
    (require 'rml-emacs)))

;; Loading or building databases.
;;

;; variables to be customized

(defvar rml-lib-path 'lazy
  "Path list for rml lib sources (rmli files)

'lazy means ask rml to find it for your at first use.")
(defun rml-lib-path ()
  "Computes if necessary and returns the path for rml libs"
  (if (listp rml-lib-path) nil
    (setq rml-lib-path
          (split-string
           (shell-command-to-string
            (or
             (and (boundp 'inferior-rml-program)
                      (string-match "\\([^ ]*/rml\\)\\( \\|$\\)"
                       inferior-rml-program)
                      (let ((file
                             (concat (match-string 1 inferior-rml-program)
                                     "c")))
                        (and (file-executable-p file)
                             (concat file " -where"))))
             "rmlc -where")))))
  rml-lib-path)

;; General purpose auxiliary functions

(defun rml-capitalize (s)
  (concat (capitalize (substring s 0 1)) (substring s 1)))

(defun rml-uncapitalize (s)
  (if (> (length s) 0)
      (concat (downcase (substring s 0 1)) (substring s 1))
    s))

(defun iter (f l) (while (consp l) (apply f (list (car l))) (setq l (cdr l))))

(defun rml-find-files (path filter &optional depth split)
  (let* ((path-string
          (if (stringp path)
              (if (file-directory-p path) path nil)
            (mapconcat '(lambda (d) (if (file-directory-p d) d))
                       path " ")))
         (command
          (and path-string
               (concat "find " path-string
                       " '(' " filter " ')' "
                       (if depth (concat " -maxdepth " (int-to-string depth)))
                       (if split nil " -printf '%\p '")
                       )))
          (files
           (and command (shell-command-to-string command))))
         (if (and split (stringp files)) (split-string files "\n") files)
         ))

;; Specialized auxiliary functions


;; Global table of modules contents of modules loaded lazily.

(defvar rml-module-alist 'lazy
  "A-list of modules with how and where to find help information.
  'delay means non computed yet")

(defun rml-add-rmli-modules (modules tag &optional path)
  (let ((files
         (rml-find-files (or path (rml-lib-path))
                           "-type f -name '*.rmli'" 1 t)))
    (while (consp files)
      (if (string-match "\\([^/]*\\).rmli" (car files))
          (let* ((module (rml-capitalize (match-string 1 (car files))))
                 (dir (file-name-directory (car files)))
                 (dirp (member dir (rml-lib-path))))
            (if (and (consp dirp) (string-equal dir (car dirp)))
                (setq dir (car dirp)))
            (if (assoc module modules) nil
              (setq modules
                    (cons (cons module (cons (cons tag dir) 'lazy)) modules))
              )))
      (setq files (cdr files)))
    modules))

(defun rml-add-path (dir &optional path)
  "Extend  rml-module-alist with modules of DIR relative to PATH"
  (interactive "D")
  (let* ((old (rml-lib-path))
         (new
          (if (file-name-absolute-p dir) dir
            (concat
             (or (find-if '(lambda (p) (file-directory-p (concat p  "/" dir)))
                      (cons default-directory old))
                 (error "Directory not found"))
             "/" dir))))
    (setq rml-lib-path (cons (car old) (cons new (cdr old))))
    (setq rml-module-alist
          (rml-add-rmli-modules (rml-module-alist) 'lib new))))

(defun rml-module-alist ()
  "Call by need value of variable rml-module-alist"
  (if (listp rml-module-alist)
      nil
    ;; build list of rmli files
    (setq rml-module-alist (rml-add-rmli-modules nil 'lib))
    ;; dumping information ? TODO
    )
  rml-module-alist)

(defun rml-get-or-make-module (module &optional tag)
  (let ((info (assoc module (rml-module-alist))))
    (if info nil
      (setq info (cons module (cons (cons 'local default-directory) 'lazy)))
      (setq rml-module-alist (cons info rml-module-alist))
      )
    info))

;; Symbols of module are lazily computed

(defun rml-module-filename (module)
  (let ((module (rml-uncapitalize module)) (name))
    (if (file-exists-p (setq name (concat module ".rmli"))) nil
      (let ((tmp (rml-lib-path)))
        (while (consp tmp)
          (setq name (concat (car tmp) "/" module ".rmli"))
          (if (file-exists-p name) (setq tmp nil)
            (setq name nil)))))
    name))

(defun rml-module-symbols (module-info)
  (let* ((module (car module-info))
         (tail (and module-info (cdr module-info)))
         (tag (caar tail))
         (dir (cdar tail))
         (file)
         (alist))
    (if (listp (cdr tail))
        (cdr tail)
      (if (equal tag 'info)
          (setq dir (car rml-lib-path)) ; XXX to be fixed
        )
      (setq file (concat dir "/" (rml-uncapitalize module) ".rmli"))
      (message file)
      (save-window-excursion
        (set-buffer (get-buffer-create "*rml-help*"))
        (if (and file (file-exists-p file))
            (progn
              (message "Scanning module %s" file)
              (insert-file-contents file))
          (message "Module %s not found" module))
        (while (re-search-forward
                "\\([ \t]*val\\|let\\|external\\|  [|]\\) \\([a-zA-Z_0-9'][a-zA-Z_0-9']*\\)\\|^  *[{]* \\([a-z_][A-Za-z_0-9]*\\) : [^;\n][^;\n]*;"
                (point-max) 'move)
          (pop-to-buffer (current-buffer))
          (setq alist (cons (or (match-string 2) (match-string 3)) alist)))
        (erase-buffer)
        )
      (setcdr tail alist)
      alist)
      ))

;; Local list of visible modules.

(defvar rml-visible-modules 'lazy
  "A-list of open modules, local to every file.")
(make-variable-buffer-local 'rml-visible-modules)
(defun rml-visible-modules ()
  (if (listp rml-visible-modules) nil
    (progn
      (setq rml-visible-modules
            (list (rml-get-or-make-module "Pervasives")))
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward "^ *open  *\\([A-Z][a-zA-Z'_0-9]*\\)"
                                  (point-max) t)
          (let ((module (match-string 1)))
            (if (assoc module rml-visible-modules) nil
              (setq rml-visible-modules
                    (cons (rml-get-or-make-module module)
                          rml-visible-modules)))))
        )))
  rml-visible-modules)

(defun rml-open-module (arg)
  "*Make module of name ARG visible whe ARG is a string.
When call interactively, make completion over known modules."
  (interactive "P")
  (if (not (stringp arg))
      (let ((modules (rml-module-alist)))
        (setq arg
              (completing-read "Open module: " modules))))
  (if (and (stringp arg) (not (equal arg "")))
      (progn
        (if (assoc arg (rml-visible-modules))
            (rml-close-module arg))
        (setq rml-visible-modules
              (cons (rml-get-or-make-module arg) (rml-visible-modules)))
        ))
  (message "%S" (mapcar 'car (rml-visible-modules))))

(defun rml-close-module (arg)
  "*Close module of name ARG when ARG is a string.
When call interactively, make completion over visible modules.
Otherwise if ARG is true, close all modules and reset to default. "
  (interactive "P")
  (if (= (prefix-numeric-value arg) 4)
      (setq rml-visible-modules 'lazy)
    (let* ((modules (rml-visible-modules)))
      (if (null modules) (error "No visible module to close"))
      (unless (stringp arg)
        (setq arg
              (completing-read
               (concat "Close module ["  (caar modules) "] : ")
               modules))
        (if (equal arg "") (setq arg (caar modules))))
      (setq rml-visible-modules
            (remove-if '(lambda (m) (equal (car m) arg))
                       rml-visible-modules))
      ))
  (message "%S" (mapcar 'car (rml-visible-modules))))


;; Look for identifiers around point

(defun rml-qualified-identifier (&optional show)
  "Search for a qualified identifier (Path. entry) around point.

Entry may be nil.
Currently, the path may only be nil or a single Module.
For paths is of the form Module.Path', it returns Module
and always nil for entry.

If defined Module and Entry are represented by a region in the buffer,
and are nil otherwise.

For debugging purposes, it returns the string Module.entry if called
with an optional non-nil argument.
"
  (save-excursion
    (let ((module) (entry))
      (if (looking-at "[ \n]") (skip-chars-backward " "))
      (if (re-search-backward
           "\\([^A-Za-z0-9_.']\\|\\`\\)\\([A-Za-z0-9_']*[.]\\)*[A-Za-z0-9_']*\\="
           (- (point) 100) t)
          (progn
            (or (looking-at "\\`[A-Za-z)-9_.]") (forward-char 1))
            (if (looking-at "\\<\\([A-Za-z_][A-Za-z0-9_']*\\)[.]")
                (progn
                  (setq module (cons (match-beginning 1) (match-end 1)))
                  (goto-char (match-end 0))))
            (if (looking-at "\\<\\([A-Za-z_][A-Za-z0-9_']*\\)\\>")
                (setq entry (cons (match-beginning 1) (match-end 1))))))
      (if show
          (concat
           (and module (buffer-substring (car module) (cdr module)))
           "."
           (and entry (buffer-substring (car entry) (cdr entry))))
      (cons module entry))
    )))

;; completion around point

(defun rml-completion (pattern module)
  (let ((list
         (or
          (and module
               (list
                (or (assoc module (rml-module-alist))
                    (error "Unknown module %s" module))))
          (rml-visible-modules))))
    (message "Completion from %s" (mapconcat 'car list " "))
    (if (null pattern)
        (apply 'append (mapcar 'rml-module-symbols list))
      (let ((pat (concat "^" (regexp-quote pattern))) (res))
        (iter
         '(lambda (l)
            (iter '(lambda (x)
                     (if (string-match pat (car l))
                         (if (member x res) nil (setq res (cons x res)))))
                  (rml-module-symbols l)))
         list)
        res)
      )))

(defun rml-complete (arg)
  "Does completion for Rml identifiers qualified.

It attemps to recognize an qualified identifier Module . entry
around point using function \\[rml-qualified-identifier].

If Module is defined, it does completion for identifier in Module.

If Module is undefined, it does completion in visible modules.
Then, if completion fails, it does completion among  all modules
where identifier is defined."
  (interactive "p")
  (let* ((module-entry (rml-qualified-identifier)) (entry)
         (module)
         (beg) (end) (pattern))
    (if (car module-entry)
        (progn
          (setq module
                (buffer-substring (caar module-entry) (cdar module-entry)))
          (or (assoc module (rml-module-alist))
              (and (setq module
                         (completing-read "Module: " (rml-module-alist)
                                          nil nil module))
                   (save-excursion
                     (goto-char (caar module-entry))
                     (delete-region (caar module-entry) (cdar module-entry))
                     (insert module) t)
                   (setq module-entry (rml-qualified-identifier))
                   (car module-entry)
                   (progn (setq entry (cdr module-entry)) t))
              (error "Unknown module %s" module))))
    (if (consp (cdr module-entry))
        (progn
          (setq beg (cadr module-entry))
          (setq end (cddr module-entry)))
      (if (and module
           (save-excursion
            (goto-char (cdar module-entry))
            (looking-at " *[.]")))
          (progn
            (setq beg (match-end 0))
            (setq end beg))))
    (if (not (and beg end))
        (error "Did not find anything to complete around point")

      (setq pattern (buffer-substring beg end))
      (let* ((all-completions (rml-completion pattern module))
             (completion
              (try-completion pattern (mapcar 'list all-completions))))
        (cond ((eq completion t))

              ((null completion)
               (let*
                   ((modules (rml-find-module pattern))
                    (visible (intersection modules (rml-visible-modules)))
                    (hist)
                    (module
                     (cond
                      ((null modules)
                       nil)
                      ((equal (length modules) 1)
                       (caar modules))
                      ((equal (length visible) 1)
                       (caar visible))
                      (t
                       (setq hist (mapcar 'car modules))
                       (completing-read "Module: " modules nil t
                                        "" (cons hist 0)))
                      )))
                 (if (null module)
                     (error "Can't find completion for \"%s\"" pattern)
                   (message "Completion found in module %s" module)
                   (if (and (consp module-entry) (consp (cdr module-entry)))
                       (delete-region (caar module-entry) end)
                     (delete-region beg end))
                   (insert module "." pattern))))

              ((not (string-equal pattern completion))
               (delete-region beg end)
               (goto-char beg)
               (insert completion))

              (t
                (with-output-to-temp-buffer "*Completions*"
                  (display-completion-list all-completions))
                ))
               ))))


;; Info files (only in rmldoc style)


(defvar rml-info-prefix "rml-lib"
  "Prefix of rml info files describing library modules.
Suffix .info will be added to info files.
Additional suffix .gz may be added if info files are compressed.
")
;;

(defun rml-hevea-info-add-entries (entries dir name)
  (let*
      ((filter
        (concat "-type f -regex '.*/" name
                "\\(.info\\|\\)\\(-[0-9]*\\|\\)\\([.]gz\\|\\)'"
                ))
       (section-regexp
        "\\* \\(Section [1-9][0-9--]*\\)::[ \t][ \t]*Module *\\([A-Z][A-Za-z_0-9]*\\)")
       (files (rml-find-files dir filter))
       (command))
    ;; scanning info files
    (if (or (null files)
            (not (stringp files))
            (string-match files "^ *$"))
        (message "No info file found: %s." (mapconcat 'identity files " "))
      (message "Scanning info files %s." files)
      (save-window-excursion
        (set-buffer (get-buffer-create "*rml-help*"))
        (setq command
              (concat "zcat -f " files
                      " | grep -e '" section-regexp "'"))
        (message "Scanning files with: %s" command)
        (or (shell-command command (current-buffer))
            (error "Error while scanning"))
        (goto-char (point-min))
        (while (re-search-forward section-regexp (point-max) t)
          (let* ((module (match-string 2))
                 (section (match-string 1)))
            ;; (message "%s %s" module section)
            (if (assoc module entries) nil
              (setq entries
                    (cons (cons module (concat "(" name ")" section))
                          entries))
              )))
        (let ((buf (get-buffer "*rml-help*")))
          (if buf (kill-buffer buf)))))
    entries))

(defun rml-hevea-info ()
  "The default way to create an info data base from the value
of \\[Info-default-directory-list] and the base name \\[rml-info-name]
of files to look for.

This uses info files produced by HeVeA.
"
  (let ((collect) (seen))
    (iter '(lambda (d)
             (if (member d seen) nil
               (setq collect
                     (rml-hevea-info-add-entries
                      collect d rml-info-prefix))
               (setq done (cons d seen))))
          Info-directory-list)
    collect))

(defun rml-rmldoc-info-add-entries (entries dir name)
  (let*
      ((module-regexp "^Node: \\([A-Z][A-Za-z_0-9]*\\)[^ ]")
       (command
        (concat
         "find " dir " -type f -regex '.*/" name
         "\\(.info\\|\\)\\([.]gz\\|\\)' -print0"
         " | xargs -0 zcat -f | grep '" module-regexp "'")))
    (message "Scanning info files in %s" dir)
    (save-window-excursion
      (set-buffer (get-buffer-create "*rml-help*"))
      (or (shell-command command (current-buffer)) (error "HERE"))
      (goto-char (point-min))
      (while (re-search-forward module-regexp (point-max) t)
        (if (equal (char-after (match-end 1)) 127)
            (let* ((module (match-string 1)))
              (if (assoc module entries) nil
                (setq entries
                      (cons (cons module (concat "(" name ")" module))
                            entries))
                ))))
      ; (kill-buffer (current-buffer))
      )
    entries))

(defun rml-rmldoc-info ()
  "The default way to create an info data base from the value
of \\[Info-default-directory-list] and the base name \\[rml-info-name]
of files to look for.

This uses info files produced by rmldoc."
  (require 'info)
  (let ((collect) (seen))
    (iter '(lambda (d)
             (if (member d seen) nil
               (setq collect
                     (rml-rmldoc-info-add-entries collect d
                                                      rml-info-prefix))
               (setq done (cons d seen))))
          Info-directory-list)
    collect))

;; Continuing

(defvar rml-info-alist 'rml-rmldoc-info
  "A-list binding module names to info entries:

  nil means do not use info.

  A function to build the list lazily (at the first call). The result of
the function call will be assign permanently to this variable for future
uses. We provide two default functions \\[rml-info-default-function]
(info produced by HeVeA is the default) and \\[rml-info-default-function]
(info produced by rmldoc).

  Otherwise, this value should be an alist binding module names to info
entries of the form to \"(entry)section\" be taken by the \\[info]
command. An entry may be an info module or a complete file name."
)

(defun rml-info-alist ()
  "Call by need value of variable rml-info-alist"
  (cond
   ((listp rml-info-alist))
   ((functionp rml-info-alist)
    (setq rml-info-alist (apply rml-info-alist nil)))
   (t
    (error "wrong type for rml-info-alist")))
  rml-info-alist)

;; help around point

(defun rml-find-module (symbol &optional module-list)
  (let ((list (or module-list (rml-module-alist)))
        (collect))
    (while (consp list)
      (if (member symbol (rml-module-symbols (car list)))
          (setq collect (cons (car list) collect)))
      (setq list (cdr list)))
    (nreverse collect)
    ))

(defun rml-buffer-substring (region)
  (and region (buffer-substring-no-properties (car region) (cdr region))))

;; Help function.


(defun rml-goto-help (&optional module entry)
  "Searches info manual for MODULE and ENTRY in MODULE.
If unspecified, MODULE and ENTRY are inferred from the position in the
current buffer using \\[rml-qualified-identifier]."
  (interactive)
  (let ((window (selected-window))
        (info-section (assoc module (rml-info-alist))))
    (if info-section
        (rml-info-other-window (cdr info-section))
      (rml-visible-modules)
      (let* ((module-info
              (or (assoc module (rml-module-alist))
                  (and (file-exists-p
                        (concat (rml-uncapitalize module) ".rmli"))
                       (rml-get-or-make-module module))))
             (location (cdr (cadr module-info))))
	(cond
         (location
          (view-file-other-window
           (concat location (rml-uncapitalize module) ".rmli"))
          (bury-buffer (current-buffer)))
         (info-section (error "Aborted"))
         (t (error "No help for module %s" module))))
      )
    (if (stringp entry)
        (let ((here (point))
              (case-fold-search nil))
          (goto-char (point-min))
          (if (or (re-search-forward
                   (concat "\\(val\\|exception\\|external\\|[|{;]\\) +"
                           (regexp-quote entry))
                   (point-max) t)
                  (re-search-forward
                   (concat "type [^{]*{[^}]*" (regexp-quote entry) " :")
                   (point-max) t)
                  (progn
                    (if (window-live-p window) (select-window window))
                    (error "Entry %S not found in module %S"
                           entry module))
                  ;; (search-forward entry (point-max) t)
                  )
              (recenter 1)
            (progn
              (message "Help for entry %s not found in module %s"
                       entry module)
              (goto-char here)))))
    (rml-link-activate (cdr info-section))
    (if (window-live-p window) (select-window window))
    ))

(defun rml-help (arg)
  "Find documentation for Rml qualified identifiers.

It attemps to recognize an qualified identifier of the form
``Module . entry'' around point using function `rml-qualified-identifier'.

If Module is undetermined it is temptatively guessed from the identifier name
and according to visible modules. If this is still unsucessful,  the user is
then prompted for a Module name.

The documentation for Module is first seach in the info manual if available,
then in the ``module.rmli'' source file. The entry is then searched in the documentation.

Visible modules are computed only once, at the first call.
Modules can be made visible explicitly with `rml-open-module' and
hidden with `rml-close-module'.

Prefix arg 0 forces recompilation of visible modules (and their content)
from the file content.

Prefix arg 4 prompts for Module and identifier instead of guessing values
from the possition of point in the current buffer.
"
  (interactive "p")
  (let ((module) (entry) (module-entry))

    (cond
     ((= arg 4)
      (or (and
           (setq module
                (completing-read "Module: " (rml-module-alist)
                                 nil t "" (cons 'hist 0)))
           (not (string-equal module "")))
          (error "Quit"))
      (let ((symbols
             (mapcar 'list
                     (rml-module-symbols
                      (assoc module (rml-module-alist))))))
        (setq entry (completing-read "Value: " symbols nil t)))
      (if (string-equal entry "") (setq entry nil))
      )
     (t
      (if (= arg 0) (setq rml-visible-modules 'lazy))
      (setq module-entry (rml-qualified-identifier))
      (setq entry (rml-buffer-substring (cdr module-entry)))
      (setq module
            (or (rml-buffer-substring (car module-entry))
                (let ((modules
                       (or (rml-find-module entry (rml-visible-modules))
                           (rml-find-module entry)))
                      (hist) (default))
                  (cond
                   ((null modules)
                    (error "No module found for entry %s" entry))
                   ((equal (length modules) 1)
                    (caar modules))
                   (t
                    (setq hist (mapcar 'car modules))
                    (setq default (car hist))
                    (setq module
                          (completing-read
                           (concat "Module: "
                                   (and default (concat "[" default "] ")))
                           modules nil t "" (cons 'hist 0)))
                    (if (string-equal module "") default module))
                   ))))
      ))
     (message "Help for %s%s%s" module (if entry "." "") (or entry ""))
     (rml-goto-help module entry)
     ))

;; auto-links

(defconst rml-link-regexp
  "\\(type\\|and\\) \\('[a-z] +\\|(\\('[a-z], *\\)*'[a-z])\\|\\) *\\([a-zA-Z0-9_]*\\)\\( *$\\| =\\)")
(defconst rml-longident-regexp
  "\\([A-Z][a-zA-Z_0]*\\)[.]\\([a-zA-Z][A-Za-z0-9_]*\\)")

(defvar rml-links nil
  "Local links in the current of last info node or interface file.

The car of the list is a key that indentifies the module to prevent
recompilation when next help command is relative to the same module.
The cdr is a list of elments, each of which is an string and a pair of
buffer positions."
)
(make-variable-buffer-local 'rml-links)

(defun rml-info-links (section)
  (if (and rml-links section (equal (car rml-links) section))
      (cdr rml-links)
    (save-excursion
      (goto-char (point-min))
      (let ((regexp (concat (if (equal major-mode 'Info-mode) "^ - " "^")
                            rml-link-regexp))
            (all))
        (while (re-search-forward regexp (point-max) t)
          (setq all
                (cons (cons (match-string 4)
                            (cons (match-beginning 4)
                                  (match-end 4)))
                      all)))
        (setq rml-links (cons section all))
        ))))

(defvar rml-link-map (make-sparse-keymap))
(define-key rml-link-map [mouse-2] 'rml-link-goto)

(defun rml-link-goto (click)
  (interactive "e")
  (let* ((pos (rml-event-point-start click))
         (buf (window-buffer (rml-event-window click)))
         (window (selected-window))
         (link))
    (setq link
          (with-current-buffer buf
           (buffer-substring (previous-property-change
                              pos buf (- pos 100))
                             (next-property-change
                              pos buf (+ pos 100)))))
    (if (string-match (concat "^" rml-longident-regexp "$") link)
        (rml-goto-help (match-string 1 link) (match-string 2 link))
      (if (not (equal (window-buffer window) buf))
          (switch-to-buffer-other-window buf))
      (if (setq link (assoc link (cdr rml-links)))
          (progn
            (goto-char (cadr link))
            (recenter 1)))
      (if (window-live-p window) (select-window window))
      )))

(cond
 ((and (x-display-color-p)
       (not (memq 'rml-link-face (face-list))))
  (make-face 'rml-link-face)
  (set-face-foreground 'rml-link-face "Purple")))


(defun rml-link-activate (section)
  (if (cdr (rml-info-links section))
      (let ((regexp (concat "[^A-Za-z0-9'_]\\("
                            rml-longident-regexp "\\|"
                            (mapconcat 'car (cdr rml-links) "\\|")
                            "\\)[^A-Za-z0-9'_]"))
            (case-fold-search nil))
        (goto-char (point-min))
        (unwind-protect
            (save-excursion
              (setq buffer-read-only nil)
              (goto-char (point-min))
              (while (re-search-forward regexp (point-max) t)
                (put-text-property (match-beginning 1) (match-end 1)
                                   'mouse-face 'highlight)
                (put-text-property (match-beginning 1) (match-end 1)
                                   'local-map rml-link-map)
                (if (x-display-color-p)
                    (put-text-property (match-beginning 1) (match-end 1)
                                       'face 'rml-link-face)))
              )
          (setq buffer-read-only t))
          )))



;; bindings ---now in rml.el

; (and
;  (boundp 'rml-mode-map)
;  (keymapp rml-mode-map)
;  (progn
;    (define-key rml-mode-map [?\C-c?i] 'rml-add-path)
;    (define-key rml-mode-map [?\C-c?]] 'rml-close-module)
;    (define-key rml-mode-map [?\C-c?[] 'rml-open-module)
;    (define-key rml-mode-map [?\C-c?\C-h] 'rml-help)
;    (define-key rml-mode-map [?\C-c?\t] 'rml-complete)
;    (let ((map (lookup-key rml-mode-map [menu-bar rml])))
;      (and
;       (keymapp map)
;       (progn
;         (define-key map [separator-help] '("---"))
;         (define-key map [open] '("Open add path" . rml-add-path ))
;         (define-key map [close]
;           '("Close module for help" . rml-close-module))
;         (define-key map [open] '("Open module for help" . rml-open-module))
;         (define-key map [help] '("Help for identifier" . rml-help))
;         (define-key map [complete] '("Complete identifier" . rml-complete))
;         )
;    ))))


(provide 'rml-help)
