;; for rml-help.el
(defalias 'rml-info-other-window 'info-other-window)

;; for rml-types.el

(defalias 'rml-line-beginning-position 'line-beginning-position)

(defun rml-event-window (e) (posn-window (event-start e)))
(defun rml-event-point-start (e) (posn-point (event-start e)))
(defun rml-event-point-end (e) (posn-point (event-end e)))
(defalias 'rml-read-event 'read-event)
(defmacro rml-track-mouse (&rest body) (cons 'track-mouse body))

(provide 'rml-emacs)
