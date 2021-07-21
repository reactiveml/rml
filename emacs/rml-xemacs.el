(require 'overlay)

;; for rml-help.el
(defun rml-info-other-window (arg)
  (save-excursion (info arg))
  (view-buffer-other-window "*info*"))

;; for rml-types.el
(defun rml-line-beginning-position ()
  (save-excursion (beginning-of-line) (point)))

(defun rml-event-window (e) (event-window e))
(defun rml-event-point-start (e) (event-closest-point e))
(defun rml-event-point-end (e) (event-closest-point e))
(defalias 'rml-read-event 'next-event)
(defmacro rml-track-mouse (&rest body) (cons 'progn body))

(defun mouse-movement-p (e) (equal (event-type e) 'motion))

(provide 'rml-xemacs)
