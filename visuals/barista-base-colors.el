;;; base-colors -*- lexical-binding: t; -*-

(defgroup barista '()
  "Faces and colors for the barista emacs theme")

;; Derive our default color set from classic Emacs faces.
;; This allows dropping barista components into already themed Emacsen with varying
;; degrees of visual appeal.
;;
;; We memorize the default colorset in this var in order not to confuse
;; customize: the STANDARD argument of defcustom gets re-evaluated by customize
;; to determine if the current value is default or not.
(defvar barista-base-colors--defaults
  `((foreground . ,(face-foreground 'default nil t))
    (background . ,(face-background 'default nil t))
    (highlight . ,(face-background 'fringe nil t))
    (critical . ,(face-foreground 'error nil t))
    (salient . ,(face-foreground 'font-lock-keyword-face nil t))
    (strong . ,(face-foreground 'default nil t))
    (popout . ,(face-foreground 'font-lock-string-face nil t))
    (subtle . ,(face-background 'mode-line-inactive nil t))
    (faded . ,(face-foreground 'shadow nil t))))

(defun barista-base-colors--get (name)
  "Get default color associated with symbol NAME."
  (cdr (assoc name barista-base-colors--defaults)))

(defcustom barista-color-foreground (barista-base-colors--get 'foreground)
  ""
  :type 'color
  :group 'barista)

(defcustom barista-color-background (barista-base-colors--get 'background)
  ""
  :type 'color
  :group 'barista)

(defcustom barista-color-highlight (barista-base-colors--get 'highlight)
  ""
  :type 'color
  :group 'barista)

(defcustom barista-color-critical (barista-base-colors--get 'critical)
  ""
  :type 'color
  :group 'barista)

(defcustom barista-color-salient (barista-base-colors--get 'salient)
  ""
  :type 'color
  :group 'barista)

(defcustom barista-color-strong (barista-base-colors--get 'strong)
  ""
  :type 'color
  :group 'barista)

(defcustom barista-color-popout (barista-base-colors--get 'popout)
  ""
  :type 'color
  :group 'barista)

(defcustom barista-color-subtle (barista-base-colors--get 'subtle)
  ""
  :type 'color
  :group 'barista)

(defcustom barista-color-faded (barista-base-colors--get 'faded)
  ""
  :type 'color
  :group 'barista)

(provide 'barista-base-colors)
