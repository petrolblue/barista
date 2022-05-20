;;; barista-faces --- Face settings for barista-emacs  -*- lexical-binding: t; -*-

(require 'barista-base-colors)

(defcustom barista-font-family-monospaced "PragmataPro Mono Liga 1.4"
  "Name of the font-family to use for barista.
Defaults to Roboto Mono. Customizing this might lead to conflicts
if the family does not have sufficient bold/light etc faces."
  :group 'barista
  :type 'string)

(defcustom barista-font-family-proportional nil
  "Font to use for variable pitch faces.
Setting this allows barista to display variable pitch faces when,
for instance, 'variable-pitch-mode' or 'mixed-pitch-mode' is active in a buffer.
Defaults to nil."
  :group 'barista
  :type 'string)

(defcustom barista-font-size 14
  "Default value for the font size of barista-theme in pt units.
Note: to change this after startup, call
\(barista-faces\) and \(barista-themes\)."
  :group 'barista
  :type 'integer)

;; A theme is fully defined by these seven faces

(defface barista-face-default nil
  "Default face is used for regular information."
  :group 'barista)

(defface barista-face-variable-pitch nil
  "Default variable-pitch face is used for variable pitch mode."
  :group 'barista)

(defface barista-face-critical nil
  "Critical face is for information that requires immediate action.
It should be of high constrast when compared to other faces. This
can be realized (for example) by setting an intense background
color, typically a shade of red. It must be used scarcely."
  :group 'barista)

(defface barista-face-popout nil
  "Popout face is used for information that needs attention.
To achieve such effect, the hue of the face has to be
sufficiently different from other faces such that it attracts
attention through the popout effect."
  :group 'barista)

(defface barista-face-strong nil
  "Strong face is used for information of a structural nature.
It has to be the same color as the default color and only the
weight differs by one level (e.g., light/regular or
regular/bold). IT is generally used for titles, keywords,
directory, etc."
  :group 'barista)

(defface barista-face-salient nil
  "Salient face is used for information that are important.
To suggest the information is of the same nature but important,
the face uses a different hue with approximately the same
intensity as the default face. This is typically used for links."
  :group 'barista)

(defface barista-face-faded nil
  "Faded face is for information that are less important.
It is made by using the same hue as the default but with a lesser
intensity than the default. It can be used for comments,
secondary information and also replace italic (which is generally
abused anyway)."
  :group 'barista)

(defface barista-face-subtle nil
  "Subtle face is used to suggest a physical area on the screen.
It is important to not disturb too strongly the reading of
information and this can be made by setting a very light
background color that is barely perceptible."
  :group 'barista)

(defface barista-face-region nil
  "Region face, used when we select something."
  :group 'barista)

(defface barista-face-function nil
  "Highlight that functions receive."
  :group 'barista)

(defface barista-face-comment nil
  "Highlight that comments receive."
  :group 'barista)

(defface barista-face-header-default nil
  "Default face for ther header line."
  :group 'barista)

(defface barista-face-header-critical nil
  "Critical face for ther header line."
  :group 'barista)

(defface barista-face-header-popout nil
  "Popout face for ther header line."
  :group 'barista)

(defface barista-face-header-strong nil
  "Strong face for ther header line."
  :group 'barista)

(defface barista-face-header-salient nil
  "Salient face for ther header line."
  :group 'barista)

(defface barista-face-header-faded nil
  "Faded face for ther header line."
  :group 'barista)

(defface barista-face-header-subtle nil
  "Subtle face for ther header line."
  :group 'barista)

(defface barista-face-header-highlight nil
  "Highlight face for ther header line."
  :group 'barista)

(defface barista-face-header-separator nil
  "Face for separating item in the header line (internal use)"
  :group 'barista)

(defface barista-face-header-filler nil
  "Face compsenting spaces in the header line (internal use) "
  :group 'barista)

(defface barista-face-tag-default nil
  "Default face for tags"
  :group 'barista)

(defface barista-face-tag-faded nil
  "Faded face for tags"
  :group 'barista)

(defface barista-face-tag-strong nil
  "Strong face for tags"
  :group 'barista)

(defface barista-face-tag-salient nil
  "Salient face for tags"
  :group 'barista)

(defface barista-face-tag-popout nil
  "Popout face for tags"
  :group 'barista)

(defface barista-face-tag-critical nil
  "Critical face for tags"
  :group 'barista)

(defun barista-what-faces (pos)
  "Get the font faces at POS."
  (interactive "d")
  (let ((faces (remq nil
                     (list
                      (get-char-property pos 'read-face-name)
                      (get-char-property pos 'face)
                      (plist-get (text-properties-at pos) 'face)))))
    (message "Faces: %s" faces)))

(defun barista-faces ()
  "Derive face attributes for barista-faces using barista-theme values."
  (set-face-attribute 'barista-face-default nil
                      :foreground barista-color-foreground
                      :background barista-color-background
                      :family     barista-font-family-monospaced
                      :height       (* barista-font-size 10))
  (set-face-attribute 'barista-face-critical nil
                      :foreground barista-color-foreground
                      :background barista-color-critical)
  (set-face-attribute 'barista-face-popout nil
                      :foreground barista-color-popout)

  (set-face-attribute 'barista-face-variable-pitch nil
                          :foreground (face-foreground 'barista-face-default)
                          :background (face-background 'barista-face-default)
                          :family barista-font-family-proportional
                          :height (* barista-font-size 10))
  (if (display-graphic-p)
      (set-face-attribute 'barista-face-strong nil
                          :foreground (face-foreground 'barista-face-default)
                          :weight 'bold)
    (set-face-attribute 'barista-face-strong nil
                        :foreground (face-foreground 'barista-face-default)
                        :weight 'bold))

  (set-face-attribute 'barista-face-salient nil
                      :foreground barista-color-salient
                      :weight 'light)

  (set-face-attribute 'barista-face-faded nil
                      :foreground barista-color-faded
                      :weight 'light)

  (set-face-attribute 'barista-face-subtle nil
                      :background barista-color-subtle)

  (set-face-attribute 'barista-face-region nil
                      :background barista-color-region)

  (set-face-attribute 'barista-face-function nil
                      :background barista-color-function)

  (set-face-attribute 'barista-face-comment nil
                      :background barista-color-comment)

  (set-face-attribute 'barista-face-header-default nil
                      :foreground barista-color-foreground
                      :background barista-color-subtle
                      :box `(:line-width 1
                                         :color ,barista-color-background
                                         :style nil))

  (set-face-attribute 'barista-face-tag-default nil
                      :foreground barista-color-foreground
                      :background barista-color-background
                      :weight 'regular
                      :height (if (display-graphic-p)
                                  (round
                                   (* 0.85 (* 10 barista-font-size)))
                                                1)
                      :box `(:line-width 1
                                         :color ,barista-color-foreground
                                         :style nil))

  (set-face-attribute 'barista-face-header-strong nil
                      :foreground barista-color-strong
                      :background barista-color-subtle
                      :inherit 'barista-face-strong
                      :box `(:line-width 1
                                         :color ,barista-color-background
                                         :style nil))

  (set-face-attribute 'barista-face-tag-strong nil
                      :foreground barista-color-strong
                      :background barista-color-subtle
                      :weight 'regular
                      :height (if (display-graphic-p)
                                  (round
                                   (* 0.85 (* 10 barista-font-size)))
                                                1)
                      :box `(:line-width 1
                                         :color ,barista-color-strong
                                         :style nil))

  (set-face-attribute 'barista-face-header-salient nil
                      :foreground barista-color-background
                      :background barista-color-salient
                      :box `(:line-width 1
                                         :color ,barista-color-background
                                         :style nil))

  (set-face-attribute 'barista-face-tag-salient nil
                      :foreground barista-color-background
                      :background barista-color-salient
                      :weight 'regular
                      :height (if (display-graphic-p)
                                  (round
                                   (* 0.85 (* 10 barista-font-size)))
                                                1)
                      :box `(:line-width 1
                                         :color ,barista-color-salient
                                         :style nil))

  (set-face-attribute 'barista-face-header-popout nil
                      :foreground barista-color-background
                      :background barista-color-popout
                      :box `(:line-width 1
                                         :color ,barista-color-background
                                         :style nil))

  (set-face-attribute 'barista-face-tag-popout nil
                      :foreground barista-color-background
                      :background barista-color-popout
                      :weight 'regular
                      :height (if (display-graphic-p)
                                  (round
                                   (* 0.85 (* 10 barista-font-size)))
                                                1)
                      :box `(:line-width 1
                                         :color ,barista-color-popout
                                         :style nil))

  (set-face-attribute 'barista-face-header-faded nil
                      :foreground barista-color-background
                      :background barista-color-faded
                      :box `(:line-width 1
                                         :color ,barista-color-background
                                         :style nil))

  (set-face-attribute 'barista-face-tag-faded nil
                      :foreground barista-color-background
                      :background barista-color-faded
                      :weight 'regular
                      :height (if (display-graphic-p)
                                  (round
                                   (* 0.85 (* 10 barista-font-size)))
                                                1)
                      :box `(:line-width 1
                                         :color ,barista-color-faded
                                         :style nil))

  (set-face-attribute 'barista-face-header-subtle nil)

  (set-face-attribute 'barista-face-header-critical nil
                      :foreground barista-color-background
                      :background barista-color-critical
                      :box `(:line-width 1
                                         :color ,barista-color-background
                                         :style nil))
  (set-face-attribute 'barista-face-tag-critical nil
                      :foreground barista-color-background
                      :background barista-color-critical
                      :weight 'regular
                      :height (if (display-graphic-p)
                                  (round
                                   (* 0.85 (* 10 barista-font-size)))
                                                1)
                      :box `(:line-width 1
                                         :color ,barista-color-critical
                                         :style nil))

  (set-face-attribute 'barista-face-header-separator nil
                      :inherit 'barista-face-default
                      :height 0.1)
  (set-face-attribute 'barista-face-header-filler nil
                      :inherit 'barista-face-header-default
                      :height 0.1)
  (set-face-attribute 'barista-face-header-highlight nil
                      :inherit 'barista-face-header-faded
                      :box nil))

 (defun barista-theme--flyspell ()
  "Derive flyspell faces from barista faces."
  (with-eval-after-load 'flyspell
    (set-face 'flyspell-duplicate                     'barista-face-default)
    (set-face-attribute 'flyspell-incorrect nil
                  :inherit 'barista-face-critical)))

(defun barista-theme--flycheck ()
  "Derive flycheck faces from barista faces."
  (with-eval-after-load 'flycheck
    (set-face-attribute 'flycheck-error nil
                        :underline `(:style wave :color ,barista-color-critical)    :background barista-color-background)
    (set-face-attribute 'flycheck-warning nil
                        :underline `(:style wave :color ,barista-color-critical)
                        :background barista-color-background)
    (set-face-attribute 'flycheck-info  nil
                        :underline `(:style wave :color ,barista-color-faded)
                        :background barista-color-background)))

(defun barista-theme--company ()
  "Derive company faces from barista faces."
  (with-eval-after-load 'company
    (set-face 'company-echo-common              'barista-face-faded)
    (set-face 'company-preview-common           'barista-face-default)
    (set-face 'company-preview-search           'barista-face-faded)
    (set-face 'company-scrollbar-bg             'barista-face-default)
    (set-face 'company-scrollbar-fg             'barista-face-default)
    (set-face 'company-tooltip-annotation       'barista-face-faded)
    (set-face 'company-tooltip-common           'barista-face-faded)
    (set-face-attribute 'company-tooltip-common-selection nil
                        :background barista-color-background
                        :foreground barista-color-critical :weight 'bold)
    (set-face-attribute 'company-tooltip-selection nil
                        :background barista-color-background
                        :foreground barista-color-critical)
    (set-face-attribute 'company-preview nil
                        :background barista-color-background
                        :foreground barista-color-foreground)
    (set-face-attribute 'company-tooltip nil
                        :background barista-color-background
                        :foreground barista-color-foreground)))

(defun barista-theme--ctrlf ()
  "Derive ctrlf faces from barista faces."
  (with-eval-after-load 'ctrlf
    (set-face 'ctrlf-highlight-active 'barista-face-strong)
    (set-face 'ctrlf-highlight-line   'barista-face-highlight)))

(defun barista-theme--rainbow-delimiters ()
  "Derive rainbow-delimiters faces from barista faces."
  (with-eval-after-load 'rainbow-delimiters
    (set-face-attribute 'rainbow-delimiters-depth-1-face nil
                        :foreground barista-color-critical)
    (set-face-attribute 'rainbow-delimiters-depth-2-face nil
                        :foreground barista-color-foreground)
    (set-face-attribute 'rainbow-delimiters-depth-3-face nil
                        :foreground barista-color-foreground)
    (set-face-attribute 'rainbow-delimiters-depth-4-face nil
                        :foreground barista-color-foreground)
    (set-face-attribute 'rainbow-delimiters-depth-5-face nil
                        :foreground barista-color-foreground)
    (set-face-attribute 'rainbow-delimiters-depth-6-face nil
                        :foreground barista-color-foreground)
    (set-face-attribute 'rainbow-delimiters-depth-7-face nil
                        :foreground barista-color-foreground)
    (set-face-attribute 'rainbow-delimiters-depth-8-face nil
                        :foreground barista-color-foreground)
    (set-face-attribute 'rainbow-delimiters-depth-9-face nil
                        :foreground barista-color-foreground)
    (set-face-attribute 'rainbow-delimiters-base-error-face nil
                        :foreground barista-color-critical
                        :background barista-color-background :weight 'bold)))


(provide 'barista-faces)
;;; barista-faces.el ends here
