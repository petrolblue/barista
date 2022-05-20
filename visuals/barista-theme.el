;;; -*- lexical-binding: t; -*-

(require 'barista-faces)

;; When we set a face, we take care of removing any previous settings
(defun set-face (face style)
  "Reset FACE and make it inherit STYLE."
  (set-face-attribute face nil
                      :foreground 'unspecified :background 'unspecified
                      :family     'unspecified :slant      'unspecified
                      :weight     'unspecified :height     'unspecified
                      :underline  'unspecified :overline   'unspecified
                      :box        'unspecified :inherit    style))

(defun barista-theme--basics ()
  "Derive basic Emacs faces from barista-faces and barista-color-theme."
  (set-foreground-color barista-color-foreground)
  (set-background-color barista-color-background)

  ;; THIS
  (set-face-attribute 'default nil
                      :foreground (face-foreground 'default)
                      :background (face-background 'default)
                      :weight     'regular
                      :family     (face-attribute 'barista-face-default :family)
                      :height     (face-attribute 'barista-face-default :height))
  (if (display-graphic-p)
      (set-face-attribute 'bold nil :weight 'regular)
    (set-face-attribute 'bold nil :weight 'bold))

  ;; Structural
  (set-face 'bold                                     'barista-face-strong)
  (set-face 'italic                                    'barista-face-faded)
  (set-face 'bold-italic                              'barista-face-strong)
  (set-face-attribute 'region nil
                      :background barista-color-region
                      :foreground barista-color-foreground)
  (set-face 'highlight                                'barista-face-subtle)
  (set-face 'fixed-pitch-serif                       'barista-face-default)
  (set-face 'cursor                                  'barista-face-default)
  (if 'barista-font-family-proportional
      (set-face-attribute 'variable-pitch nil ;; to work with mixed-pitch
                :foreground (face-foreground 'default)
                :background (face-background 'default)
                :family     (face-attribute 'barista-face-variable-pitch :family)
                :height     (face-attribute 'barista-face-variable-pitch :height)
                :weight     'light)
      (set-face 'variable-pitch                     'barista-face-default))

  (set-face-attribute 'cursor nil
                      :background barista-color-popout)
  (set-face-attribute 'window-divider nil
                      :foreground (face-background 'barista-face-default))
  (set-face-attribute 'window-divider-first-pixel nil
                      :foreground barista-color-background)
  (set-face-attribute 'window-divider-last-pixel nil
                      :foreground barista-color-background)
  (set-face-foreground 'vertical-border barista-color-subtle)

  ;; Semantic
  (set-face 'shadow                                    'barista-face-faded)
  (set-face 'success                                 'barista-face-salient)
  (set-face 'warning                                  'barista-face-popout)
  (set-face 'error                                  'barista-face-critical)
  (set-face 'match                                    'barista-face-popout)
  
  ;; General
  (set-face 'buffer-menu-buffer                       'barista-face-strong)
  (set-face 'minibuffer-prompt                        'barista-face-strong)
  (set-face 'link                                    'barista-face-salient)
  (set-face 'fringe                                    'barista-face-faded)
  (set-face-attribute 'fringe nil
                      :foreground (face-background 'barista-face-subtle)
                      :background (face-background 'default))
  (set-face 'isearch                                  'barista-face-strong)
  (set-face 'isearch-fail                              'barista-face-faded)
  (set-face 'lazy-highlight                           'barista-face-subtle)
  (set-face 'trailing-whitespace                      'barista-face-subtle)
  (set-face-attribute 'show-paren-match nil
                      :foreground barista-color-salient
                      :background barista-color-background
                      :underline t)
  (set-face 'show-paren-mismatch                           'face-normal)
  (set-face-attribute 'tooltip nil                         :height 0.85)
  (set-face 'secondary-selection                      'barista-face-subtle)
  (set-face 'completions-common-part                   'barista-face-faded)
  (set-face 'completions-first-difference            'barista-face-default))

(defun barista-theme--font-lock ()
  "Derive font-lock faces from barista-faces."
  (set-face-attribute 'font-lock-comment-face nil
                      :background barista-color-comment
                      :foreground barista-color-foreground)
  (set-face-attribute 'font-lock-doc-face nil
                      :background barista-color-comment
                      :foreground barista-color-foreground
                      :bold 'nil)
  (set-face-attribute 'font-lock-string-face nil
                      :background barista-color-subtle
                      :foreground barista-color-foreground)
  (set-face-attribute 'font-lock-constant-face nil
                      :background barista-color-subtle
                      :foreground barista-color-foreground)
  (set-face-attribute 'font-lock-warning-face nil
                      :background barista-color-background
                      :foreground barista-color-critical)
  (set-face-attribute 'font-lock-function-name-face nil
                      :background barista-color-function
                      :foreground barista-color-foreground)
  (set-face-attribute 'font-lock-variable-name-face nil
                      :background barista-color-subtle
                      :foreground barista-color-foreground)
  (set-face-attribute 'font-lock-builtin-face nil
                      :background barista-color-subtle
                      :foreground barista-color-foreground)
  (set-face-attribute 'font-lock-type-face nil
                      :background barista-color-subtle
                      :foreground barista-color-foreground)
  (set-face-attribute 'font-lock-keyword-face nil
                      :background barista-color-subtle
                      :foreground barista-color-foreground))

(defun barista-theme--mode-line ()
  "Derive mode-line and header-line faces from barista-faces."
  (set-face-attribute 'mode-line nil
                      :foreground barista-color-faded
                      :background barista-color-background
                      :weight 'bold
                      :box `(:line-width 1
                             :color ,(face-background 'barista-face-default)
                             :style nil))
  (set-face-attribute 'mode-line-inactive nil
                      :foreground barista-color-strong
                      :background barista-color-background
                      :box `(:line-width 1
                      :color ,(face-background 'barista-face-default)
                      :style nil))
  (set-face-attribute 'mode-line-buffer-id nil
                      :foreground barista-color-strong
                      :bold t)
  (set-face-attribute 'mode-line-emphasis nil
                      :foreground barista-color-strong
                      :bold t)
  (set-face-attribute 'mode-line-highlight nil
                      :foreground barista-color-strong)
  (set-face-attribute 'header-line nil
                      :foreground (face-foreground 'barista-face-faded)
                      :background (face-background 'barista-face-default)
                      :weight 'bold
                      :overline nil
                      :underline nil
                      :box `(:line-width 1
                                         :color ,(face-background 'barista-face-default)
                                         :style nil)
                      :inherit nil)
  (set-face-attribute 'internal-border nil
                       :background (face-background 'barista-face-default)))


(defun barista-theme--minibuffer ()
  "Derive minibuffer / echo area faces from barista faces."
  ;; Minibuffer / echo area
  (dolist (buffer (list " *Minibuf-0*" " *Echo Area 0*"
                        " *Minibuf-1*" " *Echo Area 1*"))
    (when (get-buffer buffer)
      (with-current-buffer buffer
        (face-remap-add-relative 'default 'barista-face-faded)))))


(defun barista-theme--hl-line ()
  "Derive hl-line faces from barista faces."
  (with-eval-after-load 'hl-line
    (set-face-attribute 'hl-line nil
                        :background barista-color-highlight)))

(defun barista-theme--buttons ()
  "Derive button faces from barista faces."
  ;; Buttons
  (with-eval-after-load 'cus-edit
    (set-face-attribute 'custom-button nil
                         :foreground (face-foreground 'barista-face-faded)
                         :background (face-background 'barista-face-default)
                         :box `(:line-width 1
                                :color ,(face-foreground 'barista-face-faded)
                                :style nil))
    (set-face-attribute 'custom-button-mouse nil
                         ;;                      :inherit 'custom-button
                         :foreground (face-foreground 'barista-face-faded)
                         :background (face-background 'barista-face-subtle)
                         :box `(:line-width 1
                                            :color ,(face-foreground 'barista-face-faded)
                                            :style nil))
    (set-face-attribute 'custom-button-pressed nil
                         :foreground (face-background 'default)
                         :background (face-foreground 'barista-face-salient)
                         :inherit 'barista-face-salient
                         :box `(:line-width 1
                                            :color ,(face-foreground 'barista-face-salient)
                                            :style nil)
                         :inverse-video nil)))

(defun barista-theme--info ()
  "Derive info faces from barista faces."
  (with-eval-after-load 'info
    (set-face 'info-menu-header                       'barista-face-strong)
    (set-face 'info-header-node                      'barista-face-default)
    (set-face 'info-index-match                      'barista-face-salient)
    (set-face 'Info-quoted                             'barista-face-faded)
    (set-face 'info-title-1                           'barista-face-strong)
    (set-face 'info-title-2                           'barista-face-strong)
    (set-face 'info-title-3                           'barista-face-strong)
    (set-face 'info-title-4                           'barista-face-strong)))


(defun barista-theme--speedbar ()
  "Derive speedbar faces from barista faces "
  (with-eval-after-load 'speedbar
    (set-face 'speedbar-button-face                    'barista-face-faded)
    (set-face 'speedbar-directory-face                'barista-face-strong)
    (set-face 'speedbar-file-face                    'barista-face-default)
    (set-face 'speedbar-highlight-face             'barista-face-highlight)
    (set-face 'speedbar-selected-face                 'barista-face-subtle)
    (set-face 'speedbar-separator-face                 'barista-face-faded)
    (set-face 'speedbar-tag-face                       'barista-face-faded)))


(defun barista-theme--bookmark ()
  "Derive bookmark faces from barista faces."
  (with-eval-after-load 'bookmark
    (set-face 'bookmark-menu-heading                  'barista-face-strong)
    (set-face 'bookmark-menu-bookmark                'barista-face-salient)))


(defun barista-theme--message ()
  "Derive message faces from barista faces."
  (with-eval-after-load 'message
    (unless (version< emacs-version "27.0")
      (set-face 'message-cited-text-1                  'barista-face-faded)
      (set-face 'message-cited-text-2                  'barista-face-faded)
      (set-face 'message-cited-text-3                  'barista-face-faded)
      (set-face 'message-cited-text-4                 'barista-face-faded))
    (set-face 'message-cited-text                      'barista-face-faded)
    (set-face 'message-header-cc                     'barista-face-default)
    (set-face 'message-header-name                    'barista-face-strong)
    (set-face 'message-header-newsgroups             'barista-face-default)
    (set-face 'message-header-other                  'barista-face-default)
    (set-face 'message-header-subject                'barista-face-salient)
    (set-face 'message-header-to                     'barista-face-salient)
    (set-face 'message-header-xheader                'barista-face-default)
    (set-face 'message-mml                            'barista-face-popout)
    (set-face 'message-separator                       'barista-face-faded)))


(defun barista-theme--outline ()
  "Derive outline faces from barista faces."
  (with-eval-after-load 'outline
    (set-face 'outline-1                              'barista-face-strong)
    (set-face 'outline-2                              'barista-face-strong)
    (set-face 'outline-3                              'barista-face-strong)
    (set-face 'outline-4                              'barista-face-strong)
    (set-face 'outline-5                              'barista-face-strong)
    (set-face 'outline-6                              'barista-face-strong)
    (set-face 'outline-7                              'barista-face-strong)
    (set-face 'outline-8                              'barista-face-strong)))


(defun barista-theme--customize ()
  "Derive customize faces from barista faces."
  (with-eval-after-load 'cus-edit
    (set-face 'widget-field                           'barista-face-subtle)
    (set-face 'widget-button                          'barista-face-strong)
    (set-face 'widget-single-line-field               'barista-face-subtle)
    (set-face 'custom-group-subtitle                  'barista-face-strong)
    (set-face 'custom-group-tag                       'barista-face-strong)
    (set-face 'custom-group-tag-1                     'barista-face-strong)
    (set-face 'custom-comment                          'barista-face-faded)
    (set-face 'custom-comment-tag                      'barista-face-faded)
    (set-face 'custom-changed                        'barista-face-salient)
    (set-face 'custom-modified                       'barista-face-salient)
    (set-face 'custom-face-tag                        'barista-face-strong)
    (set-face 'custom-variable-tag                    'barista-face-strong)
    (set-face 'custom-invalid                         'barista-face-popout)
    (set-face 'custom-visibility                     'barista-face-salient)
    (set-face 'custom-state                          'barista-face-salient)
    (set-face 'custom-link                           'barista-face-salient)))

(defun barista-theme--package ()
  "Derive package faces from barista faces."
  (with-eval-after-load 'package
    (set-face 'package-description                   'barista-face-default)
    (set-face 'package-help-section-name             'barista-face-default)
    (set-face 'package-name                          'barista-face-salient)
    (set-face 'package-status-avail-obso               'barista-face-faded)
    (set-face 'package-status-available              'barista-face-default)
    (set-face 'package-status-built-in               'barista-face-salient)
    (set-face 'package-status-dependency             'barista-face-salient)
    (set-face 'package-status-disabled                 'barista-face-faded)
    (set-face 'package-status-external               'barista-face-default)
    (set-face 'package-status-held                   'barista-face-default)
    (set-face 'package-status-incompat                 'barista-face-faded)
    (set-face 'package-status-installed              'barista-face-salient)
    (set-face 'package-status-new                    'barista-face-default)
    (set-face 'package-status-unsigned               'barista-face-default))

  ;; Button face is hardcoded, we have to redefine the relevant
  ;; function
  (defun package-make-button (text &rest properties)
    "Insert button labeled TEXT with button PROPERTIES at point.
PROPERTIES are passed to `insert-text-button', for which this
function is a convenience wrapper used by `describe-package-1'."
    (let ((button-text (if (display-graphic-p)
                           text (concat "[" text "]")))
          (button-face (if (display-graphic-p)
                           `(:box `(:line-width 1
                                    :color ,barista-color-subtle
                                    :style nil)
                                  :foreground ,barista-color-faded
                                  :background ,barista-color-subtle)
                         'link)))
      (apply #'insert-text-button button-text
               'face button-face 'follow-link t properties))))

(defun barista-theme--hydra ()
  "Derive hydra faces from barista faces."
  (with-eval-after-load 'hydra
    (set-face 'hydra-face-amaranth 'barista-face-strong)
    (set-face 'hydra-face-blue     'barista-face-strong)
    (set-face 'hydra-face-pink     'barista-face-strong)
    (set-face 'hydra-face-red      'barista-face-strong)
    (set-face 'hydra-face-teal     'barista-face-strong)))

(defun barista-theme--flyspell ()
  "Derive flyspell faces from barista faces."
  (with-eval-after-load 'flyspell
    (set-face 'flyspell-duplicate                     'barista-face-default)
    (set-face-attribute 'flyspell-incorrect nil
                        :underline `(:style wave :color ,barista-color-critical)
                        :background barista-color-background)))

(defun barista-theme--parinfer-rust ()
  "Derive parinfer-rust faces from barista-faces"
  (with-eval-after-load 'parinfer-rust
    (set-face-attribute 'parinfer-rust-dim-parens nil
                        :background barista-color-background
                        :foreground barista-color-foreground)))


(defun barista-theme--rainbow-delimiters ()
  "Derive rainbow-delimiters faces from barista faces."
  (with-eval-after-load 'rainbow-delimiters
    (set-face-attribute 'rainbow-delimiters-depth-1-face nil
                        :foreground barista-color-strong
                        :weight 'bold)
    (set-face-attribute 'rainbow-delimiters-depth-2-face nil
                        :foreground barista-color-faded)
    (set-face-attribute 'rainbow-delimiters-depth-3-face nil
                        :foreground barista-color-numbers)
    (set-face-attribute 'rainbow-delimiters-depth-4-face nil
                        :foreground barista-color-critical)
    (set-face-attribute 'rainbow-delimiters-depth-5-face nil
                        :foreground barista-color-salient)
    (set-face-attribute 'rainbow-delimiters-depth-6-face nil
                        :foreground barista-color-strong)
    (set-face-attribute 'rainbow-delimiters-base-error-face nil
                        :foreground barista-color-critical
                        :background barista-color-background
                        :weight 'bold)))


(defun barista-theme--cider-overlay ()
  "Derive cider-overlay face from barista faces"
  (with-eval-after-load 'cider
    (set-face-attribute 'cider-result-overlay-face nil
			            :background "#defbe6"
                        :foreground "#198038"
                        :weight 'bold
                        :box nil)))


(defun barista-theme--flycheck ()
  "Derive flycheck faces from barista faces."
  (with-eval-after-load 'flycheck
    (set-face-attribute 'flycheck-error nil
                        :underline `(:style wave :color ,barista-color-critical)
                        :background barista-color-background)
    (set-face-attribute 'flycheck-warning nil
                        :underline `(:style wave :color ,barista-color-critical)
                        :background barista-color-background)
    (set-face-attribute 'flycheck-info  nil
                        :underline `(:style wave :color ,barista-color-faded)
                        :background barista-color-background)
    (set-face-attribute 'flycheck-error-list-error  nil
                        :background barista-color-background
                        :foreground barista-color-critical)))

(defun barista-theme--company ()
  "Derive company faces from barista faces."
  (with-eval-after-load 'company
    (set-face 'company-echo-common              'barista-face-faded)
    (set-face-attribute 'company-preview-common  nil
                        :background "#ffffff")
    (set-face 'company-preview-search           'barista-face-faded)
    (set-face-attribute 'company-scrollbar-bg nil
                        :background "#ffffff")
    (set-face-attribute 'company-scrollbar-fg nil
                        :background "#ffffff")
    (set-face 'company-tooltip-annotation       'barista-face-faded)
    (set-face 'company-tooltip-common           'barista-face-faded)
    (set-face-attribute 'company-tooltip-common-selection nil
                        :background "#ffffff"
                        :foreground barista-color-popout
                        :weight 'bold)
    (set-face-attribute 'company-tooltip-selection nil
                        :background "#ffffff"
                        :foreground barista-color-popout)
    (set-face-attribute 'company-preview nil
                        :background "#ffffff"
                        :foreground barista-color-foreground)
    (set-face-attribute 'company-tooltip nil
                        :background "#ffffff"
                        :foreground barista-color-foreground)))

(defun barista-theme--ctrlf ()
  "Derive ctrlf faces from barista faces."
  (with-eval-after-load 'ctrlf
    (set-face-attribute 'ctrlf-highlight-active nil
                        :background barista-color-function
                        :foreground barista-color-foreground
                        :box `(:line-width 1
                               :color ,barista-color-popout
                               :style nil))
    (set-face-attribute 'ctrlf-highlight-line nil
                        :background barista-color-highlight)
    (set-face-attribute 'ctrlf-highlight-passive nil
                        :background barista-color-background
                        :foreground barista-color-foreground
                        :box `(:line-width 1
                               :color ,barista-color-comment
                               :style nil))))

(defun barista-theme--numbers-mode ()
  "Derive highlight-numbers-mode faces from barista face."
  (with-eval-after-load 'highlight-numbers
    (set-face-attribute 'highlight-numbers-number nil
                        :background barista-color-background
                        :foreground barista-color-numbers)))


(defun barista-theme--ido ()
  "Derive ido faces from barista faces."
  (with-eval-after-load 'ido
    (set-face 'ido-first-match                       'barista-face-salient)
    (set-face 'ido-only-match                          'barista-face-faded)
    (set-face 'ido-subdir                             'barista-face-strong)))

(defun barista-theme--diff ()
  "Derive diff faces from barista faces."
  (with-eval-after-load 'diff-mode
    (set-face 'diff-header                             'barista-face-faded)
    (set-face 'diff-file-header                       'barista-face-strong)
    (set-face 'diff-context                          'barista-face-default)
    (set-face 'diff-removed                            'barista-face-faded)
    (set-face 'diff-changed                           'barista-face-popout)
    (set-face 'diff-added                            'barista-face-salient)
    (set-face 'diff-refine-added                    '(barista-face-salient
                                                      barista-face-strong))
    (set-face 'diff-refine-changed                    'barista-face-popout)
    (set-face 'diff-refine-removed                    'barista-face-faded)
    (set-face-attribute     'diff-refine-removed nil :strike-through t)))


(defun barista-theme--term ()
  "Derive term faces from barista faces, and material theme colors."
  (with-eval-after-load 'term
    ;; (setq eterm-256color-disable-bold nil)
    (set-face 'term-bold                              'barista-face-strong)
    (set-face-attribute 'term-color-black nil
                         :foreground (face-foreground 'barista-face-default)
                         :background (face-foreground 'barista-face-default))
    (set-face-attribute 'term-color-white nil
                         :foreground (face-background 'barista-face-default)
                         :background (face-background 'barista-face-default))
    (set-face-attribute 'term-color-blue nil
                         :foreground barista-color-foreground
                         :background barista-color-foreground)
    (set-face-attribute 'term-color-cyan nil
                         :foreground barista-color-foreground
                         :background barista-color-foreground)
    (set-face-attribute 'term-color-green nil
                         :foreground barista-color-salient
                         :background barista-color-salient)
    (set-face-attribute 'term-color-magenta nil
                         :foreground barista-color-salient
                         :background barista-color-salient)
    (set-face-attribute 'term-color-red nil
                         :foreground barista-color-critical
                         :background barista-color-critical)
    (set-face-attribute 'term-color-yellow nil
                         :foreground barista-color-critical
                         :background barista-color-critical)))

(defun barista-theme--calendar ()
  "Derive calendar faces from barista faces."
  (with-eval-after-load 'calendar
    (set-face 'calendar-today                         'barista-face-strong)))


(defun barista-theme--agenda ()
  "Derive agenda faces from barista faces."
  (with-eval-after-load 'org-agenda
    (set-face 'org-agenda-calendar-event             'barista-face-default)
    (set-face 'org-agenda-calendar-sexp              'barista-face-salient)
    (set-face 'org-agenda-clocking                     'barista-face-faded)
    (set-face 'org-agenda-column-dateline              'barista-face-faded)
    (set-face 'org-agenda-current-time                'barista-face-strong)
    (set-face 'org-agenda-date                       'barista-face-salient)
    (set-face 'org-agenda-date-today                '(barista-face-salient
                                                       barista-face-strong))
    (set-face 'org-agenda-date-weekend                 'barista-face-faded)
    (set-face 'org-agenda-diary                        'barista-face-faded)
    (set-face 'org-agenda-dimmed-todo-face             'barista-face-faded)
    (set-face 'org-agenda-done                         'barista-face-faded)
    (set-face 'org-agenda-filter-category              'barista-face-faded)
    (set-face 'org-agenda-filter-effort                'barista-face-faded)
    (set-face 'org-agenda-filter-regexp                'barista-face-faded)
    (set-face 'org-agenda-filter-tags                  'barista-face-faded)
    ;;  (set-face 'org-agenda-property-face                'barista-face-faded)
    (set-face 'org-agenda-restriction-lock             'barista-face-faded)
    (set-face 'org-agenda-structure                   'barista-face-strong)))


(defun barista-theme--org ()
  "Derive org faces from barista faces."
  (with-eval-after-load 'org
    (set-face 'org-archived                            'barista-face-faded)
    (set-face 'org-block                                       'hl-line)
    (set-face 'org-block-begin-line                    'barista-face-faded)
    (set-face 'org-block-end-line                      'barista-face-faded)
    (unless (version< emacs-version "27.0")
      (set-face-attribute 'org-block nil                      :extend t)
      (set-face-attribute 'org-block-begin-line nil           :extend t)
      (set-face-attribute 'org-block-end-line nil             :extend t))
    (set-face 'org-checkbox                            'barista-face-faded)
    (set-face 'org-checkbox-statistics-done            'barista-face-faded)
    (set-face 'org-checkbox-statistics-todo            'barista-face-faded)
    (set-face 'org-clock-overlay                       'barista-face-faded)
    (set-face 'org-code                                'barista-face-faded)
    (set-face 'org-column                              'barista-face-faded)
    (set-face 'org-column-title                        'barista-face-faded)
    (set-face 'org-date                                'barista-face-faded)
    (set-face 'org-date-selected                       'barista-face-faded)
    (set-face 'org-default                             'barista-face-faded)
    (set-face 'org-document-info                       'barista-face-faded)
    (set-face 'org-document-info-keyword               'barista-face-faded)
    (set-face 'org-document-title                      'barista-face-faded)
    (set-face 'org-done                              'barista-face-default)
    (set-face 'org-drawer                              'barista-face-faded)
    (set-face 'org-ellipsis                            'barista-face-faded)
    (set-face 'org-footnote                            'barista-face-faded)
    (set-face 'org-formula                             'barista-face-faded)
    (set-face 'org-headline-done                       'barista-face-faded)
    (set-face 'org-latex-and-related                   'barista-face-faded)
    (set-face 'org-level-1                            'barista-face-strong)
    (set-face 'org-level-2                            'barista-face-strong)
    (set-face 'org-level-3                            'barista-face-strong)
    (set-face 'org-level-4                            'barista-face-strong)
    (set-face 'org-level-5                            'barista-face-strong)
    (set-face 'org-level-6                            'barista-face-strong)
    (set-face 'org-level-7                            'barista-face-strong)
    (set-face 'org-level-8                            'barista-face-strong)
    (set-face 'org-link                              'barista-face-salient)
    (set-face 'org-list-dt                             'barista-face-faded)
    (set-face 'org-macro                               'barista-face-faded)
    (set-face 'org-meta-line                           'barista-face-faded)
    (set-face 'org-mode-line-clock                     'barista-face-faded)
    (set-face 'org-mode-line-clock-overrun             'barista-face-faded)
    (set-face 'org-priority                            'barista-face-faded)
    (set-face 'org-property-value                      'barista-face-faded)
    (set-face 'org-quote                               'barista-face-faded)
    (set-face 'org-scheduled                           'barista-face-faded)
    (set-face 'org-scheduled-previously                'barista-face-faded)
    (set-face 'org-scheduled-today                     'barista-face-faded)
    (set-face 'org-sexp-date                           'barista-face-faded)
    (set-face 'org-special-keyword                     'barista-face-faded)
    (set-face 'org-table                               'barista-face-faded)
    (set-face 'org-tag                                'barista-face-popout)
    (set-face 'org-tag-group                           'barista-face-faded)
    (set-face 'org-target                              'barista-face-faded)
    (set-face 'org-time-grid                           'barista-face-faded)
    (set-face 'org-todo                              'barista-face-salient)
    (set-face 'org-upcoming-deadline                 'barista-face-default)
    (set-face 'org-verbatim                           'barista-face-popout)
    (set-face 'org-verse                               'barista-face-faded)
    (set-face 'org-warning                            'barista-face-popout)))

(defun barista-theme--dired ()
  "Derive dired faces from barista faces"
  (with-eval-after-load 'dired
    (set-face-attribute 'dired-header nil
                        :background barista-color-background
                        :foreground barista-color-foreground
                        :weight 'bold)
    (set-face-attribute 'dired-flagged nil
                        :background barista-color-background
                        :foreground barista-color-critical
                        :weight 'bold)
    (set-face-attribute 'dired-warning nil
                        :background barista-color-background
                        :foreground barista-color-critical
                        :weight 'bold)
    (set-face-attribute 'dired-set-id nil
                        :background barista-color-background
                        :foreground barista-color-critical
                        :weight 'bold)))


(defun barista-theme--rst ()
  "Derive rst faces from barista faces."
  (with-eval-after-load 'rst
    (set-face 'rst-adornment                           'barista-face-faded)
    (set-face 'rst-block                             'barista-face-default)
    (set-face 'rst-comment                             'barista-face-faded)
    (set-face 'rst-definition                        'barista-face-salient)
    (set-face 'rst-directive                         'barista-face-salient)
    (set-face 'rst-emphasis1                           'barista-face-faded)
    (set-face 'rst-emphasis2                          'barista-face-strong)
    (set-face 'rst-external                          'barista-face-salient)
    (set-face 'rst-level-1                            'barista-face-strong)
    (set-face 'rst-level-2                            'barista-face-strong)
    (set-face 'rst-level-3                            'barista-face-strong)
    (set-face 'rst-level-4                            'barista-face-strong)
    (set-face 'rst-level-5                            'barista-face-strong)
    (set-face 'rst-level-6                            'barista-face-strong)
    (set-face 'rst-literal                           'barista-face-salient)
    (set-face 'rst-reference                         'barista-face-salient)
    (set-face 'rst-transition                        'barista-face-default)))


(defun barista-theme--markdown ()
  "Derive markdown faces from barista faces."
  (with-eval-after-load 'markdown-mode
    (set-face 'markdown-blockquote-face              'barista-face-default)
    (set-face 'markdown-bold-face                     'barista-face-strong)
    (set-face 'markdown-code-face                    'barista-face-default)
    (set-face 'markdown-comment-face                   'barista-face-faded)
    (set-face 'markdown-footnote-marker-face         'barista-face-default)
    (set-face 'markdown-footnote-text-face           'barista-face-default)
    (set-face 'markdown-gfm-checkbox-face            'barista-face-default)
    (set-face 'markdown-header-delimiter-face          'barista-face-faded)
    (set-face 'markdown-header-face                   'barista-face-strong)
    (set-face 'markdown-header-face-1                 'barista-face-strong)
    (set-face 'markdown-header-face-2                 'barista-face-strong)
    (set-face 'markdown-header-face-3                 'barista-face-strong)
    (set-face 'markdown-header-face-4                 'barista-face-strong)
    (set-face 'markdown-header-face-5                 'barista-face-strong)
    (set-face 'markdown-header-face-6                'barista-face-strong)
    (set-face 'markdown-header-rule-face             'barista-face-default)
    (set-face 'markdown-highlight-face               'barista-face-default)
    (set-face 'markdown-hr-face                      'barista-face-default)
    (set-face 'markdown-html-attr-name-face          'barista-face-default)
    (set-face 'markdown-html-attr-value-face         'barista-face-default)
    (set-face 'markdown-html-entity-face             'barista-face-default)
    (set-face 'markdown-html-tag-delimiter-face      'barista-face-default)
    (set-face 'markdown-html-tag-name-face           'barista-face-default)
    (set-face 'markdown-inline-code-face              'barista-face-popout)
    (set-face 'markdown-italic-face                    'barista-face-faded)
    (set-face 'markdown-language-info-face           'barista-face-default)
    (set-face 'markdown-language-keyword-face        'barista-face-default)
    (set-face 'markdown-line-break-face              'barista-face-default)
    (set-face 'markdown-link-face                    'barista-face-salient)
    (set-face 'markdown-link-title-face              'barista-face-default)
    (set-face 'markdown-list-face                      'barista-face-faded)
    (set-face 'markdown-markup-face                    'barista-face-faded)
    (set-face 'markdown-math-face                    'barista-face-default)
    (set-face 'markdown-metadata-key-face              'barista-face-faded)
    (set-face 'markdown-metadata-value-face            'barista-face-faded)
    (set-face 'markdown-missing-link-face            'barista-face-default)
    (set-face 'markdown-plain-url-face               'barista-face-default)
    (set-face 'markdown-pre-face                     'barista-face-default)
    (set-face 'markdown-reference-face               'barista-face-salient)
    (set-face 'markdown-strike-through-face            'barista-face-faded)
    (set-face 'markdown-table-face                   'barista-face-default)
    (set-face 'markdown-url-face                     'barista-face-salient)))


(defun barista-theme--ivy ()
  "Derive ivy faces from barista faces."
  (with-eval-after-load 'ivy
    (set-face 'ivy-action                              'barista-face-faded)
    (set-face 'ivy-completions-annotations             'barista-face-faded)
    (set-face 'ivy-confirm-face                        'barista-face-faded)
    (set-face 'ivy-current-match    '(barista-face-strong barista-face-subtle))
    (set-face 'ivy-cursor                             'barista-face-strong)
    (set-face 'ivy-grep-info                          'barista-face-strong)
    (set-face 'ivy-grep-line-number                    'barista-face-faded)
    (set-face 'ivy-highlight-face                     'barista-face-strong)
    (set-face 'ivy-match-required-face                 'barista-face-faded)
    (set-face 'ivy-minibuffer-match-face-1             'barista-face-faded)
    (set-face 'ivy-minibuffer-match-face-2             'barista-face-faded)
    (set-face 'ivy-minibuffer-match-face-3             'barista-face-faded)
    (set-face 'ivy-minibuffer-match-face-4             'barista-face-faded)
    (set-face 'ivy-minibuffer-match-highlight         'barista-face-strong)
    (set-face 'ivy-modified-buffer                    'barista-face-popout)
    (set-face 'ivy-modified-outside-buffer            'barista-face-strong)
    (set-face 'ivy-org                                 'barista-face-faded)
    (set-face 'ivy-prompt-match                        'barista-face-faded)
    (set-face 'ivy-remote                            'barista-face-default)
    (set-face 'ivy-separator                           'barista-face-faded)
    (set-face 'ivy-subdir                              'barista-face-faded)
    (set-face 'ivy-virtual                             'barista-face-faded)
    (set-face 'ivy-yanked-word                         'barista-face-faded)))

(defun barista-theme--helm ()
  "Derive helm faces from barista faces."
  (with-eval-after-load 'helm
    (set-face 'helm-selection                '(barista-face-strong barista-face-subtle))
    (set-face 'helm-match                    'barista-face-strong)
    (set-face 'helm-source-header            'barista-face-salient)
    (set-face 'helm-visible-mark             'barista-face-strong)))

(defun barista-theme--helm-swoop ()
  "Derive helm faces from barista faces."
  (with-eval-after-load 'helm-swoop
    (set-face 'helm-swoop-target-line-face   '(barista-face-strong barista-face-subtle))))

(defun barista-theme--helm-occur ()
  "Derive helm faces from barista faces."
  (with-eval-after-load 'helm-occur
    (set-face 'helm-moccur-buffer             'barista-face-strong)))

(defun barista-theme--helm-ff ()
  "Derive helm faces from barista faces."
  (with-eval-after-load 'helm-ff
    (set-face 'helm-ff-file                 'barista-face-faded)
    (set-face 'helm-ff-prefix               'barista-face-strong)
    (set-face 'helm-ff-dotted-directory     'barista-face-faded)
    (set-face 'helm-ff-directory            'barista-face-strong)
    (set-face 'helm-ff-executable           'barista-face-popout)))

(defun barista-theme--helm-grep ()
  "Derive helm faces from barista faces."
  (with-eval-after-load 'helm-grep
    (set-face 'helm-grep-match                        'barista-face-strong)
    (set-face 'helm-grep-file                         'barista-face-faded)
    (set-face 'helm-grep-lineno                       'barista-face-faded)
    (set-face 'helm-grep-finish                       'barista-face-default)))

(defun barista-theme ()
  "Derive many, many faces from the core barista faces."
  (barista-theme--agenda)
  (barista-theme--basics)
  (barista-theme--bookmark)
  (barista-theme--buttons)
  (barista-theme--calendar)
  (barista-theme--company)
  (barista-theme--ctrlf)
  (barista-theme--customize)
  (barista-theme--diff)
  (barista-theme--flycheck)
  (barista-theme--flyspell)
  (barista-theme--font-lock)
  (barista-theme--helm)
  (barista-theme--helm-ff)
  (barista-theme--helm-grep)
  (barista-theme--helm-occur)
  (barista-theme--helm-swoop)
  (barista-theme--rainbow-delimiters)
  (barista-theme--hl-line)
  (barista-theme--ido)
  (barista-theme--info)
  (barista-theme--ivy)
  (barista-theme--markdown)
  (barista-theme--message)
  (barista-theme--minibuffer)
  (barista-theme--parinfer-rust)
  (barista-theme--dired)
  (barista-theme--mode-line)
  (barista-theme--org)
  (barista-theme--outline)
  (barista-theme--package)
  (barista-theme--cider-overlay)
  (barista-theme--hydra)
  (barista-theme--numbers-mode)
  (barista-theme--rst)
  (barista-theme--speedbar)
  (barista-theme--term))
 
(provide 'barista-theme)

;;; barista-theme.el ends here
