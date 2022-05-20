;;; mnml command interface -*- lexical-binding: t; -*-

(define-minor-mode barista-command-mode
  "barista command mode"
  :keymap  (make-sparse-keymap))

(defface barista-face-command nil
  "Face for the whole header line command"
  :group 'barista)

(defface barista-face-command-prompt nil
  "Face for header line command prompt"
  :group 'barista)

(defface barista-face-command-cursor nil
  "Face for header line command cursor"
  :group 'barista)

(set-face-attribute 'barista-face-command nil
                    :foreground barista-color-foreground
                    :background barista-color-subtle
                    :box `(:line-width 1
                           :color ,barista-color-foreground
                           :style nil)
                   :inherit nil)

(set-face-attribute 'barista-face-command-prompt nil
                    :inherit 'barista-face-strong
                    :foreground barista-color-background
                    :background barista-color-foreground
                    :box `(:line-width 1
                           :color ,barista-color-foreground
                           :style nil))

(set-face-attribute 'barista-face-command-cursor nil
                    :foreground barista-color-background
                    :background barista-color-foreground)

(defvar barista-command--slave nil
  "Slave buffer displaying the command.")

(defvar barista-command--master "*barista-command*"
  "Master buffer recording keystrokes.")

(defvar barista-command--cookie nil
  "Cookie returned by face-remap-add-relative.")

(defun barista-command--update ()
  "This function makes sure the content of the master buffer is copied
to the slave buffer header line and cursor stays on first line."

  ;; Makes sure cursor stays on first line
  (with-current-buffer barista-command--master
   (let ((eol (save-excursion (goto-char (point-min)) (point-at-eol))))
    (if (> (point) eol) (goto-char eol))))

  ;; Update slave header line
  (with-current-buffer barista-command--slave
    (force-mode-line-update nil)))


(defun barista-command--check-focus (&rest args)
  "This function check if the maste buffer has focus.
If not, it closes barista command."

  (if (not (eq (selected-window)
               (get-buffer-window barista-command--master)))
      (barista-command--close)))

(defun barista-command--close ()
  "Close barista command"

  (interactive)

  ;; Remove advice
  (advice-remove #'select-window #'barista-command--check-focus)

  ;; Close master window
  (when (window-live-p (get-buffer-window barista-command--master))
    (delete-window (get-buffer-window barista-command--master)))

  ;; Kill master buffer
  (when (get-buffer barista-command--master)
    (kill-buffer barista-command--master))

  ;; Restore slave to normal state
  (with-selected-window (get-buffer-window barista-command--slave)
    (kill-local-variable 'header-line-format)
    (face-remap-remove-relative barista-command--cookie))

  ;; Update mode lines
  (force-mode-line-update t))


(defun barista-command (&optional prompt callback content information)

  ;; Cannot open barista command while in minibuffer
  (when (minibufferp)
    (error "Cannot open barista command while in minibuffer"))

  ;; Cannot open barista command while in barista command
  (when (eq (current-buffer) (get-buffer barista-command--master))
    (error "Cannot open barista command while in mini command"))

  ;; Kill the master buffer & window if openened (not strictly necessary)
  (when (window-live-p (get-buffer-window barista-command--master))
    (delete-window (get-buffer-window barista-command--master))
    (kill-buffer barista-command--master))

  ;; Save the slave buffer
  (setq barista-command--slave (current-buffer))

  ;; Install barista face command in the slave buffer
  (setq barista-command--cookie
        (face-remap-add-relative 'header-line 'barista-face-command))

  ;; Create master buffer by splitting slave buffer
  (let ((window-min-height 1)
        (window-safe-min-height 1)
        (window-resize-pixelwise t)
        (split-window-keep-point t))
    (with-selected-window (split-window-vertically -2)
      (switch-to-buffer (get-buffer-create barista-command--master))
      (erase-buffer)
      (org-mode)
      (barista-command-mode)
      (if content (insert content))
      (insert "\n")
      (insert "-")

      ;; This tries to hide most of the master window
      (goto-char (point-min))
      (overlay-put (make-overlay (point-at-bol) (+ (point-at-eol) 1))
                   'face '(:height 10))
     (setq cursor-type nil)

      (setq header-line-format nil)
      (setq mode-line-format nil)
      (face-remap-add-relative 'default `(:foreground ,barista-color-background))
      (face-remap-add-relative 'region  `(:background ,barista-color-background))
      (fit-window-to-buffer)
      (setq window-size-fixed t)

      ;; History
      ;; (goto-char (point-max))
      ;; (insert "history-item-1 history-item-2 history-item-3")
      ;; (goto-char (point-min))
      ))


  ;; Install header line in master buffer
  (setq header-line-format
        (list

         ;; Prompt + one space
         (propertize " "  'face 'barista-face-command-prompt
		          'display `(raise -0.20))
         (propertize (or prompt "M-x") 'face 'barista-face-command-prompt)
         (propertize " "  'face 'barista-face-command-prompt
	  	          'display `(raise +0.15))
         (propertize " " )

         ;; Input (copied from master). we need to add a space at end
         ;; of content to be able to show cursor when it is at the end
         ;; of the line.
         `(:eval
           (let* ((content (with-current-buffer barista-command--master
                             (save-excursion (goto-char (point-min))
                               (buffer-substring (point-at-bol) (point-at-eol)))))
                  (content (cond ((> (length content) 0)
                                  (concat content " "))
                                 ((> (length ,information) 0)
                                  (propertize ,information 'face 'barista-face-faded))
                                 (t " ")))
                  (point  (with-current-buffer barista-command--master (point)))
                  (region (with-current-buffer barista-command--master (region-active-p))))

             ;; Cursor
             (put-text-property (- point 1) point
                                'face 'barista-face-command-cursor content)
             ;; Region
             (if region
                 (let ((beg (with-current-buffer barista-command--master (region-beginning)))
                       (end (with-current-buffer barista-command--master (region-end))))
                   (put-text-property (- beg 1) (- end 1)
                                      'face `(:foreground ,barista-color-background
                                              :background ,barista-color-faded) content)))
             content))))

  ;; Install key bindings
  (with-current-buffer barista-command--master
    (add-hook 'post-command-hook 'barista-command--update nil t)
    (define-key barista-command-mode-map (kbd "C-g") #'barista-command--close)
    (define-key barista-command-mode-map (kbd "<tab>")
      #'(lambda() (interactive) (dabbrev-expand nil)))

    (define-key barista-command-mode-map (kbd "<return>")
      (lambda ()
        (interactive)
        (let* ((content (with-current-buffer barista-command--master
                          (save-excursion (goto-char (point-min))
                          (buffer-substring (point-at-bol) (point-at-eol))))))
          (barista-command--close)
          (if callback (funcall callback content)
            (message content))))))

  ;; Update mode lines and swicch to master buffer
  (barista-command--update)
  (select-window (get-buffer-window barista-command--master) t)
  (redisplay)

  ;; Advice after select window to check for focus
  (advice-add #'select-window :after #'barista-command--check-focus))


(defun barista-command-x ()
  (interactive)
  (barista-command "M-x"  #'barista-command-x-finalize "" "Enter command"))

(defun barista-command-x-finalize (command)
  (interactive)
  (command-execute (intern command)))


(defun barista-command-shell ()
  (interactive)
  (barista-command ">_"  #'barista-command-shell-finalize
                "" "Enter shell command"))

(defun barista-command-shell-finalize (command)
  (interactive)
  (shell-command command)
  (switch-to-buffer "*Shell Command Output*"))

(define-key global-map (kbd "M-x") #'barista-command-x)
(define-key global-map (kbd "M-s") #'barista-command-shell)

(provide 'barista-command)
