(setq gbj-backup-dir "~/backups/")

;;setup prefix key
(setq gbj-prefix-key ?\C-l)
(setq old-prefix-key
      (lookup-key (current-global-map) (vector gbj-prefix-key)))

(global-unset-key (vector gbj-prefix-key))
(global-set-key (vector gbj-prefix-key gbj-prefix-key)  old-prefix-key)

(defun gbj-mark-line (arg)
  "Put lines into the kill-ring.  Prefix arg works as in forward-line"
  (interactive "p")
  (let ((beg (point)))
    (save-excursion
      (forward-line arg)
      (kill-ring-save beg (point)))))

(defun gbj-mark-word ()
  "put the filename at point into the kill-ring"
  (interactive )
  (kill-new (thing-at-point 'filename)))

(global-set-key (vector gbj-prefix-key ?m)  'gbj-mark-word)
(global-set-key (vector gbj-prefix-key  '(control m))  'gbj-mark-line)

(setq comint-input-ring-size 200)

(eval-after-load "shell"
  '(progn
     (define-key shell-mode-map
       [(meta p)] 'comint-previous-matching-input-from-input)
     (define-key shell-mode-map
       [(super p)] 'comint-previous-matching-input-from-input)
     (define-key shell-mode-map
       [(meta n)] 'comint-next-matching-input-from-input)))

(eval-after-load "comint"
  '(progn
     (define-key comint-mode-map
       [(meta p)] 'comint-previous-matching-input-from-input)
     (define-key comint-mode-map
       [(meta n)] 'comint-next-matching-input-from-input)))

(defmacro gbj-make-buffer-key (key-string)
  "assigns a function to key-string that switches to a buffer previously remembered.  With a prefix arg, remembers the name of the current buffer"
  `(global-set-key ,key-string
                   '(lambda (&optional arg ) (interactive "P")
                      (if  arg
                          (set (intern (concat "gbj-make-buffer-key" ,key-string))
                               (buffer-name (current-buffer)))
                        (progn
                                        ;Make sure its been set
                                        ; before using it
                          (if (intern-soft (concat "gbj-make-buffer-key"
                                                   ,key-string))

                              (switch-to-buffer
                               (symbol-value (intern (concat "gbj-make-buffer-key"
                                                             ,key-string))))
                            (message
                             (format "No buffer has been set to key %s.  Set it with ^u%s"
                                     ,key-string ,key-string))))))))

(defmacro gbj-make-shell-key (key-string name)
  "assigns a function to key-string that either creates or switches to a shell buffer"
  (interactive)
  `(global-set-key ,key-string
                   '(lambda () (interactive)
                      (if (get-buffer ,name)
                          (switch-to-buffer ,name)
                        (progn (shell)
                               (abbrev-mode)
                               (rename-buffer ,name))))))


(gbj-make-buffer-key (string gbj-prefix-key ?a))
(gbj-make-buffer-key (string gbj-prefix-key ?b))
(gbj-make-buffer-key (string gbj-prefix-key ?c))

;; make a bunch of shell keys
(gbj-make-shell-key (string gbj-prefix-key  ?\C-a) "shell-a")
(gbj-make-shell-key (string gbj-prefix-key  ?\C-b) "shell-b")
(gbj-make-shell-key (string gbj-prefix-key  ?\C-c) "shell-c")
(gbj-make-shell-key (string gbj-prefix-key  ?\C-d) "shell-d")
(gbj-make-shell-key (string gbj-prefix-key  ?\C-e) "shell-e")
(gbj-make-shell-key (string gbj-prefix-key  ?\C-f) "shell-f")

(global-set-key (vector gbj-prefix-key  ?\C-o) `occur)

(defun gbj-backup (&optional arg)
  "makes a copy of the current file with timestamp appended to the filename"
  (interactive)
  (let*
      ((orig-file (if arg arg buffer-file-name))
       (mod-str nil)
       (new-file nil))
    (if (not orig-file)
        (setq orig-file (concat "~/tmp/" (buffer-name))))
    (with-temp-buffer
                                        ; get the time stamp
      (insert (current-time-string
               (nth 5 (file-attributes orig-file))))

                                        ;fix up the bad chars
      (goto-char (point-min))
      (while (re-search-forward "[ :]" nil t)
        (replace-match "-" nil nil))
      (goto-char (point-min))
      (while (re-search-forward "--" nil t)
        (replace-match "-" nil nil))
      (setq mod-str (buffer-string))
      (setq new-file (concat
                      gbj-backup-dir (file-name-nondirectory orig-file) "." (substring mod-str 4 )))
                                        ; copy out the file
      (copy-file  orig-file new-file  )


      (shell-command (format "chmod 700 %s" new-file))
      (message (format "Wrote out %s" new-file)))))

(defun gbj-backup-shell (&optional arg)
  "makes a copy of the current shell with timestamp appended to the filename"
  (interactive)
  (let*
      (
       (save-dir gbj-backup-dir)
       (orig-file arg)
       (mod-str nil)
       )
    (if (not orig-file)
        (setq orig-file (concat save-dir (buffer-name))))
    (with-temp-buffer
                                        ; get the time stamp
      (insert (current-time-string))

                                        ;fix up the bad chars
      (goto-char (point-min))
      (while (re-search-forward "[ :]" nil t)
        (replace-match "-" nil nil))
      (goto-char (point-min))
      (while (re-search-forward "--" nil t)
        (replace-match "-" nil nil))
      (setq mod-str (buffer-string)))
                                        ; copy out the file
    (write-file  (concat orig-file "." (substring mod-str 4 )))
    (rename-buffer (file-name-nondirectory orig-file))
    (setq buffer-file-name nil)
    (auto-save-mode -1)
    (message (format "Wrote out %s" (concat orig-file "." (substring mod-str 4 ))))))

(defun gbj-backup-both ()
  (interactive)
  (if buffer-file-name
      (gbj-backup)
    (gbj-backup-shell)))

(global-set-key (vector gbj-prefix-key ?\C-s) `gbj-backup-both)

(global-set-key (vector gbj-prefix-key ?\C-w)
                '(lambda  (&optional arg ) (interactive "P")
                   (if arg
                       (window-configuration-to-register 97 nil)
                     (jump-to-register 97 nil))))


(global-set-key (vector gbj-prefix-key ?\C-r) `shell-resync-dirs)

(add-hook 'clojure-mode-hook
          (lambda ()
            (paredit-mode)
            (setq
             indent-tabs-mode nil
             whitespace-style '(trailing lines space-before-tab
                                         tabs space-after-tab)
             whitespace-line-column 80
             line-number-mode t
             column-number-mode t)
            (whitespace-mode t)))

;; quickly cleanup code buffers
(defun cleanup ()
  (interactive)
  (untabify (point-min) (point-max))
  (indent-region (point-min) (point-max))
  (delete-trailing-whitespace))

(global-set-key (kbd "C-c n") 'cleanup)

