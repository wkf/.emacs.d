;;; user-mode-line  --- a sassy mode line

;;; Commentary:

;;; Code:

(require 'f)
(require 'vc-git)
(require 'projectile)
(require 'spinner)
(require 'powerline)
(require 'eshell)
(require 'flycheck)
;; (require 'lispyville)

(defgroup user-mode-line nil
  ""
  :group 'user)

(defcustom user-mode-line/emacs-state-color "yellow"
  "Color to use for Emacs state."
  :type 'string
  :group 'user-mode-line)

(defcustom user-mode-line/insert-state-color "green"
  "Color to use for insert state."
  :type 'string
  :group 'user-mode-line)

(defcustom user-mode-line/motion-state-color "magenta"
  "Color to use for motion state."
  :type 'string
  :group 'user-mode-line)

(defcustom user-mode-line/normal-state-color "blue"
  "Color to use for normal state."
  :type 'string
  :group 'user-mode-line)

(defcustom user-mode-line/replace-state-color "red"
  "Color to use for replace state."
  :type 'string
  :group 'user-mode-line)

(defcustom user-mode-line/visual-state-color "orange"
  "Color to use for visual state."
  :type 'string
  :group 'user-mode-line)

(defcustom user-mode-line/special-state-color "cyan"
  "Color to use for special state."
  :type 'string
  :group 'user-mode-line)

(defcustom user-mode-line/unknown-state-color "pink"
  "Color to use for unknown state."
  :type 'string
  :group 'user-mode-line)

(defface user/mode-line-info
  `((t (:inherit mode-line)))
  "")

(defface user/mode-line-default
  `((t (:inherit mode-line :foreground ,(face-foreground 'default))))
  "")

(defface user/mode-line-changed
  `((t (:inherit mode-line :foreground ,(face-foreground 'diff-changed))))
  "")

(defface user/mode-line-locked
  `((t (:inherit mode-line :foreground ,(face-foreground 'error))))
  "")

(defface user/mode-line-added
  `((t (:inherit mode-line :foreground ,(face-foreground 'diff-added))))
  "")

(defface user/mode-line-deleted
  `((t (:inherit mode-line :foreground ,(face-foreground 'diff-removed))))
  "")

(defface user/mode-line-warning
  `((t (:inherit mode-line :foreground ,(face-foreground 'warning))))
  "")

(defface user/mode-line-alert
  `((t (:inherit mode-line :foreground ,(face-foreground 'warning))))
  "")

(defface user/mode-line-error
  `((t (:inherit mode-line :foreground ,(face-foreground 'error))))
  "")

(defface user/mode-line-success
  `((t (:inherit mode-line :foreground ,(face-foreground 'success))))
  "")

(defface user/mode-line-inactive
  `((t (:inherit mode-line)))
  "")

(defface user/mode-line-buffer-project
  `((t (:inherit mode-line :foreground ,(face-foreground 'default))))
  "")

(defface user/mode-line-buffer-path
  `((t (:inherit mode-line)))
  "")

(defface user/mode-line-buffer-name
  `((t (:inherit mode-line :foreground ,(face-foreground 'default))))
  "")

(defface user/mode-line-hud-disabled
  `((t (:inherit mode-line)))
  "")

(defface user/mode-line-emacs-state
  `((t (:foreground ,(face-background 'default) :background ,user-mode-line/emacs-state-color)))
  "")

(defface user/mode-line-insert-state
  `((t (:foreground ,(face-background 'default) :background ,user-mode-line/insert-state-color)))
  "")

(defface user/mode-line-motion-state
  `((t (:foreground ,(face-background 'default) :background ,user-mode-line/motion-state-color)))
  "")

(defface user/mode-line-normal-state
  `((t (:foreground ,(face-background 'default) :background ,user-mode-line/normal-state-color)))
  "")

(defface user/mode-line-replace-state
  `((t (:foreground ,(face-background 'default) :background ,user-mode-line/replace-state-color)))
  "")

(defface user/mode-line-visual-state
  `((t (:foreground ,(face-background 'default) :background ,user-mode-line/visual-state-color)))
  "")

(defface user/mode-line-special-state
  `((t (:foreground ,(face-background 'default) :background ,user-mode-line/special-state-color)))
  "")

(defface user/mode-line-unknown-state
  `((t (:foreground ,(face-background 'default) :background ,user-mode-line/unknown-state-color)))
  "")

(defvar user/evil-state-faces
  `((emacs . user/mode-line-emacs-state)
    (insert . user/mode-line-insert-state)
    (motion . user/mode-line-motion-state)
    (normal . user/mode-line-normal-state)
    (replace . user/mode-line-replace-state)
    (visual . user/mode-line-visual-state)))

(defvar user/-selected-window (frame-selected-window))

(defun user/set-selected-window (&rest args)
  "Set selected window and ignore ARGS."
  (when (not (minibuffer-window-active-p (frame-selected-window)))
    (setq user/-selected-window (frame-selected-window))))

(add-hook 'focus-in-hook 'user/set-selected-window)
(add-hook 'window-configuration-change-hook 'user/set-selected-window)
(advice-add 'select-window :after #'user/set-selected-window)
(advice-add 'handle-switch-frame :after #'user/set-selected-window)

(defsubst user/selected-window-p ()
  "Predicate returning t if current window is the selected window."
  (eq (selected-window) user/-selected-window))

(defun user/make-xpm (color height width)
  "Create an XPM bitmap of COLOR of size HEIGHT x WIDTH."
  (when window-system
    (propertize
     " " 'display
     (let ((data nil)
	   (i 0))
       (setq data (make-list height (make-list width 1)))
       (pl/make-xpm "percent" color color (reverse data))))))

(defun user/get-evil-state ()
  "Get current evil state."
  (if (eq 'operator evil-state) evil-previous-state evil-state))

(defun user/get-face-height (face)
  "Get height in pixels of FACE."
  (aref (font-info (face-font face)) 2))

(defun user/get-face-width (face)
  "Get width in pixels of FACE."
  (aref (aref (font-get-glyphs (face-attribute face :font) 65 66) 0) 4))

(defun user/get-evil-state-highlight-face ()
  "Set the highlight face depending on the evil state."
  (if (bound-and-true-p evil-local-mode)
      (if (and (featurep 'lispyvile)
               (lispyville--lispy-keybindings-active-p))
          'user/mode-line-special-state
        (let* ((state (user/get-evil-state))
               (face (assq state user/evil-state-faces)))
          (or (cdr face) 'user/mode-line-unknown-state)))
    'user/mode-line-unknown-state))

(defun user/get-project-name ()
  "Get current projectile project name."
  (when (projectile-project-p)
    (propertize (projectile-project-name)
		            'face (user/active-face 'user/mode-line-info))))

(defun user/project-root (&optional strict-p)
  "Get the path to the root of your project.  STRICT-P determines if project root is required."
  (let (projectile-require-project-root strict-p)
    (projectile-project-root)))

(defun user/shorten-buffer-path (buffer-path max-length)
  "Shorten BUFFER-PATH to at most MAX-LENGTH."
  (when (and buffer-path (not (equal buffer-path ".")))
    (if (> (length buffer-path) max-length)
        (let ((path (reverse (split-string buffer-path "/" t)))
              (output ""))
          (when (and path (equal "" (car path)))
            (setq path (cdr path)))
          (while (and path (<= (length output) (- max-length 4)))
            (setq output (concat (car path) "/" output))
            (setq path (cdr path)))
          (when path
            (setq output (concat "../" output)))
          (when (string-suffix-p "/" output)
            (setq output (substring output 0 -1)))
          output)
      buffer-path)))

(defun user/buffer-path-segment ()
  "Displays the buffer's full path relative to the project root, excluding the file basename."
  (let ((max-length (truncate (* (window-body-width) 0.4))))
    (cond (buffer-file-name
           (let* ((default-directory (f-dirname buffer-file-name))
                  (buffer-path (f-relative default-directory (user/project-root)))
                  (short-buffer-path (user/shorten-buffer-path buffer-path max-length)))
             (and short-buffer-path (concat short-buffer-path "/"))))
          ((eq major-mode 'eshell-mode)
           (let* ((default-directory (eshell/pwd))
                  (buffer-path (if (projectile-project-p)
                                   (f-relative default-directory (user/project-root))
                                 (abbreviate-file-name default-directory)))
                  (short-buffer-path (user/shorten-buffer-path buffer-path max-length)))
             (if short-buffer-path
                 (concat short-buffer-path "/")
               "")))
          ((eq major-mode 'vterm-mode)
           (let* ((default-directory (or (vterm--get-pwd 1) "."))
                  (buffer-path (if (projectile-project-p)
                                   (f-relative default-directory (user/project-root))
                                 (abbreviate-file-name default-directory)))
                  (short-buffer-path (user/shorten-buffer-path buffer-path max-length)))
             (if (equal short-buffer-path "./")
                 ""
               short-buffer-path))
           )
          ((eq major-mode 'term-mode)
           (let* ((buffer-path (if (projectile-project-p)
                                   (f-relative default-directory (user/project-root))
                                 (abbreviate-file-name default-directory)))
                  (short-buffer-path (user/shorten-buffer-path buffer-path max-length)))
             (if (equal short-buffer-path "./")
                 ""
               short-buffer-path))))))

;; user/-mode-line-spinner-reasons
;; user/-mode-line-spinner-timers
;; (user/start-mode-line-spinner 'something 3)
;; (user/stop-mode-line-spinner 'something)
;; (user/mode-line-spinner-spinning-p)

(defvar-local user/-mode-line-spinner nil)
(defvar-local user/-mode-line-spinner-timers '())
(defvar-local user/-mode-line-spinner-reasons '())
(defvar-local user/-mode-line-alert-level nil)
(defvar-local user/-mode-line-alert-levels '())

(defun user/show-mode-line-alert (reason level)
  "Show mode line alert for REASON with LEVEL."
  (setf (alist-get reason user/-mode-line-alert-levels) level)
  (when (not (eq user/-mode-line-alert-level 'error))
    (setq user/-mode-line-alert-level level)))

(defun user/hide-mode-line-alert (reason)
  "Hide mode line alert for REASON."
  (setf (alist-get reason user/-mode-line-alert-levels) nil)
  (setq user/-mode-line-alert-level nil)
  (dolist (e user/-mode-line-alert-levels)
    (when (cdr e)
      (when (not (eq user/-mode-line-alert-level 'error))
        (setq user/-mode-line-alert-level (cdr e))))))

(defun user/stop-mode-line-spinner (reason)
  "Stop mode line spinner for REASON."
  (setq user/-mode-line-spinner-reasons
        (remq reason user/-mode-line-spinner-reasons))
  (when-let ((timer (alist-get reason user/-mode-line-spinner-timers)))
    (cancel-timer timer)
    (setf (alist-get reason user/-mode-line-spinner-timers) nil)))

(defun user/start-mode-line-spinner (reason &optional duration)
  "Start mode line spinner for REASON for optional DURATION."
  (add-to-list 'user/-mode-line-spinner-reasons reason)
  (when duration
    (setf (alist-get reason user/-mode-line-spinner-timers)
          (run-at-time duration nil
                       (lambda (reason)
                         (user/stop-mode-line-spinner reason))
                       reason))))

(defun user/mode-line-spinner-spinning-p ()
  "Return t if mode line spinner should be spinning."
  user/-mode-line-spinner-reasons)

(defun user/evil-state-segment ()
  "Return a mode-line segment for the current evil state."
  (unless user/-mode-line-spinner
    (setq user/-mode-line-spinner (make-spinner 'rotating-line t)))
  (propertize
   (if (user/mode-line-spinner-spinning-p)
       (concat
        "  "
        (spinner-start-print user/-mode-line-spinner)
        "  ")
     (progn
       (spinner-stop user/-mode-line-spinner)
       (if user/-mode-line-alert-level
           (concat "  "
                   (pcase user/-mode-line-alert-level
                     ('warn "?")
                     ('error "!")
                     (_ "*"))
                   "  ")
         "  λ  ")))
   'face (user/active-face
          (user/get-evil-state-highlight-face))))

(defun user/git-p ()
  "Can we find git."
  (or (and buffer-file-name (vc-find-root buffer-file-name ".git"))
      (and (eq major-mode 'eshell-mode) (vc-find-root (eshell/pwd) ".git"))
      (and (eq major-mode 'term-mode)) (vc-find-root default-directory ".git")))

(defun user/format-branch-stats (stats)
  "Format branch STATS."
  (if (string= "" stats)
      "+0-0"
    (let ((insertions (and
                       (string-match "\\([0-9]+\\) insertions(\\+)" stats)
                       (match-string 1 stats)))
          (deletions (and
                      (string-match "\\([0-9]+\\) deletions(-)" stats)
                      (match-string 1 stats))))
      (concat
       (if insertions
           (propertize (concat "+" insertions)
                       'face (user/active-face 'user/mode-line-info))
         "+0")
       (if deletions
           (propertize (concat "-" deletions)
                       'face (user/active-face 'user/mode-line-info))
         "-0")))))

(defun user/get-branch-stats ()
  "Return current branch status."
  (let* ((tree-stats (vc-git--run-command-string nil "diff" "--shortstat" "--"))
         (tree-string (user/format-branch-stats tree-stats))
         (staged-stats (vc-git--run-command-string nil "diff" "--staged" "--shortstat" "--"))
         (staged-string (user/format-branch-stats staged-stats)))
    (format "(%s/%s)" tree-string staged-string)))

(defun user/git-branch-changed-p ()
  "Return t if git branch has changed."
  (eq 1 (call-process "git" nil nil nil "diff-index" "--quiet" "HEAD" "--")))

(defun user/get-branch-name-and-stats ()
  "Return current git branch."
  (when (user/git-p)
    (let ((branch (car (vc-git-branches))))
      (unless (null branch)
        (let* ((stats (user/get-branch-stats))
               (changed-p (not (string= stats "(+0-0/+0-0)"))))
          (cond
           ((string-match "^(HEAD detached at \\([[:word:]]+\\))$" branch)
            (format "(%s) (%s) %s"
                (propertize (match-string 1 branch)
                            'face (user/active-face (if changed-p
                                                        'user/mode-line-changed
                                                      'user/mode-line-info)))
                (propertize "?"
                            'face (user/active-face 'user/mode-line-error))
                stats))
           ((string= branch "master")
            (format "%s (%s) %s"
                    (propertize branch
                                'face (user/active-face (if changed-p
                                                            'user/mode-line-changed
                                                          'user/mode-line-info)))
                    (propertize "!"
                                'face (user/active-face 'user/mode-line-error))
                    stats))
           (t (format "%s %s" branch stats))))))))

(defvar user/-branch-status-by-project '())
(defvar-local user/-branch-status-timer nil)

(defun user/get-branch-name-changed ()
  "Return current git branch."
  (when (user/git-p)
    (let ((branch (car (vc-git-branches))))
      (unless (null branch)
        (unless user/-branch-status-timer
          (setq user/-branch-status-timer
                (run-with-timer
                 0
                 3
                 (lambda ()
                   (let* ((status (user/git-branch-changed-p))
                          (project (projectile-project-name))
                          (cell (assoc project user/-branch-status-by-project)))
                     (if cell
                         (setcdr cell status)
                       (add-to-list 'user/-branch-status-by-project `(,project . ,status))))
                   (force-mode-line-update)))))
        (let ((changed-p (cdr (assoc (projectile-project-name) user/-branch-status-by-project))))
          (cond
           ((string-match "^(HEAD detached at \\([[:word:]]+\\))$" branch)
            (format "(%s) (%s)"
                    (propertize (match-string 1 branch)
                                'face (user/active-face (if changed-p
                                                            'user/mode-line-changed
                                                          'user/mode-line-info)))
                    (propertize "?"
                                'face (user/active-face 'user/mode-line-error))))
           ((string= branch "master")
            (format "%s (%s)"
                    (propertize branch
                                'face (user/active-face (if changed-p
                                                            'user/mode-line-changed
                                                          'user/mode-line-info)))
                    (propertize "!"
                                'face (user/active-face 'user/mode-line-error))))
           (t (format "%s" branch))))))))

(defun user/get-branch-name ()
  "Return the current branch name."
  (when (user/git-p)
    (let ((branch (car (vc-git-branches))))
      (unless (null branch)
        (cond
         ((string-match "^(HEAD detached at \\([[:word:]]+\\))$" branch)
          (format "(%s)"
                  (propertize (match-string 1 branch)
                              'face (user/active-face 'user/mode-line-error))))
         ;; ((string= branch "master")
         ;;  (format "%s"
         ;;          (propertize branch
         ;;                      'face (user/active-face 'user/mode-line-error))))
         (t (format "%s"
                    (propertize branch
                                'face (user/active-face 'user/mode-line-info)))))))))

(defun user/get-major-mode ()
  "Return the major mode, including process info."
  (concat (format-mode-line mode-name)
          (if (stringp mode-line-process) mode-line-process)))

(defvar-local user/-flycheck-errors nil)

(defun user/active-face (face)
  "Return FACE if active window selected, else mode-line-inactive."
  (if (user/selected-window-p) face 'mode-line-inactive))

(defun user/style-flycheck-errors-count (errors)
  "Style flycheck ERRORS."
  (let-alist errors
    (format "%s:%s:%s"
            (propertize (number-to-string (or .info 0))
                        'face (user/active-face
                               (if .info 'user/mode-line-success 'user/mode-line-info)))
            (propertize (number-to-string (or .warning 0))
                        'face (user/active-face
                               (if .warning 'user/mode-line-alert 'user/mode-line-info)))
            (propertize (number-to-string (or .error 0))
                        'face (user/active-face
                               (if .error 'user/mode-line-error 'user/mode-line-info))))))

(defun user/style-flycheck-errors (errors)
  "Style flycheck ERRORS."
  (let-alist errors
    (format "%s"
            (propertize "!"
                        'face (user/active-face
                               (cond (.error 'user/mode-line-error)
                                     (.warning 'user/mode-line-alert)
                                     (.info 'user/mode-line-success)
                                     (t 'user/mode-line-info)))))))

(defun user/flycheck-count-segment (&optional status)
  "Return a mode-line segment describing STATUS."
  (let ((text (pcase (or status flycheck-last-status-change)
                ('not-checked "")
                ('no-checker "")
                ('running
                 (progn
                   (user/start-mode-line-spinner 'flycheck)
                   (if user/-flycheck-errors
                     (user/style-flycheck-errors user/-flycheck-errors)
                     "")))
                ('errored (format "%s"
                                  (propertize "?"
                                              'face (user/active-face 'user/mode-line-error))))
                ('finished
                 (if flycheck-current-errors
                     (user/style-flycheck-errors
                      (setq user/-flycheck-errors (flycheck-count-errors flycheck-current-errors)))
                   ""))
                ('interrupted "")
                ('suspicious (format "%s"
                                     (propertize "?"
                                                 'face (user/active-face 'user/mode-line-alert)))))))
    (unless (eq (or status flycheck-last-status-change) 'running)
      (user/stop-mode-line-spinner 'flycheck))
    text))

(defun user/flycheck-segment ()
  "Show an alert indicator when there are any flycheck errors."
  (pcase flycheck-last-status-change
    ('running
     (progn
       (user/start-mode-line-spinner 'flycheck)
       (if user/-flycheck-errors
           (user/show-mode-line-alert 'flycheck 'error)
         (user/hide-mode-line-alert 'flycheck))))
    ('finished
     (if flycheck-current-errors
         (progn (setq user/-flycheck-errors (flycheck-count-errors flycheck-current-errors))
                (user/show-mode-line-alert 'flycheck 'error))
       (user/hide-mode-line-alert 'flycheck)))
    ((or 'errored 'suspicious)
     (user/show-mode-line-alert 'flycheck 'warn))
    ((or 'not-checked 'no-checker 'interrupted)
     (user/hide-mode-line-alert 'flycheck)))
  (unless (eq flycheck-last-status-change 'running)
    (user/stop-mode-line-spinner 'flycheck))
  "")

(defun user/project-and-branch-name-segment ()
  "Return project name segment, possibly with git branch."
  (let* ((project-name (user/get-project-name))
         (branch-name (user/get-branch-name)))
    (cond ((and project-name branch-name)
           (concat project-name ":" branch-name " "))
          ((or project-name branch-name)
           (concat project-name branch-name " ")))))

(defun user/special-buffer-p ()
  "Return t if buffer is a temp buffer."
  (string-prefix-p "*" (buffer-name)))

(defun user/shell-buffer-p ()
  "Return t if buffer is a shell buffer."
  (or (eq major-mode 'eshell-mode)
      (eq major-mode 'term-mode)
      (eq major-mode 'vterm-mode)))

(defun user/project-name-segment ()
  "Return project name segment."
  (when (or (user/shell-buffer-p)
            (not (user/special-buffer-p)))
    (let ((project-name (user/get-project-name)))
      (and project-name
           (concat project-name "/")))))

(defun user/get-git-file-status ()
  "Get status of file from git."
  (when vc-mode
    (let* ((state (vc-state buffer-file-name))
           (symbol (cond ((memq state '(edited needs-merge conflict)) "*")
                         ((memq state '(added)) "+")
                         ((memq state '(needs-update removed)) "!")
                         ((memq state '(removed)) "-")
                         ((memq state '(unregistered)) "?")))
           (face (cond ((memq state '(edited added need-merge removed)) (user/active-face 'user/mode-line-warning))
                       ((memq state '(needs-update conflict unregistered)) (user/active-face 'user/mode-line-error)))))
      (when symbol
        (format "(%s)" (propertize symbol 'face face))))))

(defun user/buffer-name-segment-with-status ()
  "Return a mode-line segment with the name of the current buffer."
  (let* ((status (user/get-git-file-status))
         (status (if status (concat " " status) ""))
         (face (cond ((not (user/selected-window-p)) 'mode-line-inactive)
                     ((eq major-mode 'eshell-mode) 'user/mode-line-buffer-name)
                     ((buffer-modified-p) 'user/mode-line-changed)
                     (buffer-read-only 'user/mode-line-locked)
                     (t 'user/mode-line-buffer-name))))
    (concat (propertize "%b" 'face face) status)))

(defun user/buffer-name-segment ()
  "Return a mode-line segment with the name of the current buffer."
  (cond
   ((eq major-mode 'eshell-mode) "")
   ((eq major-mode 'term-mode) "")
   ((eq major-mode 'vterm-mode) "")
   (t  (propertize
        "%b"
        'face (cond
               ((not (user/selected-window-p)) 'mode-line-inactive)
               ((buffer-modified-p) 'user/mode-line-changed)
               (buffer-read-only 'user/mode-line-locked)
               (t 'user/mode-line-buffer-name))))))

(defun user-mode-line ()
  "Return mode-line format."
  `("%e"
    (:eval
     (let ((lhs (list
                 (powerline-raw (user/evil-state-segment))
                 (powerline-raw " ")
                 (powerline-raw (user/project-name-segment))
                 (powerline-raw (user/buffer-path-segment))
                 (powerline-raw (user/buffer-name-segment))))
           (rhs (list
                 (powerline-raw (user/flycheck-segment))
                 (powerline-raw " ")
                 (powerline-hud (user/active-face
                                 (user/get-evil-state-highlight-face))
                                'user/mode-line-hud-disabled 5))))
       (concat
        (powerline-render lhs)
        (powerline-fill (user/active-face 'mode-line) (powerline-width rhs))
        (powerline-render rhs))))))

(provide 'user-mode-line)
;;; user-mode-line.el ends here
