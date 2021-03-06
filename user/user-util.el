;;; user.util --- core emacs configuration

;;; Commentary:

;;; Code:

(require 'f)
;; (require 'ivy)
(require 'tramp)
;; (require 'cider)
;; (require 'projectile)

;; (defmacro -> (&rest body)
;;   "Arrow the BODY."
;;   (let ((result (pop body)))
;;     (dolist (form body result)
;;       (setq result (append (list (car form) result)
;;                            (cdr form))))))

;; (defmacro ->> (&rest body)
;;   "Double arrow the BODY."
;;   (let ((result (pop body)))
;;     (dolist (form body result)
;;       (setq result (append form (list result))))))

;; check when opening large files
(defun user/check-large-file ()
  "Check if a file is too large."
  (when (> (buffer-size) (* 1024 1024))
    (when (y-or-n-p "Open file literally? ")
      (setq buffer-read-only t)
      (buffer-disable-undo)
      (fundamental-mode))))

(defun user/projectile-switch-to-project-file (directory file)
  "Projectile action function, open projectile FILE within DIRECTORY."
  (let ((default-action projectile-switch-project-action))
    (setq projectile-switch-project-action
          (lambda ()
            (let ((default-file (f-join directory file)))
              (if (f-exists? default-file)
                  (find-file default-file)
                (message "The file %s doesn't exist in the selected project" default-file)))))
    (projectile-switch-project-by-name directory)
    (setq projectile-switch-project-action default-action)))

(defun user/switch-to-other-buffer ()
  "Switch to other buffer in current window."
  (interactive)
  (switch-to-buffer nil))

(defun user/rename-this-buffer-and-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (cond ((get-buffer new-name)
               (error "A buffer named '%s' already exists!" new-name))
              (t
               (rename-file filename new-name 1)
               (rename-buffer new-name)
               (set-visited-file-name new-name)
               (set-buffer-modified-p nil)
               (message "File '%s' successfully renamed to '%s'" name (file-name-nondirectory new-name))))))))

(defun user/tramp-file-p ()
  "Is current file loaded with tramp."
  (tramp-tramp-file-p default-directory))

(defun user/get-tramp-user ()
  "Return remote user name."
  (tramp-file-name-user (tramp-dissect-file-name default-directory)))

(defun user/get-tramp-host ()
  "Return remote host."
  (tramp-file-name-host (tramp-dissect-file-name default-directory)))

(defun user/eshell-prompt-theme ()
  "An eshell prompt theme."
  (setq eshell-prompt-regexp "^ [^#\nλ]*[#λ]  ")
  (concat
   (when (user/tramp-file-p)
     (propertize
      (concat " "
              (user/get-tramp-user)
              "@"
              (user/get-tramp-host))
      'face 'epe-remote-face))
   (propertize " λ"
               'face 'epe-symbol-face)
   (propertize (if (= (user-uid) 0) "#" "")
               'face 'epe-sudo-symbol-face)
   "  "))

(defun eshell/clear ()
  "Clear the eshell buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)))

(defun user/run-eshell ()
  "Open eshell, ignoring IGNORED."
  (interactive)
  (if (projectile-project-p)
      (projectile-run-eshell)
    (eshell)))

(defun user/run-new-eshell (&optional command)
  "Switch to eshell, optionally running COMMAND."
  (interactive)
  (let ((current-prefix-arg '(4)))
    (if (projectile-project-p)
        (let ((eshell-buffer-name (concat "*eshell " (projectile-project-name) "*")))
          (projectile-with-default-dir (projectile-project-root)
            (call-interactively 'eshell)))
      (call-interactively 'eshell)))
  (when command
    (eshell-return-to-prompt)
    (insert command)
    (eshell-send-input)))

(defun user/clojure-reload ()
  "Reload Clojure namespaces."
  (interactive)
  ("(require 'reloaded.repl) (reloaded.repl/reset)"
   cider-interactive-eval))

(defun user/clojure-reload-all ()
  "Reload all Clojure namespaces."
  (interactive)
  (cider-interactive-eval
   "(require 'reloaded.repl) (reloaded.repl/reset-all)"))

(defun user/clojure-aviary-browse ()
  "Browse to aviary page in default browser."
  (interactive)
  (cider-interactive-eval
   "(require 'reloaded.repl) (require 'aviary.core) (aviary.core/browse reloaded.repl/system)"))

(defun user/clojure-aviary-export ()
  "Use aviary to export current project."
  (interactive)
  (cider-interactive-eval
   "(require 'aviary.util) (@(resolve (symbol (str (aviary.util/lein-project-name) \".site\") \"export\")))"))

(defun user/-sql-connect (new)
  "Connect to sql, creating a new buffer if NEW is t."
  (if nil (if (get-buffer "*SQL*")
              (switch-to-buffer-other-window "*SQL*")
            (ivy-read "Connect: " sql-connection-alist
                      :action (lambda (db)
                                (setq sql-product
                                      (->> sql-connection-alist
                                           (alist-get (car db))
                                           (alist-get 'sql-product)
                                           (cadar)))
                                (if new
                                    (sql-connect (car db) (car db))
                                  (sql-connect (car db))))))))

(defun user/sql-connect ()
  "Connect to a sql database."
  (interactive)
  (user/-sql-connect nil))

(defun user/sql-new-connect ()
  "Connect to a sql database."
  (interactive)
  (user/-sql-connect t))

;; https://github.com/Fuco1/.emacs.d/blob/af82072196564fa57726bdbabf97f1d35c43b7f7/site-lisp/redef.el#L20-L94
(defun user/lisp-indent-function (indent-point state)
  "This function is the normal value of the variable `lisp-indent-function'.
The function `calculate-lisp-indent' calls this to determine
if the arguments of a Lisp function call should be indented specially.

INDENT-POINT is the position at which the line being indented begins.
Point is located at the point to indent under (for default indentation);
STATE is the `parse-partial-sexp' state for that position.

If the current line is in a call to a Lisp function that has a non-nil
property `lisp-indent-function' (or the deprecated `lisp-indent-hook'),
it specifies how to indent.  The property value can be:

* `defun', meaning indent `defun'-style
  \(this is also the case if there is no property and the function
  has a name that begins with \"def\", and three or more arguments);

* an integer N, meaning indent the first N arguments specially
  (like ordinary function arguments), and then indent any further
  arguments like a body;

* a function to call that returns the indentation (or nil).
  `lisp-indent-function' calls this function with the same two arguments
  that it itself received.

This function returns either the indentation to use, or nil if the
Lisp function does not specify a special indentation."
  (let ((normal-indent (current-column))
        (orig-point (point)))
    (goto-char (1+ (elt state 1)))
    (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
    (cond
     ;; car of form doesn't seem to be a symbol, or is a keyword
     ((and (elt state 2)
           (or (not (looking-at "\\sw\\|\\s_"))
               (looking-at ":")))
      (if (not (> (save-excursion (forward-line 1) (point))
                  calculate-lisp-indent-last-sexp))
          (progn (goto-char calculate-lisp-indent-last-sexp)
                 (beginning-of-line)
                 (parse-partial-sexp (point)
                                     calculate-lisp-indent-last-sexp 0 t)))
      ;; Indent under the list or under the first sexp on the same
      ;; line as calculate-lisp-indent-last-sexp.  Note that first
      ;; thing on that line has to be complete sexp since we are
      ;; inside the innermost containing sexp.
      (backward-prefix-chars)
      (current-column))
     ((and (save-excursion
             (goto-char indent-point)
             (skip-syntax-forward " ")
             (not (looking-at ":")))
           (save-excursion
             (goto-char orig-point)
             (looking-at ":")))
      (save-excursion
        (goto-char (+ 2 (elt state 1)))
        (current-column)))
     (t
      (let ((function (buffer-substring (point)
                                        (progn (forward-sexp 1) (point))))
            method)
        (setq method (or (function-get (intern-soft function)
                                       'lisp-indent-function)
                         (get (intern-soft function) 'lisp-indent-hook)))
        (cond ((or (eq method 'defun)
                   (and (null method)
                        (> (length function) 3)
                        (string-match "\\`def" function)))
               (lisp-indent-defform state indent-point))
              ((integerp method)
               (lisp-indent-specform method state
                                     indent-point normal-indent))
              (method
               (funcall method indent-point state))))))))

(provide 'user-util)
;;; user-util.el ends here
