;;; user.shell --- shell configuration
;;; Commentary:
;;; Code:

(require 'pcomplete)
(require 'projectile)
(require 'xterm-color)

(defun user/tramp-file-p ()
  "Is current file loaded with tramp."
  (tramp-tramp-file-p default-directory))

(defun user/get-tramp-user ()
  "Return remote user name."
  (tramp-file-name-user (tramp-dissect-file-name default-directory)))

(defun user/get-tramp-host ()
  "Return remote host."
  (tramp-file-name-real-host (tramp-dissect-file-name default-directory)))

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

(defun user/run-new-eshell ()
  "Switch to eshell, ignoring IGNORED."
  (interactive)
  (let ((current-prefix-arg '(4)))
    (if (projectile-project-p)
        (let ((eshell-buffer-name (concat "*eshell " (projectile-project-name) "*")))
          (projectile-with-default-dir (projectile-project-root)
            (call-interactively 'eshell)))
      (call-interactively 'eshell))))

(setq xterm-color-names `[,user/black
                          ,user/red
                          ,user/green
                          ,user/yellow
                          ,user/blue
                          ,user/magenta
                          ,user/cyan
                          ,user/white])

(setq xterm-color-names-bright `[,user/bright-black
                                 ,user/red
                                 ,user/green
                                 ,user/yellow
                                 ,user/blue
                                 ,user/magenta
                                 ,user/cyan
                                 ,user/bright-white])

(use-package eshell
  :config (progn
            ;; http://emacs.stackexchange.com/questions/27849/how-can-i-setup-eshell-to-use-ivy-for-tab-completion

            (add-hook 'eshell-mode-hook
                      (lambda ()
                        (setenv "TERM" "xterm-256color")
                        (define-key eshell-mode-map (kbd "<tab>")
                          (lambda () (interactive) (pcomplete-std-complete)))))

            (add-hook 'eshell-before-prompt-hook
                      (lambda ()
                        (setq xterm-color-preserve-properties t)))

            (add-to-list 'eshell-preoutput-filter-functions 'xterm-color-filter)
            (setq eshell-output-filter-functions (remove 'eshell-handle-ansi-color eshell-output-filter-functions))

            ;; (add-hook 'eshell-pre-command-hook (lambda () (user/start-mode-line-spinner 'eshell 10)))
            ;; (add-hook 'eshell-post-command-hook (lambda () (user/stop-mode-line-spinner 'eshell)))
            ))

;; (use-package em-alias
;;   :config (progn
;;             (defalias 'E 'find-file)
;;             (defalias 'e 'find-file-other-window)
;;             (add-to-list 'eshell-command-aliases-list (list "l" "ls -alh"))))

(use-package eshell-prompt-extras
  :config (setq eshell-highlight-prompt t
                eshell-banner-message ""
                eshell-prompt-function 'user/eshell-prompt-theme))

;; On OSX, in GUI Emacs, `exec-path' isn't populated properly (it should match
;; $PATH in my shell). `exec-path-from-shell' fixes this.
(use-package exec-path-from-shell
  :config (when window-system
            (exec-path-from-shell-initialize)

            ;; (require 'rbenv)
            ;; (global-rbenv-mode)

            (setq-default eshell-path-env (getenv "PATH"))))

(defun after-term-line-mode ()
  (setq evil-emacs-state-cursor `(,user/yellow box)
        evil-insert-state-cursor `(,user/green bar)
        evil-motion-state-cursor `(,user/purple box)
        evil-normal-state-cursor `(,user/blue box)
        evil-replace-state-cursor `(,user/red bar)
        evil-visual-state-cursor `(,user/orange box))
  (evil-refresh-cursor))

(defun after-term-char-mode ()
  (setq evil-emacs-state-cursor `(,user/yellow hbar)
        evil-insert-state-cursor `(,user/green hbar)
        evil-motion-state-cursor `(,user/purple hbar)
        evil-normal-state-cursor `(,user/blue hbar)
        evil-replace-state-cursor `(,user/red hbar)
        evil-visual-state-cursor `(,user/orange hbar))
  (evil-refresh-cursor))

(add-hook 'term-mode-hook
          (lambda ()
            (make-local-variable 'evil-emacs-state-cursor)
            (make-local-variable 'evil-insert-state-cursor)
            (make-local-variable 'evil-motion-state-cursor)
            (make-local-variable 'evil-normal-state-cursor)
            (make-local-variable 'evil-replace-state-cursor)
            (make-local-variable 'evil-visual-state-cursor)))

(advice-add 'term-line-mode :after 'after-term-line-mode)
(advice-add 'term-char-mode :after 'after-term-char-mode)

(use-package shx
  :config
  (shx-global-mode 1))

(provide 'user.shell)
;;; shell.el ends here
