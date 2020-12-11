;;; user.core --- core emacs configuration

;;; Commentary:

;;; Code:
;; (display-pixel-width)
;; (display-pixel-height)

;; (setq initial-frame-alist
;;       `((top . 0) (left . ,(* (/ (display-pixel-height) 9) 2)) (width . ,(* (/ (display-pixel-height) 9) 5)) (height . ,(display-pixel-height))))

;; (set-frame-position (selected-frame) (round (* (/ (display-pixel-width) 9.0) 2)) 0)
;; (set-frame-size (selected-frame) (- (round (* (/ (display-pixel-width) 9.0) 5)) 15) (display-pixel-height) t)

;; (setq default-frame-alist
;;       `((top . 0) (left . ,(* (/ (display-pixel-width) 9) 2)) (width . ,(* (/ (display-pixel-width) 9) 5)) (height . ,(display-pixel-height))))

;; (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
;; (add-to-list 'default-frame-alist '(ns-appearance . dark)) ;; assuming you are using a dark theme

(require 'use-package)

;; (eval-when-compile
;;   (defvar ns-use-proxy-icon)
;;   (defvar ns-use-srgb-colorspace)
;;   (defvar undo-tree-auto-save-history)
;;   (defvar savehist-file)
;;   (defvar undo-tree-history-directory-alist)
;;   (defvar global-auto-revert-non-file-buffers)
;;   (defvar auto-revert-verbose)
;;   (defvar longlines-show-hard-newlines)
;;   (defvar package--init-file-ensured)
;;   (defvar ns-use-native-fullscreen)
;;   (defvar ns-pop-up-frames))

;; (defvar user/backup-directory
;;   (expand-file-name (concat user-emacs-directory "backups")))

;; (defvar user/custom-file
;;   (expand-file-name "custom.el" user-emacs-directory))

;; (defvar user/private-file
;;   (expand-file-name "private.el" user-emacs-directory))

;; (defvar user/savehist-file
;;   (expand-file-name (concat user-emacs-directory ".cache/savehist")))


;; (require 'custom)
;; (require 'private)

(fset 'yes-or-no-p 'y-or-n-p)
(savehist-mode 1)
(global-auto-revert-mode 1)

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'hack-local-variables-hook (lambda () (setq truncate-lines t)))

;; check when opening large files
(defun user/check-large-file ()
  "Check if a file is too large."
  (when (> (buffer-size) (* 1024 1024))
    (when (y-or-n-p "Open file literally? ")
      (setq buffer-read-only t)
      (buffer-disable-undo)
      (fundamental-mode))))

(add-hook 'find-file-hook 'user/check-large-file)

(when (>= emacs-major-version 25)
  (eval-after-load 'bytecomp
    '(add-to-list 'byte-compile-not-obsolete-funcs
                  'preceding-sexp)))

(with-eval-after-load "evil"
  ;; On OSX, stop copying each visual state move to the clipboard:
  ;; https://bitbucket.org/lyro/evil/issue/336/osx-visual-state-copies-the-region-on
  ;; Most of this code grokked from:
  ;; http://stackoverflow.com/questions/15873346/elisp-rename-macro
  (when (or (featurep 'mac) (featurep 'ns))
    (advice-add 'evil-visual-update-x-selection :override 'ignore)))

;; (require 'cask "/usr/local/share/emacs/site-lisp/cask/cask.el")
;; (cask-initialize)


;; (require 'lsp-mode)

;; (use-package lsp-mode
;;     :commands (lsp lsp-deferred))

;; (defvar vterm-extra-line-mode-map nil)
;; (setq vterm-extra-line-mode-map (make-sparse-keymap))

;; (define-key vterm-extra-line-mode-map (kbd "<return>")
;;   'vterm-extra-read-and-send)

;; (define-minor-mode vterm-extra-line-mode "VTermLine"
;;   "Vterm extra line mode."
;;   (read-only-mode -1))

;; (defun vterm-extra-read-and-send ()
;;   (interactive)
;;   (let ((command (buffer-substring-no-properties
;;                  (vterm--get-prompt-point) (vterm--get-end-of-line))))
;;     (vterm-send-C-a)
;;     (vterm-send-C-k)
;;     (vterm-send-string command)
;;     (vterm-send-return)))

;; (use-package dash)

(use-package hl-todo
  :init (progn
          (setq hl-todo-activate-in-modes '(prog-mode))
          (global-hl-todo-mode)))

;; (use-package quickrun
;;   :config (progn
;;             (setq quickrun-timeout-seconds 60)
;;             (quickrun-add-command
;;               "python3"
;;               '((:command . "python3"))
;;               :default "python")))

(use-package which-key
  :config (progn
            (which-key-mode)
            (which-key-setup-minibuffer)))

(defmacro -> (&rest body)
  "Arrow the BODY."
  (let ((result (pop body)))
    (dolist (form body result)
      (setq result (append (list (car form) result)
                           (cdr form))))))

(defmacro ->> (&rest body)
  "Double arrow the BODY."
  (let ((result (pop body)))
    (dolist (form body result)
      (setq result (append form (list result))))))

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

(defun user/switch-to-last-buffer ()
  "Switch to last buffer in current window."
  (interactive)
  (switch-to-buffer nil))

(provide 'user-core)
;;; user-core.el ends here
