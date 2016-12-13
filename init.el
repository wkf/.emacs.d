;;; inite.l -- emacs configuration file

;;; Commentary:

;; TODO:
;;  [x] evil leader
;;  [x] cleverparens
;;  [x] completion framework
;;  [x] window management/switching
;;  [x] perspective
;;  [x] evil extras
;;  [ ] mode line
;;  [x] configure backup files
;;  [x] fix line numbers
;;  [x] highlight current line
;;  [x] tweak all the colors
;;  [ ] eshell (change color of lambda with evil state change)
;;  [x] flycheck
;;  [x] code completion
;;  [ ] quick run
;;  [ ] repls
;;  [x] git diff
;;  [ ] clojure
;;  [x] highlight todos
;;  [x] prodigy
;;  [ ] shackle
;;  [ ] magit
;;  [ ] lispy
;;  [ ] hydra
;;  [ ] org
;;  [ ] project specific configuration
;;  [ ] clippy
;;  [ ] focus (plugin name)
;;  [ ] fix fringe indicators for long lines

;;; Code:

(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(defvar user/backup-directory
  (expand-file-name (concat user-emacs-directory "backups")))

(defvar user/custom-file
  (expand-file-name "custom.el" user-emacs-directory))

(defvar user/private-file
  (expand-file-name "private.el" user-emacs-directory))

(setq-default
 package--init-file-ensured t
 package-enable-at-startup nil
 package-archives
 '(("gnu"   . "http://elpa.gnu.org/packages/")
   ("melpa" . "http://melpa.org/packages/")
   ("org"   . "http://orgmode.org/elpa/")))

(require 'cask "/usr/local/share/emacs/site-lisp/cask/cask.el")
(cask-initialize)

(setq inhibit-startup-message t)
(setq ring-bell-function 'ignore)
(setq undo-tree-auto-save-history t)
(setq savehist-file
      (expand-file-name
       (concat user-emacs-directory ".cache/savehist")))

(savehist-mode 1)

(setq custom-file user/custom-file)
(load user/custom-file)
(load user/private-file)

(fset 'yes-or-no-p 'y-or-n-p)

;; Write backup files to own directory
(setq backup-directory-alist
      `(("." . , user/backup-directory)))

(setq undo-tree-history-directory-alist
      `(("." . , user/backup-directory)))

(setq auto-save-file-name-transforms
      `((".*" ,user/backup-directory t)))


(setq version-control t
      kept-new-versions 10
      kept-old-versions 0
      delete-old-versions t
      backup-by-copying t
      vc-make-backup-files t)

;; Auto refresh
(global-auto-revert-mode 1)
;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil)

;; ;; don't let the cursor go into minibuffer prompt
;; ;; Tip taken from Xah Lee: http://ergoemacs.org/emacs/emacs_stop_cursor_enter_prompt.html
;; (setq minibuffer-prompt-properties
;;       '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt))
;; use only spaces and no tabs
(setq-default indent-tabs-mode nil
              tab-width 2)

;; Text
(setq longlines-show-hard-newlines t)

;; Use system trash for file deletion
;; should work on Windows and Linux distros
;; on OS X, see contrib/osx layer
(setq delete-by-moving-to-trash t)

;; auto fill breaks line beyond buffer's fill-column
(setq-default fill-column 80)

;; Save clipboard contents into kill-ring before replace them
(setq save-interprogram-paste-before-kill t)

;; Single space between sentences is more widespread than double
(setq-default sentence-end-double-space nil)

;; The C-d rebinding that most shell-like buffers inherit from
;; comint-mode assumes non-evil configuration with its
;; `comint-delchar-or-maybe-eof' function, so we disable it
(with-eval-after-load 'comint
  (define-key comint-mode-map (kbd "C-d") nil));; use only spaces and no tabs
(setq-default indent-tabs-mode nil
              tab-width 2)

(require 'dash)
(require 'use-package)

(setq ns-use-srgb-colorspace t)

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'hack-local-variables-hook (lambda () (setq truncate-lines t)))

(setq create-lockfiles nil)

(setq initial-major-mode 'emacs-lisp-mode)

;; (setq scroll-step 1)

(setq eval-expression-print-length nil
      eval-expression-print-level nil)

;;; window

(setq frame-resize-pixelwise t
      window-resize-pixelwise t
      window-combination-resize t)

(add-to-list 'initial-frame-alist `(fullscreen . fullheight))
(add-to-list 'default-frame-alist `(fullscreen . fullheight))

(toggle-frame-maximized)

(setq ;; Prefixes: Command = M, Alt = A
 mac-command-modifier 'meta
 mac-option-modifier  'alt
 mouse-wheel-scroll-amount '(5 ((shift) . 2))  ; one line at a time
 mouse-wheel-progressive-speed nil             ; don't accelerate scrolling
 ;; Curse Lion and its sudden but inevitable fullscreen mode!
 ;; NOTE Meaningless to railwaycat's emacs-mac build
 ns-use-native-fullscreen t
 ;; Don't open files from the workspace in a new frame
 ns-pop-up-frames nil)

;; On OSX, in GUI Emacs, `exec-path' isn't populated properly (it should match
;; $PATH in my shell). `exe-path-from-shell' fixes this.
(when window-system
  (setenv "SHELL" "/usr/local/bin/bash")
  ;; `exec-path-from-shell' is slow, so bring out the cache
  (setq exec-path
        (eval-when-compile
          (require 'exec-path-from-shell)
          (exec-path-from-shell-initialize)
          exec-path)))

(with-eval-after-load "evil"
  ;; On OSX, stop copying each visual state move to the clipboard:
  ;; https://bitbucket.org/lyro/evil/issue/336/osx-visual-state-copies-the-region-on
  ;; Most of this code grokked from:
  ;; http://stackoverflow.com/questions/15873346/elisp-rename-macro
  (when (or (featurep 'mac) (featurep 'ns))
    (advice-add 'evil-visual-update-x-selection :override 'ignore)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar user/leader-key ",")

(use-package general
  :config (progn
            (setq general-default-keymaps 'evil-normal-state-map)

            (general-define-key
             :keymaps '(evil-normal-state-map
                        evil-motion-state-map)
             "j" 'evil-next-visual-line
             "k" 'evil-previous-visual-line
             "gj" 'evil-next-line
             "gk" 'evil-previous-line)

            (general-define-key
             :keymaps '(evil-normal-state-map
                        evil-insert-state-map)
             "C-h" 'windmove-left
             "C-l" 'windmove-right
             "C-k" 'windmove-up
             "C-j" 'windmove-down)

            (general-define-key
             :prefix user/leader-key
             "w=" 'balance-windows
             "wo" 'delete-other-windows
             "ws" 'split-window-vertically
             "wv" 'split-window-horizontally)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun evil-unimpaired/paste-above ()
  "Paste above current line."
  (interactive)
  (evil-insert-newline-above)
  (evil-paste-after 1))

(defun evil-unimpaired/paste-below ()
  "Paste below current line."
  (interactive)
  (evil-insert-newline-below)
  (evil-paste-after 1))

(defun evil-unimpaired/insert-space-above (count)
  "Insert COUNT newlines above current line."
  (interactive "p")
  (dotimes (_ count) (save-excursion (evil-insert-newline-above))))

(defun evil-unimpaired/insert-space-below (count)
  "Insert COUNT newlines below current line."
  (interactive "p")
  (dotimes (_ count) (save-excursion (evil-insert-newline-below))))

(use-package evil
  :init (setq evil-want-C-u-scroll t)
  :config (progn
            (evil-mode 1)
            (setq evil-cross-lines t)
            (general-define-key
             "\\" 'evil-repeat-find-char-reverse
             "[ p" 'evil-unimpaired/paste-above
             "] p" 'evil-unimpaired/paste-below
             "[ SPC" 'evil-unimpaired/insert-space-above
             "] SPC" 'evil-unimpaired/insert-space-below)))

(use-package evil-surround
  :config (global-evil-surround-mode 1))

(use-package evil-commentary
  :config (evil-commentary-mode))

(use-package evil-snipe
  :config (evil-snipe-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package smartparens-config
  :config (progn
            (add-hook 'emacs-lisp-mode-hook 'smartparens-strict-mode)
            (general-define-key
             :keymaps 'emacs-lisp-mode-map
             :states '(normal)
             :prefix "g"
             "j" 'sp-join-sexp
             "s" 'sp-split-sexp
             "r" 'sp-raise-sexp
             "t" 'sp-transponse-sexp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package evil-cleverparens
  :init (setq-default
         evil-cp-additional-movement-keys
         '(("L"   . evil-cp-forward-sexp)
           ("H"   . evil-cp-backward-sexp)
           ("M-l" . evil-cp-end-of-defun)
           ("M-h" . evil-cp-beginning-of-defun)
           ("("   . evil-cp-backward-up-sexp)
           (")"   . evil-cp-up-sexp)))
  :config (progn
            (add-hook 'emacs-lisp-mode-hook 'evil-cleverparens-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package ivy
  :config (progn
            (ivy-mode 1)

            (setq ivy-use-virtual-buffers t)

            (general-define-key
             :keymaps 'ivy-minibuffer-map
             "C-c C-c" 'minibuffer-keyboard-quit
             "ESC ESC ESC" 'minibuffer-keyboard-quit)

            (general-define-key
             "/" 'swiper)

            (general-define-key
             :prefix user/leader-key
             "," 'ivy-resume
             ":" 'counsel-M-x
             "bb" 'ivy-switch-buffer
             "bd" (lambda () (interactive) (kill-buffer))
             "ff" 'counsel-find-file)))

(use-package projectile
  :config (projectile-mode))

(use-package perspective
  :config (persp-mode))

(use-package counsel-projectile
  :config (progn
            (require 'persp-projectile)

            (counsel-projectile-on)
            (general-define-key
             "SPC" 'counsel-projectile)
            (general-define-key
             :prefix user/leader-key
             "pf" 'counsel-projectile-find-file
             "pd" 'counsel-projectile-find-dir
             "pb" 'counsel-projectile-switch-to-buffer
             "p/" 'counsel-projectile-ag
             "pp" 'projectile-persp-switch-project)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'pcomplete)

(use-package eshell
  :config (progn
            ;; http://emacs.stackexchange.com/questions/27849/how-can-i-setup-eshell-to-use-ivy-for-tab-completion
            (add-hook 'eshell-mode-hook
                      (lambda ()
                        (define-key eshell-mode-map (kbd "<tab>")
                          (lambda () (interactive) (pcomplete-std-complete)))))
            (general-define-key
             :keymaps 'eshell-mode-map
             :states '(normal insert)
             "C-n" 'eshell-next-input
             "C-p" 'eshell-previous-input
             "C-r" 'counsel-esh-history)))

(use-package multi-eshell
  :config (progn
            (setq multi-eshell-name "*eshell*"
                  multi-eshell-shell-function '(eshell))
            (general-define-key
             :prefix user/leader-key
             "sn" 'multi-eshell
             "ss" 'multi-eshell-switch
             "sb" 'multi-eshell-go-back)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package base16-theme
  :init (progn
          (load-theme 'base16-eighties)

          (defvar user/red
            (plist-get base16-eighties-colors :base08))

          (defvar user/orange
            (plist-get base16-eighties-colors :base09))

          (defvar user/yellow
            (plist-get base16-eighties-colors :base0A))

          (defvar user/green
            (plist-get base16-eighties-colors :base0B))

          (defvar user/cyan
            (plist-get base16-eighties-colors :base0C))

          (defvar user/blue
            (plist-get base16-eighties-colors :base0D))

          (defvar user/purple
            (plist-get base16-eighties-colors :base0E))

          (setq evil-emacs-state-cursor   `(,user/yellow box)
                evil-insert-state-cursor  `(,user/green bar)
                evil-motion-state-cursor  `(,user/purple box)
                evil-normal-state-cursor  `(,user/blue box)
                evil-replace-state-cursor `(,user/red bar)
                evil-visual-state-cursor  `(,user/orange box))))

(use-package company
  :config (progn
            (global-company-mode)
            (setq company-idle-delay 0.2)
            (general-define-key
             :keymaps 'company-active-map
             "C-n" 'company-select-next
             "C-p" 'company-select-previous)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package eval-sexp-fu
  :config (progn
            (general-define-key
             :prefix user/leader-key
             :keymaps 'emacs-lisp-mode-map
             :states '(normal)
             "mes" 'eval-sexp-fu-eval-sexp-inner-sexp
             "mef" 'eval-sexp-fu-eval-sexp-inner-sexp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package linum
  :config (progn
            (setq linum-format " %3d ")
            (add-hook 'prog-mode-hook 'linum-mode)
            (add-hook 'text-mode-hook 'linum-mode)))

(use-package hl-line
  :init (add-hook 'prog-mode-hook 'hl-line-mode)
  :config (setq hl-line-sticky-flag nil
                global-hl-line-sticky-flag nil))

(use-package shackle)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro def-popup! (&rest params)
  "Define a shackle popup with PARAMS."
  `(push ',params shackle-rules))

(use-package gitconfig-mode
  :mode ("/\\.?git/?config$" "/\\.gitmodules$")
  :init (add-hook 'gitconfig-mode-hook 'flyspell-mode))

(use-package gitignore-mode
  :mode ("/\\.gitignore$"
         "/\\.git/info/exclude$"
         "/git/ignore$"))

(use-package git-gutter
  :commands git-gutter-mode
  :init (progn
          (add-hook 'text-mode-hook 'git-gutter-mode)
          (add-hook 'prog-mode-hook 'git-gutter-mode)
          (add-hook 'conf-mode-hook 'git-gutter-mode))
  :config (progn
            (require 'git-gutter-fringe)

            (def-popup! "^\\*git-gutter.+\\*$" :align below :size 15 :noselect t :regexp t)

            ;; colored fringe "bars"
            (define-fringe-bitmap 'git-gutter-fr:added
              ;; [224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224]
              [192 192 192 192 192 192 192 192 192 192 192 192 192 192 192 192 192 192 192 192 192 192 192 192 192]
              nil nil 'center)
            (define-fringe-bitmap 'git-gutter-fr:modified
              ;; [224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224]
              [192 192 192 192 192 192 192 192 192 192 192 192 192 192 192 192 192 192 192 192 192 192 192 192 192]
              nil nil 'center)
            (define-fringe-bitmap 'git-gutter-fr:deleted
              ;; [0 0 0 0 0 0 0 0 0 0 0 0 0 128 192 224 240 248]
              [192 192 192 192 192 192 192 192 192 192 192 192 192 192 192 192 192 192 192 192 192 192 192 192 192]
              nil nil 'center)

            ;;
            ;;
            ;; 000
            ;;
            ;; 00010000
            ;; 00011000

            ;; Refreshing git-gutter
            (advice-add 'evil-force-normal-state :after 'git-gutter)
            (add-hook 'focus-in-hook 'git-gutter:update-all-windows)))

(use-package git-messenger
  :commands git-messenger:popup-message
  :init (defvar git-messenger-map (make-sparse-keymap))
  :config (progn
            (def-popup! "*git-messenger*" :align left :size 55 :select t)
            (setq git-messenger:show-detail t)
            (general-define-key
             :keymaps 'git-messenger-map
             "q" 'git-messenger:popup-close)))

(use-package magit
  :commands (magit-status)
  :config (progn
            (setq magit-completing-read-function 'ivy-completing-read)
            (add-hook 'magit-mode-hook 'turn-off-evil-snipe-override-mode)
            (require 'evil-magit)))

;; (after! vc-annotate
;;   (evil-set-initial-state 'vc-annotate-mode     'normal)
;;   (evil-set-initial-state 'vc-git-log-view-mode 'normal)
;;   (map! :map vc-annotate-mode-map
;;         :n "q" 'kill-this-buffer
;;         :n "d" 'vc-annotate-show-diff-revision-at-line
;;         :n "D" 'vc-annotate-show-changeset-diff-revision-at-line
;;         :n "SPC" 'vc-annotate-show-log-revision-at-line
;;         :n "]]" 'vc-annotate-next-revision
;;         :n "[[" 'vc-annotate-prev-revision
;;         :n [tab] 'vc-annotate-toggle-annotation-visibility
;;         :n "RET" 'vc-annotate-find-revision-at-line))

;; (use-package browse-at-remote
;;   :commands (browse-at-remote/browse browse-at-remote/get-url))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(require 'flycheck)

;; XXX: redef this function to work around a bug trying to use a deleted buffer
(defun flycheck-global-teardown ()
  "Teardown Flycheck in all buffers.
Completely clear the whole Flycheck state in all buffers, stop
all running checks, remove all temporary files, and empty all
variables of Flycheck."
  (dolist (buffer (buffer-list))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when flycheck-mode
          (flycheck-teardown))))))

(use-package flycheck
  :config (progn
            ;; (require 'flycheck-pos-tip)
            (global-flycheck-mode)
            ;; (flycheck-pos-tip-mode)
            (setq flycheck-indication-mode 'right-fringe)

            (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
              ;; [224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224]
              [3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3]
              nil nil 'center)

            ))

(use-package rainbow-delimiters
  :config (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode))

(use-package hl-todo
  :commands (hl-todo-mode)
  :init (progn
          (setq hl-todo-activate-in-modes '(prog-mode))
          (global-hl-todo-mode)))


(use-package eshell-prompt-extras
  :config (setq eshell-highlight-prompt nil
                eshell-prompt-function 'epe-theme-lambda))

(use-package prodigy
  :init (add-to-list 'evil-emacs-state-modes 'prodigy-mode)
  :config (progn
            (add-hook 'prodigy-mode-hook (lambda () (evil-emacs-state)))
            (general-define-key
             :prefix user/leader-key
             "S" 'prodigy)

            (general-define-key
             :keymaps 'prodigy-mode-map
             :states '(emacs normal)
             "h" 'prodigy-first
             "j" 'prodigy-next
             "k" 'prodigy-prev
             "l" 'prodigy-last
             "H" 'prodigy-display-process
             "J" 'prodigy-next-with-status
             "K" 'prodigy-prev-with-status
             "L" 'prodigy-start
             "d" 'prodigy-jump-dired
             "g" 'prodigy-jump-magit
             "Y" 'prodigy-copy-cmd)

            (prodigy-define-service
              :name "mesos-master"
              :command "/usr/local/sbin/mesos-master"
              :args '("--ip=127.0.0.1" "--zk=zk://127.0.0.1:2181/mesos" "--quorum=1"
                      "--work_dir=/tmp/mesos")
              :tags '(mesos)
              :stop-signal 'kill)
            ;; "--resources=cpus(*):2.0;cpus(prod):2.0;mem(*):600.0;mem(prod):600.0;ports(*):[30000-31000];ports(prod):[32000-33000]"

            (prodigy-define-service
              :name "mesos-slave-1"
              :command "/usr/local/sbin/mesos-slave"
              :args '("--master=127.0.0.1:5050"
                      "--no-switch_user"
                      "--hostname=localhost"
                      "--port=5051"
                      "--work_dir=/tmp/mesos_slave_1"
                      "--resources=cpus(*):2.0;cpus(prod):2.0;mem(*):600.0;mem(prod):600.0;ports(*):[30000-30999];ports(prod):[31000-31999]"
                      "--default_role=prod")
              :tags '(mesos)
              :stop-signal 'kill)

            (prodigy-define-service
              :name "mesos-slave-2"
              :command "/usr/local/sbin/mesos-slave"
              :args '("--master=127.0.0.1:5050"
                      "--no-switch_user"
                      "--hostname=localhost"
                      "--port=5052"
                      "--work_dir=/tmp/mesos_slave_2"
                      "--resources=cpus(*):2.0;cpus(prod):2.0;mem(*):600.0;mem(prod):600.0;ports(*):[32000-32999];ports(prod):[33000-33999]"
                      "--default_role=prod")
              :tags '(mesos)
              :stop-signal 'kill)))

;; XXX: all busted up

;; (define-fringe-bitmap 'user/scroll-bar
;;   ;; [224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224]
;;   [3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3]
;;   nil nil 'center)

;; (defun user/get-scroll-point ()
;;   "Get scroll point."
;;   (cond
;;    ((eq (point-min) (window-start))
;;     (window-start))
;;    (t
;;     ;; (+ (window-start) (/ (- (window-end) (window-start)) 2))
;;     (point)
;;     )
;;    ((<= (point-max) (window-end))
;;     (window-end))
;;    ))

;; (defun user/create-scroll-indicator ()
;;   "Create scroll bar-like indicator in right fringe."
;;   (ind-create-indicator 'user/get-scroll-point
;;                         :managed t
;;                         ;; :relative nil
;;                         :face 'fringe
;;                         :fringe 'right-fringe
;;                         ;; :bitmap 'user/scroll-bar
;;                         :priority 200))

;; (use-package indicators
;;   :config (progn
;;             (add-hook 'prog-mode-hook 'user/create-scroll-indicator)
;;             (add-hook 'text-mode-hook 'user/create-scroll-indicator)))

;; (use-package yascroll
;;   :config (progn
;;             (setq yascroll:scroll-bar 'text-area)
;;             (global-yascroll-bar-mode 1)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (use-package sublimity
;;   :config (progn
;;             (require 'sublimity-map)
;;             (require 'sublimity-scroll)

;;             (setq sublimity-scroll-weight 10
;;                   sublimity-scroll-drift-length 5)

;;             (sublimity-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'vc-git)
(require 'projectile)
(require 'spinner)
(require 'powerline)

(defgroup user/mode-line nil
  ""
  :group 'user)

(defface user/mode-line-info
  `((t (:inherit mode-line)))
  "")

(defface user/mode-line-changed
  `((t (:inherit mode-line :foreground ,user/yellow :slant italic)))
  "")

(defface user/mode-line-locked
  `((t (:inherit mode-line :foreground ,user/red :slant italic)))
  "")

(defface user/mode-line-added
  `((t (:inherit mode-line :foreground ,user/green)))
  "")

(defface user/mode-line-deleted
  `((t (:inherit mode-line :foreground ,user/red)))
  "")

(defface user/mode-line-warning
  `((t (:inherit mode-line :foreground ,user/yellow)))
  "")

(defface user/mode-line-alert
  `((t (:inherit mode-line :foreground ,user/orange)))
  "")

(defface user/mode-line-error
  `((t (:inherit mode-line :foreground ,user/red)))
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

(defface user/mode-line-hud-enabled
  `((t (:inherit mode-line :background ,(face-foreground 'mode-line))))
  "")

(defface user/mode-line-hud-disabled
  `((t (:inherit mode-line)))
  "")

(defface user/mode-line-emacs-state
  `((t (:foreground ,(face-background 'default) :background ,user/yellow)))
  "")

(defface user/mode-line-insert-state
  `((t (:foreground ,(face-background 'default) :background ,user/green)))
  "")

(defface user/mode-line-motion-state
  `((t (:foreground ,(face-background 'default) :background ,user/purple)))
  "")

(defface user/mode-line-normal-state
  `((t (:foreground ,(face-background 'default) :background ,user/blue)))
  "")

(defface user/mode-line-replace-state
  `((t (:foreground ,(face-background 'default) :background ,user/red)))
  "")

(defface user/mode-line-visual-state
  `((t (:foreground ,(face-background 'default) :background ,user/orange)))
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
      (let* ((state (user/get-evil-state))
             (face (assq state user/evil-state-faces)))
        (or (cdr face) 'default))
    'default))

;; (user/get-evil-state-highlight-face)

(defun user/get-project-name ()
  "Get current projectile project name."
  (when (projectile-project-p)
    (propertize (projectile-project-name)
		'face (user/active-face 'user/mode-line-buffer-project))))

(defun user/project-root (&optional strict-p)
  "Get the path to the root of your project.  STRICT-P determines if project root is required."
  (let (projectile-require-project-root strict-p)
    (projectile-project-root)))

(defun user/buffer-path ()
  "Displays the buffer's full path relative to the project root, excluding the file basename."
  (when buffer-file-name
    (let* ((default-directory (f-dirname buffer-file-name))
           (buffer-path (f-relative (f-dirname buffer-file-name) (user/project-root)))
           (max-length (truncate (* (window-body-width) 0.4))))
      (when (and buffer-path (not (equal buffer-path ".")))
        (concat
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
	   buffer-path)
	 "/")))))

(defvar-local user/-mode-line-spinner nil)
(defvar-local user/-mode-line-spinning-p nil)

(defun user/evil-state-segment ()
  "Return a mode-line segment for the current evil state."
  (unless user/-mode-line-spinner
    (setq user/-mode-line-spinner (make-spinner 'rotating-line t)))
  (propertize
   (if user/-mode-line-spinning-p
       (concat
        "  "
        (spinner-start-print user/-mode-line-spinner)
        "  ")
     (progn
       (spinner-stop user/-mode-line-spinner)
       "  λ  "))
   'face (user/active-face (user/get-evil-state-highlight-face))))

(defun user/git-p ()
  "Can we find git."
  (or (and buffer-file-name (vc-find-root buffer-file-name ".git"))
      (and (eq major-mode 'eshell-mode) (vc-find-root (eshell/pwd) ".git"))))

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

(defun user/get-branch-name ()
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

(defun user/get-major-mode ()
  "Return the major mode, including process info."
  (concat (format-mode-line mode-name)
          (if (stringp mode-line-process) mode-line-process)))

(defvar-local user/-flycheck-errors nil)

(defun user/active-face (face)
  "Return FACE if active window selected, else mode-line-inactive."
  (if (user/selected-window-p) face 'mode-line-inactive))

(defun user/style-flycheck-errors (errors)
  "Style flycheck ERRORS."
  (let-alist errors
    (format "(%s:%s)"
            (propertize (number-to-string (or .warning 0))
                        'face (user/active-face
                               (if .warning 'user/mode-line-alert 'user/mode-line-info)))
            (propertize (number-to-string (or .error 0))
                        'face (user/active-face
                               (if .error 'user/mode-line-error 'user/mode-line-info))))))

(defun user/flycheck-segment (&optional status)
  "Return a mode-line segment describing STATUS."
  (let ((text (pcase (or status flycheck-last-status-change)
                ('not-checked "(-)")
                ('no-checker "(-)")
                ('running
                 (progn
                   (setq user/-mode-line-spinning-p t)
                   (if user/-flycheck-errors
                       (user/style-flycheck-errors user/-flycheck-errors)
                     "(-)")))
                ('errored (format "(%s)"
                                  (propertize "!"
                                              'face (user/active-face 'user/mode-line-error))))
                ('finished
                 (if flycheck-current-errors
                     (user/style-flycheck-errors
                      (setq user/-flycheck-errors (flycheck-count-errors flycheck-current-errors)))
                   ""))
                ('interrupted "(-)")
                ('suspicious (format "(%s)"
                                     (propertize "?"
                                                 'face (user/active-face 'user/mode-line-alert)))))))
    (unless (eq (or status flycheck-last-status-change) 'running)
      (setq user/-mode-line-spinning-p nil))
    text))

(defun user/project-name-segment ()
  "Return project name segment, possibly with git branch."
  (let* ((project-name (user/get-project-name))
         (branch-name (user/get-branch-name)))
    (if (and project-name branch-name)
        (concat project-name ":" branch-name)
      (concat project-name branch-name))))

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

(defun user/buffer-name-segment ()
  "Return a mode-line segment with the name of the current buffer."
  (let* ((status (user/get-git-file-status))
         (status (if status (concat " " status) ""))
         (face (cond ((not (user/selected-window-p)) 'mode-line-inactive)
                     ((buffer-modified-p) 'user/mode-line-changed)
                     (buffer-read-only 'user/mode-line-locked)
                     (t 'user/mode-line-buffer-name))))
    (concat (propertize "%b" 'face face) status)))

;;
;; TODO:
;;  - color branch name based on whether it's master or on disconnected head
;;  - color project name based on working tree status
;;  - fix inactive colors for evil state segment
;;  - fix colors for when branch is on master
;;


;; vc-mode

(defun user/mode-line ()
  "Return mode-line format."
  `("%e"
    (:eval
     (let ((lhs (list
                 (powerline-raw (user/evil-state-segment))
                 (powerline-raw " ")
                 (powerline-raw (user/buffer-path))
                 (powerline-raw (user/buffer-name-segment))
                 (powerline-raw " ")
                 (powerline-raw (user/flycheck-segment))))
           (rhs (list
                 (powerline-raw (user/project-name-segment))
                 (powerline-raw " ")
                 (powerline-hud (user/active-face
                                 (user/get-evil-state-highlight-face))
                                'user/mode-line-hud-disabled 5))))
       (concat
        (powerline-render lhs)
        (powerline-fill (user/active-face 'mode-line) (powerline-width rhs))
        (powerline-render rhs))))))

(setq-default mode-line-format (user/mode-line))

;;; init.el ends here
