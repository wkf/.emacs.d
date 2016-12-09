;;
;; TODO:
;;  [x] evil leader
;;  [x] cleverparens
;;  [x] completion framework
;;  [x] window management/switching
;;  [x] perspective
;;  [x] evil extras
;;  [ ] mode line
;;  [x] configure backup files
;;  [ ] project specific configuration
;;  [x] fix line numbers
;;  [ ] highlight current line
;;  [ ] tweak all the colors
;;  [ ] eshell
;;  [ ] flycheck
;;  [ ] code completion
;;  [ ] quick run
;;  [ ] repls
;;  [ ] git diff
;;  [ ] clojure
;;  [ ] highlight todos
;;  [ ] prodigy
;;  [ ] shackle
;;  [ ] magit
;;  [ ] lispy
;;  [ ] hydra
;;  [ ] org
;;
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

(setq custom-file user/custom-file)
(load user/custom-file)
(load user/private-file)

;; Write backup files to own directory
(setq backup-directory-alist
      `(("." . , user/backup-directory)))

(setq auto-save-file-name-transforms
      `((".*" ,user/backup-directory t)))

(setq version-control t
      kept-new-versions 10
      kept-old-versions 0
      delete-old-versions t
      backup-by-copying t
      vc-make-backup-files t)

(require 'dash)
(require 'use-package)

(setq ns-use-srgb-colorspace t)

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'hack-local-variables-hook (lambda () (setq truncate-lines t)))

(setq create-lockfiles nil)

;;; window

(setq frame-resize-pixelwise t
      window-resize-pixelwise t)

(add-to-list 'initial-frame-alist `(fullscreen . fullheight))
(add-to-list 'default-frame-alist `(fullscreen . fullheight))

(setq ;; Prefixes: Command = M, Alt = A
      mac-command-modifier 'meta
      mac-option-modifier  'alt
      ;; sane trackpad/mouse scroll settings
      mac-redisplay-dont-reset-vscroll t
      mac-mouse-wheel-smooth-scroll nil
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
	     "ws" 'split-window-vertically
	     "wv" 'split-window-horizontally)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun evil-unimpaired/paste-above ()
  (interactive)
  (evil-insert-newline-above)
  (evil-paste-after 1))

(defun evil-unimpaired/paste-below ()
  (interactive)
  (evil-insert-newline-below)
  (evil-paste-after 1))

(defun evil-unimpaired/insert-space-above (count)
  (interactive "p")
  (dotimes (_ count) (save-excursion (evil-insert-newline-above))))

(defun evil-unimpaired/insert-space-below (count)
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
  :init (setq evil-cp-additional-movement-keys
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

(defvar user/evil-state-faces)

(use-package base16-theme
  :init (progn
	  (load-theme 'base16-eighties)

	  (setq user/evil-state-faces
		`((emacs . ,(plist-get base16-eighties-colors :base0D))
		  (insert . ,(plist-get base16-eighties-colors :base0B))
		  (motion . ,(plist-get base16-eighties-colors :base0E))
		  (normal . ,(plist-get base16-eighties-colors :base0D))
		  (replace . ,(plist-get base16-eighties-colors :base08))
		  (visual . ,(plist-get base16-eighties-colors :base09))))

	  (setq evil-emacs-state-cursor   `(,(plist-get base16-eighties-colors :base0D) box)
		evil-insert-state-cursor  `(,(plist-get base16-eighties-colors :base0B) bar)
		evil-motion-state-cursor  `(,(plist-get base16-eighties-colors :base0E) box)
		evil-normal-state-cursor  `(,(plist-get base16-eighties-colors :base0D) box)
		evil-replace-state-cursor `(,(plist-get base16-eighties-colors :base08) bar)
		evil-visual-state-cursor  `(,(plist-get base16-eighties-colors :base09) box))))

(use-package company
  :config (progn
	    (global-company-mode)
	    (setq company-idle-delay 0.2)
	    (general-define-key
	     :keymaps 'company-active-map
	     "C-n" 'company-select-next
	     "C-p" 'company-select-previous)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package eval-sexp-fu
  :config (progn
	    (general-define-key
	     :prefix user/leader-key
	     :keymaps 'emacs-lisp-mode-map
	     :states '(normal)
	     "mes" 'eval-sexp-fu-eval-sexp-inner-sexp
	     "mef" 'eval-sexp-fu-eval-sexp-inner-sexp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package linum
  :config (progn
	    (setq linum-format "%3d")
	    (add-hook 'prog-mode-hook 'linum-mode)
	    (add-hook 'text-mode-hook 'linum-mode)))

(use-package shackle)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro def-popup! (&rest params)
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
	      [224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224]
	      nil nil 'center)
	    (define-fringe-bitmap 'git-gutter-fr:modified
	      [224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224]
	      nil nil 'center)
	    (define-fringe-bitmap 'git-gutter-fr:deleted
	      [0 0 0 0 0 0 0 0 0 0 0 0 0 128 192 224 240 248]
	      nil nil 'center)

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

(defsubst user/selected-window-p ()
  (eq (selected-window) powerline-selected-window))

(defun user/make-xpm (color height width)
  "Create an XPM bitmap."
  (when window-system
    (propertize
     " " 'display
     (let ((data nil)
	   (i 0))
       (setq data (make-list height (make-list width 1)))
       (pl/make-xpm "percent" color color (reverse data))))))

(defun user/get-evil-state ()
  (if (eq 'operator evil-state) evil-previous-state evil-state))

(defun user/get-face-height (face)
  (aref (font-info (face-font face)) 2))

(defun user/get-face-width (face)
  (aref (aref (font-get-glyphs (face-attribute face :font) 65 66) 0) 4))

(defun user/get-evil-state-highlight-face ()
  "Set the highlight face depending on the evil state."
  (if (bound-and-true-p evil-local-mode)
      (let* ((state (user/get-evil-state))
             (face (assq state user/evil-state-faces)))
        (if face (cdr face) (face-foreground 'default)))
    (face-foreground 'default)))

(defun user/project-name ()
  (when (projectile-project-p)
    (projectile-project-name)))

(defun user/project-root (&optional strict-p)
  "Get the path to the root of your project."
  (let (projectile-require-project-root strict-p)
    (projectile-project-root)))

(defun user/buffer-path ()
  "Displays the buffer's full path relative to the project root (includes the
project root). Excludes the file basename."
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

;; vc-mode

(defun user/evil-state-segment ()
  (propertize
   " λ "
   ;; (format " %s " (upcase
   ;; 		   (substring
   ;; 		    (symbol-name (user/get-evil-state)) 0 1)))
   'face `(:foreground
	   ,(face-background 'default)
	   :background
	   ,(user/get-evil-state-highlight-face))))

(defun user/get-branch ()
  (when (and buffer-file-name (vc-find-root buffer-file-name ".git"))
    (car (vc-git-branches))))

(defun user/get-major-mode ()
  "The major mode, including process, environment and text-scale info."
  (concat (format-mode-line mode-name)
	  (if (stringp mode-line-process) mode-line-process)
	  (and (featurep 'face-remap)
	       (/= text-scale-mode-amount 0)
	       (format " (%+d)" text-scale-mode-amount))))

;; (vc-mode buffer-file-name)
;; vc-mode
;; (vc-backend buffer-file-name)
;; (vc-backend buffer-file-name)
;; (car (vc-git-branches))
;;
;; (vc-find-root buffer-file-name ".git")
;;
;; (vc-registered buffer-file-name)

;; (vc-working-revision buffer-file-name)

;; (defun user/modeline ()
;;   `(:eval
;;     (list
;;      (user/make-xpm
;;       (user/get-evil-state-highlight-face)
;;       (user/get-face-height 'default)
;;       (user/get-face-width 'default))
;;      (user/buffer-path))))
;; id

(defun user/modeline ()
  `(:eval
    (list
     (user/evil-state-segment)
     " "
     (user/project-name)
     " "
     (user/buffer-path)
     "%b"
     " "
     (user/get-branch)
     " "
     "%l:%c %p"
     " "
     (user/get-major-mode)
     )))

(use-package powerline
  :config (setq-default mode-line-format (user/modeline)))
