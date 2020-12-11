;;; init.el -- emacs configuration file

;;; Commentary:

;;; Code:
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(mac-auto-operator-composition-mode)

(set-frame-font "Fira Code 12" nil t)
(set-frame-parameter (selected-frame) 'ns-appearance 'dark)
(set-frame-parameter (selected-frame) 'ns-transparent-titlebar 't)
(set-frame-position (selected-frame) (round (* (/ (display-pixel-width) 9.0) 2)) 0)
(set-frame-size (selected-frame) (- (round (* (/ (display-pixel-width) 9.0) 5)) 15) (display-pixel-height) t)

;;;

(when (>= emacs-major-version 25)
  (eval-after-load 'bytecomp
    '(add-to-list 'byte-compile-not-obsolete-funcs
                  'preceding-sexp)))

;;;

(defvar bootstrap-version)

(eval-when-compile
  (defvar straight-fix-flycheck)
  (defvar straight-use-package-by-default))

(setq straight-fix-flycheck t
      straight-use-package-by-default t)

(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(declare-function straight-use-package "straight")

(straight-use-package 'use-package)

;;

(defvar user/backup-directory
  (expand-file-name (concat user-emacs-directory "backups")))

(defvar user/custom-file
  (expand-file-name "custom.el" user-emacs-directory))

(defvar user/private-file
  (expand-file-name "private.el" user-emacs-directory))

(defvar user/savehist-file
  (expand-file-name (concat user-emacs-directory ".cache/savehist")))

(eval-when-compile
  (defvar ns-use-proxy-icon)
  (defvar ns-use-srgb-colorspace)
  (defvar undo-tree-auto-save-history)
  (defvar savehist-file)
  (defvar undo-tree-history-directory-alist)
  (defvar global-auto-revert-non-file-buffers)
  (defvar auto-revert-verbose)
  (defvar longlines-show-hard-newlines)
  (defvar package--init-file-ensured)
  (defvar ns-use-native-fullscreen)
  (defvar ns-pop-up-frames))

(setq-default
 indent-tabs-mode nil
 tab-width 2
 fill-column 80
 sentence-end-double-space nil)

(setq
 package-enable-at-startup nil
 package--init-file-ensured t
 frame-title-format nil
 inhibit-startup-message t
 ring-bell-function 'ignore
 undo-tree-auto-save-history nil
 savehist-file user/savehist-file
 custom-file user/custom-file
 backup-directory-alist `(("." . , user/backup-directory))
 auto-save-file-name-transforms `((".*" ,user/backup-directory t))
 undo-tree-history-directory-alist `(("." . , user/backup-directory))
 version-control t
 kept-new-versions 10
 kept-old-versions 0
 delete-old-versions t
 backup-by-copying t
 vc-make-backup-files t
 global-auto-revert-non-file-buffers t
 auto-revert-verbose nil
 longlines-show-hard-newlines t
 delete-by-moving-to-trash t
 save-interprogram-paste-before-kill t
 create-lockfiles nil
 initial-major-mode 'org-mode
 initial-scratch-message nil
 ns-use-srgb-colorspace t
 ns-use-proxy-icon nil
 eval-expression-print-length nil
 eval-expression-print-level nil
 scroll-step 1
 frame-resize-pixelwise t
 window-resize-pixelwise t
 window-combination-resize t
 mac-command-modifier 'meta
 mac-option-modifier 'alt
 mouse-wheel-scroll-amount '(5 ((shift) . 2))
 mouse-wheel-progressive-speed nil
 ns-use-native-fullscreen t
 ns-pop-up-frames nil
 eldoc-idle-delay 0.5
 split-height-threshold 100
 split-width-threshold 160)

(load user/custom-file)
(load user/private-file)

;;

(add-to-list 'load-path (expand-file-name "user" user-emacs-directory))

(defvar user/leader-key ",")

(eval-when-compile
  (require 'use-package))

(use-package general)

(use-package f)

(use-package s)

(use-package dash)

(use-package base16-theme
  :config
  (load-theme 'base16-material-darker t))

(use-package user-ui
  :straight nil
  :after (base16-theme))

(use-package user-util
  :straight nil
  :after (f ivy cider projectile))

(use-package user-mode-line
  :straight nil
  :after (powerline spinner projectile flycheck lispyville)
  :config (setq-default mode-line-format (user/mode-line)))

(use-package linum
  :ghook 'prog-mode-hook 'text-mode-hook
  :init (setq linum-format " %3d "))

(use-package hl-line
  :ghook 'prog-mode-hook
  :init (setq hl-line-sticky-flag nil
              global-hl-line-sticky-flag nil))

(use-package hl-todo
  :init (setq hl-todo-activate-in-modes '(prog-mode))
  :config (global-hl-todo-mode))

(use-package which-key
  :config
  (which-key-mode)
  (which-key-setup-minibuffer))

(use-package undo-fu)

(use-package undo-fu-session
  :config (global-undo-fu-session-mode))

;; On OSX, in GUI Emacs, `exec-path' isn't populated properly (it should match
;; $PATH in my shell). `exec-path-from-shell' fixes this.
(use-package exec-path-from-shell
  :config (when window-system
            (exec-path-from-shell-initialize)
            (setq-default eshell-path-env (getenv "PATH"))))

(use-package org)

(use-package evil
  :general
  (:states '(normal visual)
   "/" (lambda () (interactive) (setq isearch-forward t) (swiper))
   "\\" 'evil-repeat-find-char-reverse)
  (:states '(normal motion)
   "j" 'evil-next-visual-line
   "k" 'evil-previous-visual-line
   "gj" 'evil-next-line
   "gk" 'evil-previous-line)
  (:prefix user/leader-key
   :states '(normal visual)
   "bd" (lambda () (interactive) (kill-buffer))
   "fr" 'user/rename-this-buffer-and-file)

  :init (setq evil-want-fine-undo t
              evil-want-C-u-scroll t
              evil-want-keybinding nil)
  :config
  (evil-mode 1)
  (setq evil-cross-lines t
        evil-move-beyond-eol t
        evil-emacs-state-cursor `(,user/yellow box)
        evil-insert-state-cursor `(,user/green bar)
        evil-motion-state-cursor `(,user/purple box)
        evil-normal-state-cursor `(,user/blue box)
        evil-replace-state-cursor `(,user/red bar)
        evil-visual-state-cursor `(,user/orange box)
        general-default-keymaps 'evil-normal-state-map)
  (evil-set-undo-system 'undo-fu)
  ;; workaround for...
  ;; https://github.com/abo-abo/swiper/issues/977
  (defvar user/-evil-ex-minibuffer nil)

  (defun user/-before-evil-ex-setup ()
    (setq user/-evil-ex-minibuffer (current-buffer)))

  (defun user/-around-evil-ex-teardown (f)
    (when (eq user/-evil-ex-minibuffer (current-buffer))
      (funcall f)
      (setq user/-evil-ex-minibuffer nil)))

  (advice-add 'evil-ex-setup :before 'user/-before-evil-ex-setup)
  (advice-add 'evil-ex-teardown :around 'user/-around-evil-ex-teardown))

(use-package avy
  :straight nil
  :general
  (:prefix "SPC"
   :states '(normal visual)
   "SPC" 'evil-avy-goto-char
   "RET" 'evil-avy-goto-line
   "k" 'evil-avy-goto-line-above
   "j" 'evil-avy-goto-line-below
   "S" 'evil-avy-goto-char-2-above
   "s" 'evil-avy-goto-char-2-below
   "B" 'evil-avy-goto-word-1-above
   "W" 'evil-avy-goto-word-1-below
   "A" 'evil-avy-goto-symbol-1-above
   "a" 'evil-avy-goto-symbol-1-below)
  :custom-face
  (avy-lead-face ((t (:inherit match :foreground unspecified :background unspecified))))
  (avy-lead-face-0 ((t (:inherit match :foreground unspecified :background unspecified))))
  (avy-lead-face-1 ((t (:inherit match :foreground unspecified :background unspecified))))
  (avy-lead-face-2 ((t (:inherit match :foreground unspecified :background unspecified))))
  (avy-background-face ((t (:inherit match :foreground unspecified)))))

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

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init 'vterm)
  (evil-collection-init 'cider)
  ;; FIXME: this is gonna take some work to actually use
  ;; (evil-collection-init 'lispy)
  (evil-collection-init 'eshell)
  (evil-collection-init 'prodigy)
  (evil-collection-init 'unimpaired))

(use-package evil-surround
  :after evil
  :config (global-evil-surround-mode 1))

(use-package evil-snipe
  :after evil
  :config
  (evil-snipe-mode +1)
  (evil-snipe-override-mode +1))

(use-package evil-commentary
  :after evil
  :config (evil-commentary-mode))

(use-package evil-org
  :ghook 'org-mode-hook
  :after (org evil)
  :config (evil-org-set-key-theme '(navigation insert textobjects todo)))

(use-package prodigy
  :general (:prefix user/leader-key :states 'normal "S" 'prodigy))

(use-package shackle)

(use-package gitconfig-mode
  :ghook ('gitconfig-mode-hook #'flyspell-mode)
  :mode ("/\\.?git/?config$" "/\\.gitmodules$"))

(use-package gitignore-mode
  :mode ("/\\.gitignore$" "/\\.git/info/exclude$" "/git/ignore$"))

(use-package git-gutter
  :ghook
  'text-mode-hook
  'prog-mode-hook
  'conf-mode-hook
  ('focus-in-hook #'git-gutter:update-all-windows)
  :commands git-gutter-mode
  :config
  (push
   '("^\\*git-gutter.+\\*$" :align below :size 15 :noselect t :regexp t) shackle-rules)
  (advice-add 'evil-force-normal-state :after 'git-gutter))

(use-package fringe-helper)

(use-package git-gutter-fringe
  :after git-gutter
  :config
  (setq git-gutter-fr:side 'right-fringe)
  (define-fringe-bitmap 'git-gutter-fr:added
    [3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3]
    nil nil 'center)
  (define-fringe-bitmap 'git-gutter-fr:modified
    [3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3]
    nil nil 'center)
  (define-fringe-bitmap 'git-gutter-fr:deleted
    [3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3]
    nil nil 'center))

(use-package git-messenger
  :general (:keymaps 'git-messenger-map "q" 'git-messenger:popup-close)
  :commands git-messenger:popup-message
  :init (defvar git-messenger-map (make-sparse-keymap))
  :config
  (setq git-messenger:show-detail t)
  (push '("*git-messenger*" :align left :size 55 :select t) shackle-rules))

(use-package magit
  :custom-face
  (magit-bisect-bad ((t (:inherit magit-signature-error))))
  (magit-bisect-good ((t (:inherit magit-signature-good))))
  (magit-bisect-skip ((t (:inherit magit-signature-untrusted))))
  (magit-diff-added ((t (:inherit diff-added))))
  (magit-diff-added-highlight ((t (:inherit diff-added :background "gray17"))))
  (magit-diff-base ((t (:inherit diff-changed))))
  (magit-diff-base-highlight ((t (:inherit diff-changed :background "gray17"))))
  (magit-diff-lines-heading ((t (:inherit diff-file-header))))
  (magit-diff-removed ((t (:inherit diff-removed))))
  (magit-diff-removed-highlight ((t (:inherit diff-removed :background "gray17"))))
  (magit-diffstat-added ((t (:inherit diff-added))))
  (magit-diffstat-removed ((t (:inherit diff-removed))))
  (magit-dimmed ((t (:inherit font-lock-comment-face))))
  (magit-log-date ((t (:inherit font-lock-comment-face))))
  (magit-log-graph ((t (:inherit font-lock-comment-face))))
  (magit-refname ((t (:inherit default))))
  (magit-section-heading ((t (:inherit font-lock-type-face :weight bold))))
  ;; (magit-section-heading-selection ((t (:foreground ,user/bright-cyan))))
  ;; (magit-sequence-drop ((t (:foreground ,user/red))))
  ;; (magit-sequence-head ((t (:foreground ,user/blue))))
  ;; (magit-sequence-part ((t (:foreground ,user/yellow))))
  ;; (magit-sequence-stop ((t (:foreground ,user/green))))

  :commands magit-status
  :config
  (setq
   magit-completing-read-function 'ivy-completing-read
   magit-display-buffer-function
   (lambda (buffer)
     (display-buffer
      buffer (if (and (derived-mode-p 'magit-mode)
                      (memq (with-current-buffer buffer major-mode)
                            '(magit-process-mode
                              magit-revision-mode
                              magit-diff-mode
                              magit-stash-mode
                              magit-status-mode)))
                 nil
               '(display-buffer-same-window)))))
  (general-define-key
   :prefix user/leader-key
   :keymaps '(evil-normal-state-map
              evil-visual-state-map)
   "gs" 'magit-status
   "gb" 'magit-blame
   "gq" 'magit-blame-quit
   "gc" (lambda ()
          (interactive)
          (shell-command "git log origin/production..production --no-merges --oneline --reverse | pbcopy")))
  (general-add-hook 'magit-mode-hook 'turn-off-evil-snipe-override-mode))

(use-package evil-magit)

(use-package ivy
  :init
  (setq ivy-use-virtual-buffers t)
  :general
  (:keymaps 'ivy-minibuffer-map
            "<C-return>" 'ivy-immediate-done
            "C-c C-c" 'minibuffer-keyboard-quit
            "ESC ESC ESC" 'minibuffer-keyboard-quit)
  (:prefix
   user/leader-key
   ","  'ivy-resume
   "bb" 'ivy-switch-buffer)
  :config
  (ivy-mode 1))

(use-package swiper
  ;; :general
  ;; (:states '(normal visual)
  ;;  "/" (lambda () (interactive) (setq isearch-forward t) (swiper)))
  :config
  (general-add-advice
   '(ivy-next-line ivy-previous-line)
   :after (lambda (arg)
            (add-to-history 'regexp-search-ring (ivy--regex ivy-text))
            (setq isearch-forward t))))

;; (defun user/swiper-update-search-ring-forward (&rest args)
;;   "Update swiper results so n/N bindings work, ignoring ARGS."
;;   (add-to-history 'regexp-search-ring (ivy--regex ivy-text))
;;   (setq isearch-forward t))

;; (defun user/swiper-update-search-ring-backward (&rest args)
;;   "Update swiper results so n/N bindings work, ignoring ARGS."
;;   (add-to-history 'regexp-search-ring (ivy--regex ivy-text))
;;   (setq isearch-forward nil))

;; (advice-add 'ivy-next-line :after #'user/swiper-update-search-ring-forward)
;; (advice-add 'ivy-previous-line :after #'user/swiper-update-search-ring-backward)

;; (defun user/swiper-update-search-ring (&rest args)
;;   "Update swiper results so n/N bindings work, ignoring ARGS."
;;   (add-to-history 'regexp-search-ring (ivy--regex ivy-text))
;;   (setq isearch-forward t))


(use-package projectile
  :config (projectile-mode))

(use-package perspective
  :config (persp-mode))

(use-package persp-projectile
  :general (:prefix user/leader-key "pp" 'projectile-persp-switch-project))

(use-package counsel
  :general (:prefix user/leader-key
                    ":"  'counsel-M-x
                    "ff" 'counsel-find-file))

(use-package counsel-projectile
  :general (:prefix user/leader-key
                    "SPC" 'counsel-projectile
                    "pf" 'counsel-projectile-find-file
                    "pd" 'counsel-projectile-find-dir
                    "pb" 'counsel-projectile-switch-to-buffer
                    "p/" 'counsel-projectile-ag)
  :config (counsel-projectile-mode))

(use-package company
  :custom-face
  (company-tooltip-common ((t (:inherit company-tooltip :weight bold :underline nil))))
  (company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold :underline nil))))

  :config
  (global-company-mode)
  (setq company-idle-delay 0.2
        company-tooltip-align-annotations t)
  (general-define-key
   :keymaps 'company-active-map
   "C-n" 'company-select-next
   "C-p" 'company-select-previous))

(use-package prescient)

(use-package ivy-prescient
  :after ivy
  :config
  (ivy-prescient-mode 1))

(use-package dumb-jump
  :config
  (general-add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(use-package xterm-color
  :init
  (setq xterm-color-names `[,user/black
                            ,user/red
                            ,user/green
                            ,user/yellow
                            ,user/blue
                            ,user/magenta
                            ,user/cyan
                            ,user/white]
        xterm-color-names-bright `[,user/bright-black
                                   ,user/red
                                   ,user/green
                                   ,user/yellow
                                   ,user/blue
                                   ,user/magenta
                                   ,user/cyan
                                   ,user/bright-white]))

(use-package eshell
  :general (:prefix user/leader-key
                    "ss" 'user/run-eshell
                    "sn" 'user/run-new-eshell)
  :config
  ;; http://emacs.stackexchange.com/questions/27849/how-can-i-setup-eshell-to-use-ivy-for-tab-completion
  (general-add-hook 'eshell-mode-hook
                    (lambda ()
                      (setenv "TERM" "xterm-256color")
                      (define-key eshell-mode-map (kbd "<tab>")
                        (lambda () (interactive) (pcomplete-std-complete)))))
  (general-add-hook 'eshell-before-prompt-hook
                    (lambda ()
                      (setq xterm-color-preserve-properties t)))
  (general-add-hook
   'eshell-mode-hook
   (lambda ()
     (genera-define-key
      :keymaps 'eshell-mode-map
      :states 'normal
      ",sc" (lambda () (interactive) (eshell/clear) (eshell-send-input)))
     (if nil (general-define-key
              :keymaps 'eshell-mode-map
              :states '(normal insert)
              "RET" 'eshell-send-input
              "C-n" 'eshell-next-input
              "C-p" 'eshell-previous-input
              "C-s" 'counsel-esh-history))))
  (add-to-list 'eshell-preoutput-filter-functions 'xterm-color-filter)
  (setq eshell-output-filter-functions
        (remove 'eshell-handle-ansi-color eshell-output-filter-functions)))

(use-package eshell-prompt-extras
  :config (setq eshell-highlight-prompt t
                eshell-banner-message ""
                eshell-prompt-function 'user/eshell-prompt-theme))

(use-package multi-eshell
  :general (:prefix user/leader-key "sb" 'multi-eshell-go-back))

(use-package vterm
  :general (:prefix user/leader-key "sv" 'vterm))

(use-package shx
  :config (shx-global-mode 1))

(use-package olivetti
  :config
  (general-add-hook 'text-mode-hook 'olivetti-mode))

(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)))

(use-package focus
  :general (:prefix user/leader-key "F" 'focus-mode))

(use-package flycheck
  :custom-face
  (flycheck-error-list-info ((t (:inherit success))))
  :init
  (setq-default flycheck-emacs-lisp-load-path 'inherit)
  :config
  (setq flycheck-display-errors-delay 1.0
        flycheck-indication-mode 'left-fringe
        flycheck-check-syntax-automatically '(save mode-enabled))
  (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
    [192 192 192 192 192 192 192 192 192 192 192 192 192 192 192 192 192 192 192 192 192 192 192 192 192]
    nil nil 'center))

(use-package flycheck-tip)

;;;  lisp

(defvar user/lisp-mode-hooks
  '(emacs-lisp-mode-hook
    clojure-mode-hook
    clojurec-mode-hook
    clojurescript-mode-hook))

(general-add-hook user/lisp-mode-hooks #'eldoc-mode)
(general-add-hook user/lisp-mode-hooks #'flycheck-mode)
(general-add-hook 'emacs-lisp-mode-hook (lambda () (setq-local lisp-indent-function #'user/lisp-indent-function)))

(general-define-key
 :keymaps 'clojure-mode-map
 :states '(normal)
 :prefix user/leader-key
 "mor" 'user/clojure-reload
 "moR" 'user/clojure-reload-all
 "moab" 'user/clojure-aviary-browse
 "moae" 'user/clojure-aviary-export)

(use-package eval-sexp-fu)

(use-package flycheck-clojure
  :after flycheck
  :config (flycheck-clojure-setup))

(use-package rainbow-delimiters
  :config (general-add-hook user/lisp-mode-hooks #'rainbow-delimiters-mode))

(use-package aggressive-indent
  :config (general-add-hook user/lisp-mode-hooks #'aggressive-indent-mode))

(use-package cider
  :init
  (setq cider-default-cljs-repl
        "(do (require 'figwheel-sidecar.repl-api) (figwheel-sidecar.repl-api/cljs-repl))"
        cider-repl-pop-to-buffer-on-connect nil))

(use-package cider-eval-sexp-fu)

(defun user/lispy-space ()
  "Like lispy space, but move the cursor back once."
  (interactive)
  (if (not (lispyville--at-left-p))
      (call-interactively 'lispy-space)
    (call-interactively 'lispy-space)
    (backward-char)))

(use-package lispy
  :init
  (setq lispy-close-quotes-at-end-p t)
  (general-add-hook user/lisp-mode-hooks (lambda () (lispy-mode 1)))
  :config
  (lispy-define-key lispy-mode-map "SPC" #'user/lispy-space)
  (lispy-define-key lispy-mode-map "X" #'lispy-kill)
  (lispy-define-key lispy-mode-map "m" #'lispy-view)
  (general-add-hook
   'emacs-lisp-mode-hook
   (lambda ()
     (lispy-define-key lispy-mode-map "e" 'eval-sexp-fu-eval-sexp-inner-sexp)))
  (general-add-hook
   'clojure-mode-hook
   (lambda ()
     (lispy-define-key lispy-mode-map "e" 'eval-sexp-fu-cider-eval-sexp-inner-sexp))))

(use-package lispyville
  :after lispy
  :init
  (general-add-hook user/lisp-mode-hooks #'lispyville-mode)
  :config
  (progn
    (setq lispyville-motions-put-into-special t
          lispyville-commands-put-into-special t)
    (lispyville-set-key-theme '(operators c-w c-u prettify text-objects commentary (atom-movement t) slurp/barf-lispy mark-toggle insert))
    (lispy-define-key lispy-mode-map "v" #'lispyville-toggle-mark-type)
    (general-define-key
     :keymaps 'lispyville-mode-map
     :states 'normal
     "X" 'lispy-kill
     "gI" 'lispyville-insert-at-beginning-of-list
     "gA" 'lispyville-insert-at-end-of-list
     "go" 'lispyville-open-below-list
     "gO" 'lispyville-open-above-list
     "g<" 'lispyville-drag-forward
     "g>" 'lispyville-drag-backward
     "g(" 'lispyville-wrap-round
     "g[" 'lispyville-wrap-brackets
     "g{" 'lispyville-wrap-braces
     "(" 'lispyville-backward-up-list
     ")" 'lispyville-up-list)))

;;;  python

(setenv "IPY_TEST_SIMPLE_PROMPT" "1")

(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python3" . python-mode)
  :commands python-mode
  :init (progn
          (setq
           python-shell-interpreter "ipython3"
           python-shell-interpreter-args "--deep-reload")
          (add-hook 'python-mode-hook 'flycheck-mode)
          (add-hook 'python-mode-hook 'smartparens-mode))
  :config (define-key python-mode-map (kbd "DEL") nil)) ; interferes with smartparens

(use-package anaconda-mode
  :general
  (:keymaps 'anaconda-mode-map
            :states '(normal)
            "gd" 'anaconda-mode-find-definitions)
  (:keymaps 'anaconda-nav-mode-map
            :states '(normal)
            "ESC" 'anaconda-nav-quit)
  :after python
  :init (progn
          (add-hook 'python-mode-hook 'anaconda-mode)
          (add-hook 'python-mode-hook 'anaconda-eldoc-mode)
          (add-hook 'python-mode-hook 'eldoc-mode)
          (setq
           anaconda-mode-installation-directory (concat user-emacs-directory "anaconda/")
           anaconda-mode-eldoc-as-single-line t)))

(use-package company-anaconda
  :after (company anaconda-mode)
  :config (add-to-list 'company-backends '(company-anaconda :with company-capf)))

(use-package repl-toggle
  :general (:prefix user/leader-key "sr" 'rtog/toggle-repl)
  :config (rtog/add-repl 'python-mode (lambda () (run-python python-shell-interpreter t t))))

(use-package nose)
(use-package pip-requirements)

;;;  elixir

(use-package elixir-mode
  :custom-face
  (elixir-atom-face ((t (:inherit font-lock-constant-face))))
  (elixir-attribute-face ((t (:inherit font-lock-keyword-face))))

  :init
  (general-add-hook 'elixir-mode-hook 'smartparens-mode)
  (general-add-hook 'elixir-mode-hook
                    #'(lambda () (setq evil-shift-width 2))))

(use-package alchemist
  :general (:keymaps 'alchemist-iex-mode-map
            :states '(normal insert)
            "C-n" #'comint-next-input
            "C-p" #'comint-previous-input))

(use-package flycheck-credo
  :after flycheck
  :config (flycheck-credo-setup))

;;  javascript

(defvar user/js-mode-hooks
  '(js-mode-hook
    js2-mode-hook
    rjsx-mode-hook))

(use-package js2-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode)))

(use-package rjsx-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.jsx\\'" . rjsx-mode)))

(use-package prettier-js
  :config
  (general-add-hook user/js-mode-hooks 'prettier-js-mode))

(use-package smartparens
  :config
  (general-add-hook user/js-mode-hooks 'smartparens-mode))

(use-package tide
  :after (flycheck)
  :config
  (flycheck-add-next-checker 'javascript-eslint 'javascript-tide 'append)
  (general-add-hook user/js-mode-hooks
                    (lambda ()
                      (tide-setup)
                      (tide-format-before-save)
                      (tide-hl-identifier-mode 1)
                      (flycheck-mode 1))))

(use-package add-node-modules-path
  :config
  (general-add-hook user/js-mode-hooks #'add-node-modules-path))

(use-package indium)

;;;  others

(general-define-key
 :prefix user/leader-key
 "dd" 'user/sql-connect
 "dn" 'user/sql-new-connect)

(general-define-key
 :keymaps 'sql-interactive-mode-map
 :states '(normal insert)
 "C-n" #'comint-next-input
 "C-p" #'comint-previous-input)

(use-package lua-mode)
(use-package typescript-mode)

(use-package powerline)
(use-package spinner)
(use-package indicators)
(use-package sublimity)
(use-package yascroll)

(use-package dockerfile-mode)
(use-package wgrep)
(use-package highlight2clipboard)

(fset 'yes-or-no-p 'y-or-n-p)
(savehist-mode 1)
(global-auto-revert-mode 1)
;; (desktop-save-mode 1)

(general-add-hook 'before-save-hook 'delete-trailing-whitespace)
(general-add-hook 'hack-local-variables-hook (lambda () (setq truncate-lines t)))
(general-add-hook 'find-file-hook 'user/check-large-file)

;; TODO this can be moved inside use-package evil
(with-eval-after-load "evil"
  ;; On OSX, stop copying each visual state move to the clipboard:
  ;; https://bitbucket.org/lyro/evil/issue/336/osx-visual-state-copies-the-region-on
  ;; Most of this code grokked from:
  ;; http://stackoverflow.com/questions/15873346/elisp-rename-macro
  (when (or (featurep 'mac) (featurep 'ns))
    (advice-add 'evil-visual-update-x-selection :override 'ignore)))

;;

(user/projectile-switch-to-project-file user-emacs-directory "README.org")

;;; init.el ends here
