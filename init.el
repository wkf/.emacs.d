;;; init.el -- emacs configuration file

;;; Commentary:

;;; Code:

(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

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
 split-height-threshold 160
 split-width-threshold 160)

;;

(defvar user/leader-key ",")

(eval-when-compile
  (require 'use-package))

(use-package general)

(use-package f)
(use-package s)
(use-package dash)
(use-package diff)

(use-package base16-theme
  :after faces
  :init
  (setq base16-distinct-fringe-background nil)
  :config
  (load-theme 'base16-material-darker t))

(use-package user-ui
  :straight nil
  :load-path "user"
  :after (base16-theme diff)
  :init
  (mac-auto-operator-composition-mode)
  :config
  (let ((frame (selected-frame)))
    (set-frame-font "Fira Code 12" nil t)
    (set-frame-parameter frame 'ns-appearance 'dark)
    (set-frame-parameter frame 'ns-transparent-titlebar t)
    (user-ui/reposition-frame frame 2 0)
    (user-ui/resize-frame frame 5 1))
  (set-face-attribute 'region nil
                      :background (plist-get user-ui/colors :gray5))
  (set-face-attribute 'font-lock-comment-face nil
                      :slant 'italic))

(use-package diff
  :straight nil
  :custom-face
  (diff-changed ((t (:foreground ,(plist-get user-ui/colors :yellow))))))

(use-package user-util
  :straight nil
  :load-path "user"
  :after (f ivy cider projectile))

(use-package user-mode-line
  :straight nil
  :load-path "user"
  ;; TODO check for these at runtime so we can load mode-line sooner
  :after (user-ui powerline spinner projectile flycheck)
  :init
  (setq user-mode-line/emacs-state-color (plist-get user-ui/colors :yellow)
        user-mode-line/insert-state-color (plist-get user-ui/colors :green)
        user-mode-line/motion-state-color (plist-get user-ui/colors :magenta)
        user-mode-line/normal-state-color (plist-get user-ui/colors :blue)
        user-mode-line/replace-state-color (plist-get user-ui/colors :red)
        user-mode-line/visual-state-color (plist-get user-ui/colors :orange)
        user-mode-line/special-state-color (plist-get user-ui/colors :cyan)
        user-mode-line/unknown-state-color (plist-get user-ui/colors :pink))
  :config
  (setq-default mode-line-format (user-mode-line)))

(use-package linum
  :init
  (setq linum-format " %3d ")
  :ghook
  'prog-mode-hook
  'text-mode-hook
  :custom-face
  (linum ((t (:inherit font-lock-comment-face :underline nil)))))

(use-package hl-line
  :init
  (setq hl-line-sticky-flag nil
        global-hl-line-sticky-flag nil)
  :ghook
  'prog-mode-hook
  :custom-face
  (hl-line ((t (:background ,(plist-get user-ui/colors :gray0))))))

(use-package hl-todo
  :init
  (setq hl-todo-include-modes '(prog-mode)
        hl-todo-keyword-faces
        `(("HOLD" . ,(plist-get user-ui/colors :yellow))
          ("TODO" . ,(plist-get user-ui/colors :magenta))
          ("NEXT" . ,(plist-get user-ui/colors :yellow))
          ("THEM" . ,(plist-get user-ui/colors :yellow))
          ("PROG" . ,(plist-get user-ui/colors :yellow))
          ("OKAY" . ,(plist-get user-ui/colors :green))
          ("DONT" . ,(plist-get user-ui/colors :red))
          ("FAIL" . ,(plist-get user-ui/colors :red))
          ("DONE" . ,(plist-get user-ui/colors :green))
          ("NOTE" . ,(plist-get user-ui/colors :yellow))
          ("KLUDGE" . ,(plist-get user-ui/colors :red))
          ("HACK" . ,(plist-get user-ui/colors :red))
          ("FIXME" . ,(plist-get user-ui/colors :red))
          ("XXX" . ,(plist-get user-ui/colors :red))
          ("XXXX" . ,(plist-get user-ui/colors :red))))
  :config
  (global-hl-todo-mode t))

(use-package which-key
  :config
  (which-key-mode)
  (which-key-setup-minibuffer))

(use-package undo-fu)

(use-package undo-fu-session
  :config
  (global-undo-fu-session-mode))

;; On OSX, in GUI Emacs, `exec-path' isn't populated properly (it should match
;; $PATH in my shell). `exec-path-from-shell' fixes this.
(use-package exec-path-from-shell
  :if window-system
  :config
  (exec-path-from-shell-initialize)
  (setq-default eshell-path-env (getenv "PATH")))

(use-package org
  :config
  (defface org-checkbox-todo-text
    '((t (:inherit org-todo)))
    "Face for the text part of an unchecked org-mode checkbox.")
  (defface org-checkbox-done-text
    '((t (:inherit org-done)))
    "Face for the text part of a checked org-mode checkbox.")
  (font-lock-add-keywords
   'org-mode
   `(("^[ \t]*\\(?:[-+*]\\|[0-9]+[).]\\)[ \t]+\\(\\(?:\\[@\\(?:start:\\)?[0-9]+\\][ \t]*\\)?\\[\\(?: \\|\\([0-9]+\\)/\\2\\)\\][^\n]*\n\\)" 1 'org-checkbox-todo-text prepend)
     ("^[ \t]*\\(?:[-+*]\\|[0-9]+[).]\\)[ \t]+\\(\\(?:\\[@\\(?:start:\\)?[0-9]+\\][ \t]*\\)?\\[\\(?:X\\|\\([0-9]+\\)/\\2\\)\\][^\n]*\n\\)" 1 'org-checkbox-done-text prepend))
   'append)
  :custom-face
  (org-todo ((t (:inherit default :background unspecified))))
  (org-done ((t (:inherit default :background unspecified :foreground ,(plist-get user-ui/colors :gray2))))))

(use-package evil
  :after base16-theme
  :init
  (setq evil-want-fine-undo t
        evil-want-C-u-scroll t
        evil-want-keybinding nil)
  :config
  (evil-mode 1)
  (setq evil-cross-lines t
        evil-move-beyond-eol t
        evil-emacs-state-cursor `(,(plist-get user-ui/colors :yellow) box)
        evil-insert-state-cursor `(,(plist-get user-ui/colors :green) bar)
        evil-motion-state-cursor `(,(plist-get user-ui/colors :magenta) box)
        evil-normal-state-cursor `(,(plist-get user-ui/colors :blue) box)
        evil-replace-state-cursor `(,(plist-get user-ui/colors :red) bar)
        evil-visual-state-cursor `(,(plist-get user-ui/colors :orange) box))
  (evil-set-undo-system 'undo-fu)

  ;; On OSX, stop copying each visual state move to the clipboard:
  ;; https://bitbucket.org/lyro/evil/issue/336/osx-visual-state-copies-the-region-on
  ;; Most of this code grokked from:
  ;; http://stackoverflow.com/questions/15873346/elisp-rename-macro
  (when (or (featurep 'mac) (featurep 'ns))
    (general-add-advice 'evil-visual-update-x-selection :override 'ignore))

  ;; workaround for...
  ;; https://github.com/abo-abo/swiper/issues/977
  (defvar user/-evil-ex-minibuffer nil)

  (defun user/-before-evil-ex-setup ()
    (setq user/-evil-ex-minibuffer (current-buffer)))

  (defun user/-around-evil-ex-teardown (f)
    (when (eq user/-evil-ex-minibuffer (current-buffer))
      (funcall f)
      (setq user/-evil-ex-minibuffer nil)))

  (general-add-advice 'evil-ex-setup :before 'user/-before-evil-ex-setup)
  (general-add-advice 'evil-ex-teardown :around 'user/-around-evil-ex-teardown)
  :general
  (:states '(normal visual)
   "\\" 'evil-repeat-find-char-reverse)
  (:states '(normal motion)
   "j" 'evil-next-visual-line
   "k" 'evil-previous-visual-line
   "gj" 'evil-next-line
   "gk" 'evil-previous-line)
  (:prefix user/leader-key
   :states '(normal visual)
   "RET" 'user/switch-to-last-buffer
   "fr" 'user/rename-this-buffer-and-file
   "bd" (lambda () (interactive) (kill-buffer))))

(use-package avy
  ;;  installed along with evil
  :straight nil
  :general
  (:states '(normal visual)
   :prefix "SPC"
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

(defvar user/evil-collection-packages
  '(ivy
    vterm
    cider
    eshell
    prodigy
    unimpaired))

(use-package evil-collection
  :after evil
  :config
  (dolist (p user/evil-collection-packages)
    (evil-collection-init p)))

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package evil-snipe
  :after evil
  :config
  (evil-snipe-mode +1)
  (evil-snipe-override-mode +1))

(use-package evil-commentary
  :after evil
  :config
  (evil-commentary-mode))

(use-package evil-org
  :after (org evil)
  :config (evil-org-set-key-theme '(navigation insert textobjects todo))
  :ghook 'org-mode-hook)

(use-package evil-multiedit
  :init
  (setq evil-multiedit-store-in-search-history t)
  :config
  (evil-ex-define-cmd "ie[dit]" 'evil-multiedit-ex-match)
  :general
  (:states '(normal visual)
   "gm" 'evil-multiedit-match-all
   "gM" 'evil-multiedit-restore)
  (:states 'normal
   "gn" 'evil-multiedit-match-symbol-and-next
   "gN" 'evil-multiedit-match-symbol-and-prev)
  (:states 'visual
   "gn" 'evil-multiedit-match-and-next
   "gN" 'evil-multiedit-match-and-prev)
  (:states 'insert
   "C-s" 'evil-multiedit-toggle-marker-here)
  (:states 'motion
   "RET" 'evil-multiedit-toggle-or-restrict-region)
  (:keymaps '(evil-multiedit-state-map
              evil-multiedit-insert-state-map)
   "C-n" 'evil-multiedit-next
   "C-p" 'evil-multiedit-prev)
  :custom-face
  (iedit-occurrence ((t (:foreground ,(plist-get user-ui/colors :black) :background ,(plist-get user-ui/colors :pink))))))

(use-package prodigy
  :general
  (:states 'normal
   :prefix user/leader-key
   "S" 'prodigy)
  :custom-face
  (prodigy-green-face ((t (:inherit success))))
  (prodigy-red-face ((t (:inherit error))))
  (prodigy-yellow-face ((t (:inherit warning)))))

(use-package shackle)

(use-package gitconfig-mode
  :mode
  ("/\\.gitmodules$"
   "/\\.?git/?config$")
  :gfhook
  #'flyspell-mode)

(use-package gitignore-mode
  :mode
  ("/git/ignore$"
   "/\\.gitignore$"
   "/\\.git/info/exclude$"))

(use-package git-gutter
  :after shackle
  :commands git-gutter-mode
  :init
  (setq git-gutter:window-width -1)
  :config
  (push
   '("^\\*git-gutter.+\\*$" :align below :size 15 :noselect t :regexp t) shackle-rules)
  (advice-add 'evil-force-normal-state :after 'git-gutter)
  :ghook
  'text-mode-hook
  'prog-mode-hook
  'conf-mode-hook
  ('focus-in-hook #'git-gutter:update-all-windows))

(use-package fringe-helper)

(use-package git-gutter-fringe
  :after (git-gutter fringe-helper)
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
    nil nil 'center)
  :custom-face

  (git-gutter-fr:modified ((t (:foreground ,(plist-get user-ui/colors :yellow)))))
  )

(use-package git-messenger
  :commands git-messenger:popup-message
  :init
  (defvar git-messenger-map (make-sparse-keymap))
  :config
  (setq git-messenger:show-detail t)
  (push '("*git-messenger*" :align left :size 55 :select t) shackle-rules)
  :general
  ('git-messenger-map
   "q" 'git-messenger:popup-close))

(use-package magit
  :after evil-snipe
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
  :general
  (:states '(normal visual)
   :prefix user/leader-key
   "gs" 'magit-status
   "gb" 'magit-blame
   "gq" 'magit-blame-quit
   "gc" (lambda ()
          (interactive)
          (shell-command "git log origin/production..production --no-merges --oneline --reverse | pbcopy")))
  :gfhook
  #'turn-off-evil-snipe-override-mode
  :custom-face
  (magit-bisect-bad ((t (:inherit magit-signature-error))))
  (magit-bisect-good ((t (:inherit magit-signature-good))))
  (magit-bisect-skip ((t (:inherit magit-signature-untrusted))))
  (magit-diff-added ((t (:inherit diff-added))))
  (magit-diff-added-highlight ((t (:inherit diff-added :background ,(plist-get user-ui/colors :gray1)))))
  (magit-diff-base ((t (:inherit diff-changed))))
  (magit-diff-base-highlight ((t (:inherit diff-changed :background ,(plist-get user-ui/colors :gray1)))))
  (magit-diff-lines-heading ((t (:inherit diff-file-header))))
  (magit-diff-removed ((t (:inherit diff-removed))))
  (magit-diff-removed-highlight ((t (:inherit diff-removed :background ,(plist-get user-ui/colors :gray1)))))
  (magit-diffstat-added ((t (:inherit diff-added))))
  (magit-diffstat-removed ((t (:inherit diff-removed))))
  (magit-dimmed ((t (:inherit font-lock-comment-face))))
  (magit-log-date ((t (:inherit font-lock-comment-face))))
  (magit-log-graph ((t (:inherit font-lock-comment-face))))
  (magit-refname ((t (:inherit default))))
  (magit-section-heading ((t (:inherit font-lock-type-face :weight bold)))))

(use-package evil-magit)

(use-package ivy
  :init
  (setq ivy-use-virtual-buffers t)
  :config
  (ivy-mode 1)
  :general
  ('ivy-minibuffer-map
   "<C-return>" 'ivy-dispatching-done
   "C-c RET" 'ivy-immediate-done
   "C-c C-c" 'minibuffer-keyboard-quit
   "C-c o" 'ivy-occur
   "ESC ESC ESC" 'minibuffer-keyboard-quit)
  (:states '(normal visual)
   :prefix user/leader-key
   ","  'ivy-resume
   "bb" 'ivy-switch-buffer))

(use-package swiper
  :config
  (general-add-advice
   '(ivy-next-line ivy-previous-line)
   :after (lambda (arg)
            (add-to-history 'regexp-search-ring (ivy--regex ivy-text))
            (setq isearch-forward t)))
  :general
  (:states '(normal visual)
   "/" (lambda () (interactive) (setq isearch-forward t) (swiper))))

(use-package projectile
  :init
  (setq projectile-completion-system 'ivy
        projectile-enable-caching nil
        projectile-file-exists-local-cache-expire 30
        projectile-globally-ignored-directories
        '(".idea" ".ensime_cache" ".eunit" ".git" ".hg" ".fslckout" "_FOSSIL_" ".bzr" "_darcs" ".tox" ".svn" ".stack-work" "node_modules"))
  :config
  (projectile-mode))

(use-package perspective
  :config
  (persp-mode))

(use-package persp-projectile
  :general
  (:states '(normal visual)
   :prefix user/leader-key
   "pp" 'projectile-persp-switch-project))

(use-package counsel
  :init
  (setq counsel-find-file-ignore-regexp nil)
  :general
  (:states '(normal visual)
   :prefix user/leader-key
   ":"  'counsel-M-x
   "ff" 'counsel-find-file))

(use-package counsel-projectile
  :config
  (counsel-projectile-mode)
  :general
  (:states '(normal visual)
   :prefix user/leader-key
   "SPC" 'counsel-projectile
   "pf" 'counsel-projectile-find-file
   "pd" 'counsel-projectile-find-dir
   "pb" 'counsel-projectile-switch-to-buffer
   "p/" 'counsel-projectile-ag))

(use-package company
  :config
  (global-company-mode)
  (setq company-idle-delay 0.2
        company-tooltip-align-annotations t)
  :general
  ('company-active-map
   "C-n" 'company-select-next
   "C-p" 'company-select-previous)
  :custom-face
  (company-tooltip-common ((t (:inherit company-tooltip :weight bold :underline nil))))
  (company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold :underline nil)))))

(use-package prescient)

(use-package ivy-prescient
  :after ivy
  :config
  (ivy-prescient-mode 1))

(use-package dumb-jump
  :ghook
  ('xref-backend-function #'dumb-jump-xref-activate))

(use-package term
  :straight nil
  :config
  (defun after-term-line-mode ()
    (setq evil-emacs-state-cursor `(,(plist-get user-ui/colors :yellow) box)
          evil-insert-state-cursor `(,(plist-get user-ui/colors :green) bar)
          evil-motion-state-cursor `(,(plist-get user-ui/colors :magenta) box)
          evil-normal-state-cursor `(,(plist-get user-ui/colors :blue) box)
          evil-replace-state-cursor `(,(plist-get user-ui/colors :red) bar)
          evil-visual-state-cursor `(,(plist-get user-ui/colors :orange) box))
    (evil-refresh-cursor))

  (defun after-term-char-mode ()
    (setq evil-emacs-state-cursor `(,(plist-get user-ui/colors :yellow) hbar)
          evil-insert-state-cursor `(,(plist-get user-ui/colors :green) hbar)
          evil-motion-state-cursor `(,(plist-get user-ui/colors :magenta) hbar)
          evil-normal-state-cursor `(,(plist-get user-ui/colors :blue) hbar)
          evil-replace-state-cursor `(,(plist-get user-ui/colors :red) hbar)
          evil-visual-state-cursor `(,(plist-get user-ui/colors :orange) hbar))
    (evil-refresh-cursor))

  (add-hook 'term-mode-hook (lambda ()
                              (make-local-variable 'evil-emacs-state-cursor)
                              (make-local-variable 'evil-insert-state-cursor)
                              (make-local-variable 'evil-motion-state-cursor)
                              (make-local-variable 'evil-normal-state-cursor)
                              (make-local-variable 'evil-replace-state-cursor)
                              (make-local-variable 'evil-visual-state-cursor)))

  (advice-add 'term-line-mode :after 'after-term-line-mode)
  (advice-add 'term-char-mode :after 'after-term-char-mode))

(use-package xterm-color
  :after base16-theme
  :init
  (setq xterm-color-names `[,(plist-get user-ui/colors :black)
                            ,(plist-get user-ui/colors :red)
                            ,(plist-get user-ui/colors :green)
                            ,(plist-get user-ui/colors :yellow)
                            ,(plist-get user-ui/colors :blue)
                            ,(plist-get user-ui/colors :magenta)
                            ,(plist-get user-ui/colors :cyan)
                            ,(plist-get user-ui/colors :white)]
        xterm-color-names-bright `[,(plist-get user-ui/colors :black)
                                   ,(plist-get user-ui/colors :red)
                                   ,(plist-get user-ui/colors :green)
                                   ,(plist-get user-ui/colors :yellow)
                                   ,(plist-get user-ui/colors :blue)
                                   ,(plist-get user-ui/colors :magenta)
                                   ,(plist-get user-ui/colors :cyan)
                                   ,(plist-get user-ui/colors :white)]))

(use-package eshell
  :init
  (setenv "TERM" "xterm-256color")
  (setq eshell-preoutput-filter-functions
        '(xterm-color-filter))
  (setq eshell-output-filter-functions
        '(eshell-postoutput-scroll-to-bottom eshell-handle-control-codes))
  :gfhook
  (nil (lambda ()
         (general-def '(normal insert) 'eshell-mode-map
           "C-n" 'eshell-next-input
           "C-p" 'eshell-previous-input
           "C-s" 'counsel-esh-history
           ;; http://emacs.stackexchange.com/questions/27849/how-can-i-setup-eshell-to-use-ivy-for-tab-completion
           "TAB" (lambda () (interactive) (pcomplete-std-complete))
           "<C-return>" (lambda () (interactive) (eshell/clear-scrollback) (eshell-send-input nil nil t)))))
  ('eshell-before-prompt-hook (lambda ()
                                (setq xterm-color-preserve-properties t)))
  :general
  (:states '(normal visual)
   :prefix user/leader-key
   "ss" 'user/run-eshell
   "sn" 'user/run-new-eshell)
  :custom-face
  (eshell-prompt ((t (:inherit eshell-ls-special :weight bold :foreground unspecified)))))

(use-package eshell-prompt-extras
  :after eshell
  :config
  (setq eshell-highlight-prompt t
        eshell-banner-message ""
        eshell-prompt-function 'user/eshell-prompt-theme)
  :custom-face
  (epe-symbol-face ((t (:inherit eshell-ls-special :weight bold)))))

(use-package multi-eshell
  :general
  (:states '(normal visual)
   :prefix user/leader-key
   "sb" 'multi-eshell-go-back))

(use-package vterm
  :general
  (:states '(normal visual)
   :prefix user/leader-key
   "sv" 'vterm))

(use-package shx
  :config
  (shx-global-mode 1))

(use-package olivetti
  :ghook
  'text-mode-hook)

(use-package markdown-mode
  :mode
  (("README\\.md\\'" . gfm-mode)
   ("\\.md\\'" . markdown-mode)
   ("\\.markdown\\'" . markdown-mode)))

(use-package focus
  :general
  (:states '(normal visual)
   :prefix user/leader-key
   "F" 'focus-mode))

(use-package flycheck
  :init
  (setq-default flycheck-emacs-lisp-load-path 'inherit)
  (setq flycheck-display-errors-function 'flycheck-display-error-messages)
  :config
  (setq flycheck-display-errors-delay 1.0
        flycheck-indication-mode 'left-fringe
        flycheck-check-syntax-automatically '(save mode-enabled))
  (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
    [192 192 192 192 192 192 192 192 192 192 192 192 192 192 192 192 192 192 192 192 192 192 192 192 192]
    nil nil 'center)
  :custom-face
  (flycheck-error-list-info ((t (:inherit success)))))

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

(use-package clojure-mode
  :init
  (setq clojure-indent-style :always-indent)
  :general
  ('clojure-mode-map
   :states '(normal)
   :prefix user/leader-key
   "mor" 'user/clojure-reload
   "moR" 'user/clojure-reload-all
   "moab" 'user/clojure-aviary-browse
   "moae" 'user/clojure-aviary-export))

(use-package flycheck-clojure
  :after flycheck
  :config
  (flycheck-clojure-setup))

(use-package rainbow-delimiters
  :ghook
  user/lisp-mode-hooks)

(use-package aggressive-indent
  :ghook
  user/lisp-mode-hooks)

(use-package hydra)

(use-package lispy
  :init
  (setq lispy-colon-p nil
        lispy-close-quotes-at-end-p t
        lispy-insert-space-after-wrap nil)
  :config
  (defhydra user/lispy-g-hydra (:color blue :hint nil :idle .3 :columns 3)
    ("j" lispy-knight-down "knight down")
    ("k" lispy-knight-up "knight up")
    ("g" lispy-beginning-of-defun "beginning of def")
    ("d" lispy-goto "goto")
    ("D" lispy-goto-local "goto local")
    ("J" lispy-outline-next "next outline")
    ("K" lispy-outline-prev "prev outline")
    ("L" lispy-outline-goto-child "child outline")
    ("e" lispy-eval-and-insert "eval and insert")
    ("E" lispy-eval-other-window "eval other window")
    ("x" hydra-lispy-x/body "x mode")
    ("o" lispy-other-mode "o mode")
    ("+" lispy-widen "widen")
    ("-" lispy-narrow "narrow")
    ("(" lispy-wrap-round "wrap parens")
    ("[" lispy-wrap-brackets "wrap brackets")
    ("{" lispy-wrap-braces "wrap braces"))
  ;; (defun user/lispy-wrap-no-space (f arg)
  ;;   (let ((space-p lispy-insert-space-after-wrap))
  ;;     (setq lispy-insert-space-after-wrap nil)
  ;;     (funcall f arg)
  ;;     (setq lispy-insert-space-after-wrap space-p)))

  ;; (general-add-advice 'lispy-brackets :around 'user/lispy-wrap-no-space)
  ;; (general-add-advice 'lispy-braces :around 'user/lispy-wrap-no-space)
  :ghook
  user/lisp-mode-hooks
  :general
  ('lispy-mode-map
   :definer 'lispy
   "f" '(lispy-ace-paren
         :override '(cond ((bound-and-true-p view-mode)
                           (View-quit))))
   "t" 'lispy-ace-char
   "c" 'lispy-ace-symbol-replace
   "P" 'lispy-clone
   "C" 'lispy-kill
   "%" 'lispy-different
   "^" 'lispy-splice-sexp-killing-backward
   "$" 'lispy-splice-sexp-killing-forward
   "p" 'lispy-paste
   "y" 'lispy-new-copy
   "z" 'lispy-view
   "J" 'lispy-join
   "K" 'lispy-describe
   ">" 'lispy-slurp-or-barf-right
   "<" 'lispy-slurp-or-barf-left
   "/" 'lispy-occur
   "w" 'lispy-flow
   "H" 'lispy-forward
   "L" 'lispy-backward
   "g" 'user/lispy-g-hydra/body
   "S" 'lispy-move-up
   "q" 'lispy-convolute
   "Q" 'lispy-convolute-left
   "x" 'lispy-splice
   "T" '(lispy-teleport
         :override '(cond ((looking-at lispy-outline)
                           (end-of-line)))))
  ('lispy-mode-map
   "TAB" 'lispy-tab
   "[" 'lispy-brackets
   "]" 'lispy-close-square
   "{" 'lispy-braces
   "}" 'lispy-close-curly))

(use-package lispyville
  :after lispy
  :init
  (setq lispyville-motions-put-into-special t
        lispyville-commands-put-into-special t)
  :config
  (lispyville-set-key-theme
   '(operators
     c-w
     c-u
     prettify
     text-objects
     commentary
     (atom-movement t)
     slurp/barf-cp
     mark-toggle))

  (defun user/lispy-insert-at-end-of-list ()
    "Forward list and enter insert state."
    (interactive)
    (if (not (lispyville--at-left-p))
        (call-interactively 'lispyville-insert-at-end-of-list)
      (forward-char)
      (call-interactively 'lispyville-insert-at-end-of-list)
      (backward-char)
      (if (not (lispyville--at-right-p))
          (forward-char)
        (forward-char)
        (insert " "))))

  (defun user/lispy-insert-at-beginning-of-list ()
    "Backward list and enter insert state."
    (interactive)
    (if (not (lispyville--at-left-p))
        (call-interactively 'lispyville-insert-at-beginning-of-list)
      (forward-char)
      (call-interactively 'lispyville-insert-at-beginning-of-list)
      (when (lispyville--at-left-p)
        (insert " ")
        (backward-char))))

  (defun user/lispy-space ()
    "Like lispy space, but move the cursor back once."
    (interactive)
    (if (not (lispyville--at-left-p))
        (call-interactively 'lispy-space)
      (call-interactively 'lispy-space)
      (backward-char)
      (when (lispyville--at-left-p)
        (forward-char))))
  :ghook
  'lispy-mode-hook
  :general
  ('lispy-mode-map
   :definer 'lispy
   "SPC" 'user/lispy-space
   "v" 'lispyville-toggle-mark-type
   "A" 'user/lispy-insert-at-end-of-list
   "I" 'user/lispy-insert-at-beginning-of-list)
  ('normal
   'lispyville-mode-map
   "X" 'lispy-kill
   "gS" 'lispy-split
   "gq" 'lispy-convolute
   "gQ" 'lispy-convolute-left
   "gI" 'lispyville-insert-at-beginning-of-list
   "gA" 'lispyville-insert-at-end-of-list
   "go" 'lispyville-open-below-list
   "gO" 'lispyville-open-above-list
   "gr" 'lispy-raise-sexp
   "gR" 'lispyville-raise-list
   "g<" 'lispyville-drag-forward
   "g>" 'lispyville-drag-backward
   "g(" 'lispyville-wrap-round
   "g[" 'lispyville-wrap-brackets
   "g{" 'lispyville-wrap-braces
   "(" 'lispyville-backward-up-list
   ")" 'lispyville-up-list))

(use-package eval-sexp-fu
  :after lispy
  :ghook
  ('emacs-lisp-mode-hook (lambda ()
                           (general-def 'lispy-mode-map
                             :definer 'lispy
                             "e" 'eval-sexp-fu-eval-sexp-inner-sexp)))
  :custom-face
  (eval-sexp-fu-flash ((t (:inherit success :inverse-video t))))
  (eval-sexp-fu-flash-error ((t (:inherit error :inverse-video t)))))

(use-package cider
  :init
  (setq cider-default-cljs-repl
        "(do (require 'figwheel-sidecar.repl-api) (figwheel-sidecar.repl-api/cljs-repl))"
        cider-repl-pop-to-buffer-on-connect nil))

(use-package cider-eval-sexp-fu
  :after (cider lispy)
  :ghook
  ('clojure-mode-hook (lambda ()
                        (general-def 'lispy-mode-map
                          :definer 'lispy
                          "e" 'eval-sexp-fu-cider-eval-sexp-inner-sexp))))


;;;  python

(use-package python
  :mode
  ("\\.py\\'" . python-mode)
  :interpreter
  ("python3" . python-mode)
  :commands python-mode
  :general
  ('python-mode-map
   "DEL" nil)           ; interferes with smartparens
  :gfhook
  #'eldoc-mode
  #'flycheck-mode
  #'smartparens-mode
  :preface
  (setenv "IPY_TEST_SIMPLE_PROMPT" "1")
  :init
  (setq
   python-shell-interpreter "ipython3"
   python-shell-interpreter-args "--deep-reload"
   python-indent-guess-indent-offset nil))

(use-package anaconda-mode
  :after python
  :general
  ('normal
   'anaconda-mode-map
   "gd" 'anaconda-mode-find-definitions)
  ('normal
   'anaconda-nav-mode-map
   "ESC" 'anaconda-nav-quit)
  :init
  (setq
   anaconda-mode-installation-directory (concat user-emacs-directory "anaconda/")
   anaconda-mode-eldoc-as-single-line t)
  :ghook
  'python-mode-hook
  ('python-mode-hook #'anaconda-eldoc-mode))

(use-package company-anaconda
  :after (company anaconda-mode)
  :config
  (add-to-list 'company-backends '(company-anaconda :with company-capf)))

(use-package repl-toggle
  :config
  (rtog/add-repl 'python-mode (lambda () (run-python python-shell-interpreter t t)))
  :general
  (:states '(normal visual)
   :prefix user/leader-key
   "sr" 'rtog/toggle-repl))

(use-package pip-requirements)

;;;  elixir

(use-package elixir-mode
  :gfhook
  #'smartparens-mode
  #'(lambda () (setq evil-shift-width 2))
  :custom-face
  (elixir-atom-face ((t (:inherit font-lock-constant-face))))
  (elixir-attribute-face ((t (:inherit font-lock-keyword-face)))))

(use-package alchemist
  :general
  ('(normal insert)
   'alchemist-iex-mode-map
   "C-n" #'comint-next-input
   "C-p" #'comint-previous-input))

(use-package flycheck-credo
  :after flycheck
  :config
  (flycheck-credo-setup))

;;  javascript

(defvar user/js-mode-hooks
  '(js-mode-hook
    js2-mode-hook
    rjsx-mode-hook))

(use-package js2-mode
  :after flycheck
  :mode
  ("\\.js\\'")
  :gfhook
  #'flycheck-mode
  :init
  (setq js2-strict-trailing-comma-warning nil))

(use-package rjsx-mode
  :after flycheck
  :mode
  ("\\.jsx\\'")
  :gfhook
  #'flycheck-mode)

(use-package prettier-js
  :ghook
  user/js-mode-hooks)

(use-package smartparens
  :ghook
  user/js-mode-hooks)

(use-package tide
  :init
  (setq tide-filter-out-warning-completions t)
  :config
  (flycheck-add-next-checker 'javascript-eslint 'javascript-tide 'append)
  :ghook
  (user/js-mode-hooks (lambda ()
                        (tide-setup)
                        (tide-format-before-save)
                        (tide-hl-identifier-mode 1))))

(use-package add-node-modules-path
  :ghook
  (user/js-mode-hooks #'add-node-modules-path))

(use-package indium)

(use-package sql
  :general
  (:states '(normal visual)
   :prefix user/leader-key
   "dd" 'user/sql-connect
   "dn" 'user/sql-new-connect)
  ('(normal insert)
   'sql-interactive-mode-map
   "C-n" #'comint-next-input
   "C-p" #'comint-previous-input))

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

(load user/custom-file)
(load user/private-file)

(fset 'yes-or-no-p 'y-or-n-p)
(savehist-mode 1)
(global-auto-revert-mode 1)
;; (desktop-save-mode 1)

(general-add-hook 'before-save-hook 'delete-trailing-whitespace)
(general-add-hook 'hack-local-variables-hook (lambda () (setq truncate-lines t)))
(general-add-hook 'find-file-hook 'user/check-large-file)

(defun display-startup-echo-area-message ()
  (message (emacs-init-time)))
;;

(user/projectile-switch-to-project-file user-emacs-directory "README.org")

;;; init.el ends here
