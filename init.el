;;; init.el -- emacs configuration file

;;; Commentary:

;;; Code:

(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

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

(eval-when-compile
  (require 'use-package))

;;

(defvar user/backup-directory
  (expand-file-name "backups" user-emacs-directory))

(defvar user/custom-file
  (expand-file-name "custom.el" user-emacs-directory))

(defvar user/private-file
  (expand-file-name "private.el" user-emacs-directory))

(defvar user/savehist-file
  (expand-file-name ".cache/savehist" user-emacs-directory))

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
 sentence-end-double-space nil
 cursor-in-non-selected-windows nil)

(setq
 kill-buffer-query-functions nil
 highlight-nonselected-windows nil
 enable-recursive-minibuffers t
 minibuffer-message-timeout 1
 mode-line-format nil
 package-enable-at-startup nil
 package--init-file-ensured t
 frame-title-format nil
 inhibit-startup-message t
 ring-bell-function 'ignore
 undo-tree-auto-save-history nil
 savehist-file user/savehist-file
 custom-file user/custom-file
 backup-directory-alist `(("." . ,user/backup-directory))
 auto-save-file-name-transforms `((".*" ,user/backup-directory t))
 undo-tree-history-directory-alist `(("." . ,user/backup-directory))
 version-control t
 kept-new-versions 10
 kept-old-versions 0
 delete-old-versions t
 backup-by-copying t
 vc-make-backup-files t
 vc-follow-symlinks t
 global-auto-revert-non-file-buffers t
 auto-revert-verbose nil
 longlines-show-hard-newlines t
 delete-by-moving-to-trash t
 save-interprogram-paste-before-kill t
 create-lockfiles nil
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
 split-width-threshold 160
 gc-cons-threshold (* 10 1024 1024)
 auth-sources '("~/.authinfo"))

(load user/custom-file)
(load user/private-file)

;;

(when (>= emacs-major-version 25)
  (eval-after-load 'bytecomp
    '(add-to-list 'byte-compile-not-obsolete-funcs
                  'preceding-sexp)))

;;;

(use-package general)

(use-package emacs
  :general
  ("C-l" (defun user/flip-to-last-buffer ()
           (interactive)
           (switch-to-buffer (other-buffer (current-buffer) t) nil 'force-same-window))
   "C-c b p" 'previous-buffer
   "C-c b n" 'next-buffer
   "C-c b d" (defun user/kill-current-buffer  ()
               (interactive)
               (kill-buffer))
   "C-c C" 'calc))

(use-package gcmh
  :config
  (gcmh-mode 1))

(use-package el-patch)

(use-package f)
(use-package s)
(use-package dash)
(use-package powerline)
(use-package spinner)

(use-package base16-theme
  :after faces
  :init
  (setq base16-distinct-fringe-background nil)
  :config
  (load-theme 'base16-material-darker t))

(use-package user-ui
  :straight nil
  :load-path "user"
  :after base16-theme
  :init
  (mac-auto-operator-composition-mode)
  :config
  (let ((frame (selected-frame)))
    (set-frame-font "Fira Code 12" nil t)
    (set-frame-parameter frame 'ns-appearance 'dark)
    (set-frame-parameter frame 'ns-transparent-titlebar t)
    (user-ui/reposition-frame frame 2 0)
    (user-ui/resize-frame frame 5 1))
  (set-face-attribute 'fringe nil
                      :foreground (plist-get user-ui/colors :gray5))
  (set-face-attribute 'region nil
                      :background (plist-get user-ui/colors :gray5))
  (set-face-attribute 'font-lock-comment-face nil
                      :slant 'italic)
  (set-face-attribute 'font-lock-comment-face nil
                      :slant 'italic)
  (set-face-attribute 'lazy-highlight nil
                      :weight 'bold)
  (set-face-attribute 'isearch nil
                      :weight 'bold)
  (set-face-attribute 'isearch-fail nil
                      :weight 'bold)
  (set-face-attribute 'match nil
                      :weight 'bold))

(use-package all-the-icons)

(use-package all-the-icons-dired
  :ghook
  'dired-mode-hook
  :custom-face
  (all-the-icons-dired-dir-face ((t (:foreground ,(plist-get user-ui/colors :blue))))))

(use-package prism
  :straight (prism :type git :host github :repo "alphapapa/prism.el")
  :config

  (prism-set-colors
    :num 12
    ;; :shuffle t

    :lightens
    ;; '(5)
    ;; '(0 15 30)
    ;; '(0 30 60 90)
    ;; '(0 0 0 30 30 30 60 60 60 90 90 90)
    (-map (lambda (i) (* i 5)) (number-sequence 0 15))

    :desaturations
    ;; '(0 15 30)
    ;; '(0 30 60 90)
    ;; '(0 0 0 30 30 30 60 60 60 90 90 90)
    (-map (lambda (i) (* i 5)) (number-sequence 0 15))

    :comments-fn
    (lambda (color)
      (prism-blend color (face-attribute 'font-lock-comment-face :foreground) 0.05))

    :strings-fn
    (lambda (color)
      (prism-blend color (plist-get user-ui/colors :green) 0.10))

    :colors (-map (lambda (n)
                    (plist-get user-ui/colors n))
                  ;; '(:magenta :blue :cyan :yellow :red :orange)
                  '(:magenta :blue :cyan))))

(use-package rainbow-delimiters
  :ghook
  'prog-mode-hook)

(use-package highlight-function-calls
  :ghook
  'prog-mode-hook
  :custom-face
  (highlight-function-calls-face ((t (:underline nil :bold t)))))

(use-package highlight-quoted
  :custom-face
  (highlight-quoted-symbol  ((t (:inherit font-lock-constant-face)))))

(use-package highlight-numbers
  :ghook
  'prog-mode-hook)

(use-package highlight-escape-sequences
  :config
  (hes-mode))

(use-package user-util
  :straight nil
  :load-path "user"
  :after f)

(use-package diff
  :straight nil
  :custom-face
  (diff-changed ((t (:foreground ,(plist-get user-ui/colors :yellow))))))

(use-package user-mode-line
  :straight nil
  :load-path "user"
  :after (user-ui powerline spinner projectile flycheck)
  :init
  (setq user-mode-line/emacs-state-color (plist-get user-ui/colors :yellow)
        user-mode-line/insert-state-color (plist-get user-ui/colors :green)
        user-mode-line/motion-state-color (plist-get user-ui/colors :magenta)
        user-mode-line/normal-state-color (plist-get user-ui/colors :blue)
        user-mode-line/replace-state-color (plist-get user-ui/colors :pink)
        user-mode-line/visual-state-color (plist-get user-ui/colors :orange)
        user-mode-line/special-state-color (plist-get user-ui/colors :cyan)
        user-mode-line/unknown-state-color (plist-get user-ui/colors :red))
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
  :config
  (global-hl-line-mode)
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
  :after evil-collection
  :config
  (which-key-mode)
  (which-key-setup-minibuffer)
  (evil-collection-init 'which-key))

(use-package undo-fu)

(use-package undo-fu-session
  :config
  (global-undo-fu-session-mode))

(use-package exec-path-from-shell
  :if window-system
  :config
  (exec-path-from-shell-initialize))

(use-package org
  :after smartparens-config
  :init
  (setq org-edit-src-content-indentation 0
        org-src-preserve-indentation t
        org-hide-leading-stars t
        org-startup-indented t
        org-capture-templates '(("T" "Global Task" checkitem
                                 (file+headline "~/.emacs.d/README.org" "Tasks")
                                 "- [ ] %?\n")))
  :config
  (setq initial-major-mode 'org-mode
        initial-scratch-message nil)

  (defface org-checkbox-todo-text
    '((t (:inherit org-todo)))
    "Face for the text part of an unchecked org-mode checkbox.")

  (defface org-checkbox-done-text
    '((t (:inherit org-done)))
    "Face for the text part of a checked org-mode checkbox.")

  (defface org-checkbox-some-text
    '((t (:inherit org-done)))
    "Face for the text part of a checked org-mode checkbox.")

  (defface org-checkbox-todo-bullet
    '((t (:inherit org-done)))
    "Face for the text part of a checked org-mode checkbox.")

  (font-lock-add-keywords
   'org-mode
   `(("^[ \t]*\\(?:[-+*]\\|[0-9]+[).]\\)[ \t]+\\(\\(?:\\[@\\(?:start:\\)?[0-9]+\\][ \t]*\\)?\\[\\(?:[ -X]\\|\\([0-9]+\\)/\\2\\)\\][^\n]*\n\\)"
      0
      'org-checkbox-todo-bullet
      prepend)
     ("^[ \t]*\\(?:[-+*]\\|[0-9]+[).]\\)[ \t]+\\(\\(?:\\[@\\(?:start:\\)?[0-9]+\\][ \t]*\\)?\\[\\(?: \\|\\([0-9]+\\)/\\2\\)\\][^\n]*\n\\)"
      1
      'org-checkbox-todo-text
      prepend)
     ("^[ \t]*\\(?:[-+*]\\|[0-9]+[).]\\)[ \t]+\\(\\(?:\\[@\\(?:start:\\)?[0-9]+\\][ \t]*\\)?\\[\\(?:-\\|\\([0-9]+\\)/\\2\\)\\][^\n]*\n\\)"
      1
      'org-checkbox-some-text
      prepend)
     ("^[ \t]*\\(?:[-+*]\\|[0-9]+[).]\\)[ \t]+\\(\\(?:\\[@\\(?:start:\\)?[0-9]+\\][ \t]*\\)?\\[\\(?:X\\|\\([0-9]+\\)/\\2\\)\\][^\n]*\n\\)"
      1
      'org-checkbox-done-text
      prepend))
   'append)

  (defun user/org-confirm-babel-evaluate (lang body)
    (not (member lang '("clojure" "sh" "restclient" "emacs-lisp"))))

  (setq org-confirm-babel-evaluate 'user/org-confirm-babel-evaluate)

  (sp-with-modes 'org-mode
    (sp-local-pair "\\[" "\\]")
    (sp-local-pair "$" "$")
    (sp-local-pair "'" "'" :actions '(rem))
    (sp-local-pair "=" "=" :actions '(rem))
    (sp-local-pair "\\left(" "\\right)" :trigger "\\l(" :post-handlers '(sp-latex-insert-spaces-inside-pair))
    (sp-local-pair "\\left[" "\\right]" :trigger "\\l[" :post-handlers '(sp-latex-insert-spaces-inside-pair))
    (sp-local-pair "\\left\\{" "\\right\\}" :trigger "\\l{" :post-handlers '(sp-latex-insert-spaces-inside-pair))
    (sp-local-pair "\\left|" "\\right|" :trigger "\\l|" :post-handlers '(sp-latex-insert-spaces-inside-pair)))

  (org-babel-do-load-languages 'org-babel-load-languages '((shell . t)))

  (defun user/prettify-org-symbols ()
    "Prettify org symbols."
    (push '("[ ]" . "□") prettify-symbols-alist)
    (push '("[X]" . "■" ) prettify-symbols-alist)
    (push '("[-]" . "◩" ) prettify-symbols-alist)
    (push '("#+RESULTS:" . "►") prettify-symbols-alist)
    (push '("#+results:" . "►") prettify-symbols-alist)
    (push '("#+BEGIN_SRC" . "◤") prettify-symbols-alist)
    (push '("#+END_SRC" . "◣" ) prettify-symbols-alist)
    (push '("#+BEGIN_EXAMPLE" . "◤" ) prettify-symbols-alist)
    (push '("#+END_EXAMPLE" . "◣" ) prettify-symbols-alist)
    (push '("#+BEGIN_QUOTE" . "◤" ) prettify-symbols-alist)
    (push '("#+END_QUOTE" . "◣" ) prettify-symbols-alist)
    (push '("#+begin_quote" . "◤" ) prettify-symbols-alist)
    (push '("#+end_quote" . "◣" ) prettify-symbols-alist)
    (push '("#+begin_example" . "◤" ) prettify-symbols-alist)
    (push '("#+end_example" . "◣" ) prettify-symbols-alist)
    (push '("#+begin_src" . "◤" ) prettify-symbols-alist)
    (push '("#+end_src" . "◣" ) prettify-symbols-alist)
    (prettify-symbols-mode))

  (defun user/org-capture-global-task ()
    (interactive)
    (org-capture nil "T"))

  :ghook
  ('org-capture-mode-hook 'evil-insert-state)
  :gfhook
  #'smartparens-mode
  #'user/prettify-org-symbols
  :general
  ("C-c o t" 'user/org-capture-global-task)
  ('org-mode-map
   "C-9" 'org-up-element
   "C-0" 'org-down-element
   "C-c o I" 'org-indent-mode)
  :custom-face
  (org-block ((t (:inherit default :foreground unspecified))))
  (org-todo ((t (:inherit default :background unspecified))))
  (org-done ((t (:inherit default :background unspecified :foreground ,(plist-get user-ui/colors :gray2)))))
  (org-checkbox-todo-bullet ((t (:inherit default :background unspecified :foreground ,(plist-get user-ui/colors :gray5)))))
  (org-checkbox-some-text ((t (:inherit default :background unspecified :foreground ,(plist-get user-ui/colors :orange)))))
  (org-level-1 ((t (:inherit default :background unspecified :foreground ,(plist-get user-ui/colors :blue)))))
  (org-level-2 ((t (:inherit default :background unspecified :foreground ,(plist-get user-ui/colors :red)))))
  (org-level-3 ((t (:inherit default :background unspecified :foreground ,(plist-get user-ui/colors :magenta)))))
  (org-level-4 ((t (:inherit default :background unspecified :foreground ,(plist-get user-ui/colors :yellow)))))
  (org-level-5 ((t (:inherit default :background unspecified :foreground ,(plist-get user-ui/colors :orange)))))
  (org-level-6 ((t (:inherit default :background unspecified :foreground ,(plist-get user-ui/colors :green)))))
  (org-level-7 ((t (:inherit default :background unspecified :foreground ,(plist-get user-ui/colors :cyan)))))
  (org-level-8 ((t (:inherit default :background unspecified :foreground ,(plist-get user-ui/colors :pink))))))

(use-package org-bullets
  :init
  (setq org-bullets-bullet-list
        '("●" "◐" "◑" "◒" "◓" "○"))
  :ghook
  'org-mode-hook)

(use-package org-roam
  :ghook
  ('after-init-hook 'org-roam-mode)
  :custom
  (org-roam-directory "~/Dropbox (Personal)/Notes")
  :general
  ('org-roam-mode-map
   "C-c n l" 'org-roam
   "C-c n f" 'org-roam-find-file
   "C-c n g" 'org-roam-graph)
  ('org-mode-map
   "C-c n i" 'org-roam-insert
   "C-c n I" 'org-roam-insert-immediate))

(use-package org-transclusion
  :straight (org-transclusion :type git :host github :repo "nobiot/org-transclusion" :branch "main")
  :general
  ("C-c o T" 'org-transclusion-mode))

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
        evil-replace-state-cursor `(,(plist-get user-ui/colors :pink) bar)
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
   "ZZ" 'save-buffers-kill-terminal)
  (:states '(normal motion)
   "j" 'evil-next-visual-line
   "k" 'evil-previous-visual-line
   "gj" 'evil-next-line
   "gk" 'evil-previous-line))

(use-package avy
  ;;  installed along with evil
  :straight nil
  :init
  (setq avy-style 'at-full
        avy-keys '(?a ?o ?e ?u ?i ?d ?h ?t ?n ?s)
        avy-background t
        avy-dispatch-alist '((?x . avy-action-kill-move)
                             (?X . avy-action-kill-stay)
                             (?T . avy-action-teleport)
                             (?v . avy-action-mark)
                             (?P . avy-action-copy)
                             (?y . avy-action-yank)
                             (?Y . avy-action-yank-line)
                             (?K . user/avy-action-helpful-at-point)
                             (?D . avy-action-zap-to-char)))
  :config

  ;; HACK: redef function
  (defun avy--overlay (str beg end wnd &optional compose-fn)
    "Create an overlay with STR from BEG to END in WND.
COMPOSE-FN is a lambda that concatenates the old string at BEG with STR."
    (let ((eob (with-selected-window wnd (point-max))))
      (when (<= beg eob)
        (let* ((beg (+ beg avy--overlay-offset))
               (ol (make-overlay beg (or end (1+ beg)) (window-buffer wnd)))
               (old-str (if (eq beg eob) "" (avy--old-str beg wnd)))
               (os-line-prefix (get-text-property 0 'line-prefix old-str))
               (os-wrap-prefix (get-text-property 0 'wrap-prefix old-str))
               (comp (with-selected-window wnd (find-composition beg end nil t)))
               other-ol)
          (when os-line-prefix
            (add-text-properties 0 1 `(line-prefix ,os-line-prefix) str))
          (when os-wrap-prefix
            (add-text-properties 0 1 `(wrap-prefix ,os-wrap-prefix) str))
          ;; The following expression makes avy-goto-line work with prettified symbols.
          ;; without it, the avy overlay would temporarily break the composed symbol, which
          ;; would result in the display jumping around. To prevent this, we look for the
          ;; prettify-symbols-start and prettify-symbols-end properties, and then create an
          ;; invisible overlay to hide the broken composition. We then create another overlay
          ;; to display the pretty glyph. This way, the buffer looks the same during candidate
          ;; selection. No bouncing.
          (when (and comp (get-text-property (nth 0 comp) 'prettify-symbols-start (window-buffer wnd)))
            (let* ((comp-from (nth 0 comp))
                   (comp-to (nth 1 comp))
                   (ch (string (aref (nth 2 comp) 0)))
                   (ch-ol (make-overlay comp-from (1+ comp-from) (window-buffer wnd)))
                   (ps-ol (make-overlay comp-from comp-to (window-buffer wnd))))
              (overlay-put ch-ol 'display ch)
              (overlay-put ch-ol 'window wnd)
              (overlay-put ch-ol 'priority -50)
              (push ch-ol avy--overlays-lead)
              (overlay-put ps-ol 'window wnd)
              (overlay-put ps-ol 'invisible t)
              (push ps-ol avy--overlays-lead)))
          (when (setq other-ol (cl-find-if
                                (lambda (o) (overlay-get o 'goto-address))
                                (overlays-at beg)))
            (add-text-properties
             0 (length old-str)
             `(face ,(overlay-get other-ol 'face)) old-str))
          (overlay-put ol 'window wnd)
          (overlay-put ol 'category 'avy)
          (overlay-put ol 'priority 2)
          ;; FIXME: doesn't take into account wrap-prefix
          (if (and os-line-prefix (eq avy-command 'avy-goto-line))
              ;; The following attempts to make avy-goto-line work well with org-indent-mode.
              ;; org-indent-mode uses line-prefix and wrap-prefix to add virtual spaces to a
              ;; buffer, which makes avy's overlays look "jagged". To work around this, we use
              ;; line-prefix to show avy's overlays. Since these overlays could be wider than
              ;; the line-prefix, we also show the remainder as a normal overlay, on top of the
              ;; text in the buffer.
              (let ((s-len (length str))
                    (p-len (length os-line-prefix)))
                (if (<= s-len p-len)
                    (overlay-put ol 'line-prefix (concat str (substring os-line-prefix s-len p-len)))
                  (overlay-put ol 'line-prefix (substring str 0 p-len))
                  ;; FIXME: doesn't take into account visual line mode
                  (let* ((os (car (split-string
                                   (with-selected-window wnd
                                     (buffer-substring beg end))
                                   "\n")))
                         (ns (funcall
                              (or compose-fn #'concat)
                              (substring str p-len s-len)
                              old-str)))
                    (unless (or (= (length os) 0) (= p-len 0))
                      (overlay-put ol 'after-string (if avy-background
                                                        (propertize (substring os (- p-len)) 'face 'avy-background-face)
                                                      (substring os (- p-len)))))
                    (overlay-put ol (if (eq beg eob)
                                        'after-string
                                      'display)
                                 ns))))
            (overlay-put ol (if (eq beg eob)
                                'after-string
                              'display)
                         (funcall
                          (or compose-fn #'concat)
                          str old-str)))
          (push ol avy--overlays-lead)))))

  ;; HACK: redef function
  (defun avy--make-backgrounds (wnd-list)
    "Create a dim background overlay for each window on WND-LIST."
    (when avy-background
      (setq avy--overlays-back
            (mapcar (lambda (w)
                      (let ((ol (make-overlay
                                 (window-start w)
                                 (window-end w)
                                 (window-buffer w))))
                        (overlay-put ol 'face 'avy-background-face)
                        ;; Give the overlay a high priority so it sits on top of other ones.
                        (overlay-put ol 'priority 1)
                        (overlay-put ol 'window w)
                        ol))
                    wnd-list))))

  (defun user/avy-action-helpful-at-point (pt)
    (save-excursion
      (goto-char pt)
      (helpful-at-point)))

  (general-add-advice
   'avy-action-mark
   :after (lambda (_)
            (backward-char)
            (unless (evil-visual-state-p)
              (evil-visual-state))))
  :general
  (:states '(normal visual)
   "SPC" 'evil-avy-goto-char-2
   "RET" 'evil-avy-goto-line
   "gB" 'evil-avy-goto-symbol-1-above
   "gW" 'evil-avy-goto-symbol-1-below
   "gT" 'avy-org-refile-as-child)
  :custom-face
  (avy-lead-face ((t (:bold t :italic nil :foreground ,(plist-get user-ui/colors :yellow) :background unspecified))))
  (avy-lead-face-0 ((t (:bold t :italic nil :foreground ,(plist-get user-ui/colors :yellow) :background unspecified))))
  (avy-lead-face-1 ((t (:bold t :italic nil :foreground ,(plist-get user-ui/colors :yellow) :background unspecified))))
  (avy-lead-face-2 ((t (:bold t :italic nil :foreground ,(plist-get user-ui/colors :yellow) :background unspecified))))
  (avy-background-face ((t (:foreground ,(plist-get user-ui/colors :gray3) :underline nil)))))

(use-package daemons
  :after evil-collection
  :config
  (evil-collection-init 'daemons)
  :general
  ("C-c D" 'daemons))

(defvar user/evil-collection-packages
  '(man
    calc
    proced
    custom
    comint
    edebug
    profiler
    elisp-mode
    diff-mode
    unimpaired
    process-menu))

(use-package dired
  :straight nil
  :after evil-collection
  :init
  (setq dired-listing-switches "-alh --group-directories-first"
        insert-directory-program (executable-find "gls")
        dired-auto-revert-buffer t
        dired-dwim-target t
        dired-hide-details-hide-symlink-targets nil
        dired-recursive-copies  'always
        dired-recursive-deletes 'top)
  :config
  (evil-collection-init 'dired)

  (put 'dired-find-alternate-file 'disabled nil)

  (defun user/dired-do-kill-lines ()
    (interactive)
    (setq current-prefix-arg '(4))
    (call-interactively 'dired-do-kill-lines))

  (defun user/dired-quit-all ()
    "Kill all `dired-mode' buffers."
    (interactive)
    (dolist (buf (buffer-list (current-buffer)))
      (with-current-buffer buf
        (when (eq major-mode 'dired-mode)
          (kill-buffer buf)))))

  :gfhook
  #'dired-hide-details-mode
  :general
  (:states '(normal visual)
   "-" 'dired-jump)
  ("C-c d d" 'dired)
  ('normal
   'dired-mode-map
   "q" 'user/dired-quit-all
   "gK" 'user/dired-do-kill-lines)
  :custom-face
  (dired-mark ((t (:foreground ,(plist-get user-ui/colors :yellow) :bold t))))
  (dired-marked ((t (:foreground unspecified :bold t))))
  (dired-header ((t (:foreground ,(plist-get user-ui/colors :magenta) :bold t)))))

(use-package dired-x
  :straight nil
  :config
  (setq dired-omit-verbose nil
        dired-omit-files
        (concat dired-omit-files
                "\\|^.DS_Store\\'"
                "\\|^.project\\(?:ile\\)?\\'"
                "\\|^.\\(svn\\|git\\)\\'"
                "\\|^.ccls-cache\\'"
                "\\|\\(?:\\.js\\)?\\.meta\\'"
                "\\|\\.\\(?:elc\\|o\\|pyo\\|swp\\|class\\)\\'")
        dired-clean-confirm-killing-deleted-buffers nil))

(use-package dired-git-info
  :init
  (setq dgi-commit-message-format "%h %s"
        dgi-auto-hide-details-p nil)
  :general
  ('dired-mode-map
   :states 'normal
   ")" 'dired-git-info-mode))

(use-package dired-filter
  :init
  (setq dired-filter-verbose nil
        dired-filter-stack '((omit) (git-ignored)))
  :ghook
  'dired-mode-hook)

(use-package dired-narrow)

(use-package dired-subtree
  :custom-face
  (dired-subtree-depth-1-face ((t (:background unspecified))))
  (dired-subtree-depth-2-face ((t (:background unspecified))))
  (dired-subtree-depth-3-face ((t (:background unspecified))))
  (dired-subtree-depth-4-face ((t (:background unspecified))))
  (dired-subtree-depth-5-face ((t (:background unspecified))))
  (dired-subtree-depth-6-face ((t (:background unspecified)))))

(use-package dired-collapse
  :ghook
  'dired-mode-hook)

(use-package diredfl
  :config
  (diredfl-global-mode)
  :custom-face
  (diredfl-autofile-name ((t (:inherit default :foreground ,(plist-get user-ui/colors :gray5)))))
  (diredfl-compressed-file-name ((t (:inherit default))))
  (diredfl-compressed-file-suffix ((t (:inherit default))))
  (diredfl-date-time ((t (:inherit default :foreground ,(plist-get user-ui/colors :gray5)))))
  (diredfl-deletion ((t (:inherit default :foreground ,(plist-get user-ui/colors :red)))))
  (diredfl-deletion-file-name ((t (:inherit default))))
  (diredfl-dir-heading ((t (:inherit default :foreground ,(plist-get user-ui/colors :magenta) :bold t))))
  (diredfl-dir-name ((t (:inherit default :foreground ,(plist-get user-ui/colors :blue)))))
  (diredfl-dir-priv ((t (:inherit default :foreground ,(plist-get user-ui/colors :blue)))))
  (diredfl-exec-priv ((t (:inherit default :foreground ,(plist-get user-ui/colors :red)))))
  (diredfl-executable-tag ((t (:inherit default :foreground ,(plist-get user-ui/colors :red)))))
  (diredfl-file-name ((t (:inherit default))))
  (diredfl-file-suffix ((t (:inherit default :bold t))))
  (diredfl-flag-mark ((t (:inherit default :foreground ,(plist-get user-ui/colors :yellow)))))
  (diredfl-flag-mark-line ((t (:foreground unspecified :background unspecified))))
  (diredfl-ignored-file-name ((t (:inherit default :foreground ,(plist-get user-ui/colors :gray5)))))
  (diredfl-link-priv ((t (:inherit default))))
  (diredfl-no-priv ((t (:inherit default :foreground ,(plist-get user-ui/colors :gray5)))))
  (diredfl-number ((t (:inherit default :foreground ,(plist-get user-ui/colors :orange)))))
  (diredfl-other-priv ((t (:inherit default))))
  (diredfl-rare-priv ((t (:inherit default))))
  (diredfl-read-priv ((t (:inherit default :foreground ,(plist-get user-ui/colors :green)))))
  (diredfl-symlink ((t (:inherit default :foreground ,(plist-get user-ui/colors :cyan)))))
  (diredfl-tagged-autofile-name ((t (:inherit default :foreground ,(plist-get user-ui/colors :gray5)))))
  (diredfl-write-priv ((t (:inherit default :foreground ,(plist-get user-ui/colors :yellow))))))

(use-package shackle)
(use-package fringe-helper)

(use-package diff-hl
  :init
  (setq diff-hl-side 'right
        diff-hl-fringe-bmp-function 'diff-hl-fringe-bmp-from-type)
  :config
  ;; HACK: redef function
  (defun diff-hl-define-bitmaps ()
    (-map
     (lambda (type)
       (define-fringe-bitmap type
         [3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3]
         nil nil 'center))
     '(exclamation-mark
       question-mark
       diff-hl-bmp-i
       diff-hl-bmp-top
       diff-hl-bmp-middle
       diff-hl-bmp-bottom
       diff-hl-bmp-single
       diff-hl-bmp-insert
       diff-hl-bmp-delete
       diff-hl-bmp-empty)))

  (diff-hl-define-bitmaps)
  (diff-hl-flydiff-mode)

  :ghook
  'text-mode-hook
  'prog-mode-hook
  'conf-mode-hook
  ('dired-mode-hook 'diff-hl-dired-mode)
  ('magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  ('magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  :custom-face
  (diff-hl-change ((t (:inherit diff-changed :foreground unspecified))))
  (diff-hl-dired-ignored ((t (:foreground ,(plist-get user-ui/colors :black)))))
  (diff-hl-dired-unknknown ((t (:foreground ,(plist-get user-ui/colors :black))))))

(use-package ibuffer
  :after evil-collection
  :config
  (evil-collection-init 'ibuffer)
  :general
  (:states 'normal
   "_" 'ibuffer)
  ("C-c b b" 'ibuffer))

(use-package ibuffer-projectile
  :after ibuffer
  :init
  (setq ibuffer-formats
        '((mark modified read-only locked " "
                (name 18 18 :left :elide)
                " "
                (size 9 -1 :right)
                " "
                (mode 16 16 :left :elide)
                " "
                project-relative-file)))
  :ghook
  ('ibuffer-hook  (lambda ()
                    (ibuffer-projectile-set-filter-groups)
                    (unless (eq ibuffer-sorting-mode 'alphabetic)
                      (ibuffer-do-sort-by-alphabetic)))))

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
  :config
  (evil-org-set-key-theme '(navigation insert textobjects todo))

  (general-def
    'normal
    'org-mode-map
    "go" (evil-org-define-eol-command org-insert-heading)
    "gO" (evil-org-define-eol-command org-insert-subheading))
  :ghook
  'org-mode-hook)

(use-package evil-multiedit
  :init
  (setq evil-multiedit-follow-matches t)
  :config
  (evil-ex-define-cmd "ie[dit]" 'evil-multiedit-ex-match)
  :general
  (:states '(normal visual)
   "R" 'evil-multiedit-match-all
   "gR" 'evil-multiedit-restore)
  (:states 'normal
   "gn" 'evil-multiedit-match-symbol-and-next
   "gN" 'evil-multiedit-match-symbol-and-prev)
  (:states 'visual
   "gn" 'evil-multiedit-match-and-next
   "gN" 'evil-multiedit-match-and-prev)
  (:states 'insert
   "C-t" 'evil-multiedit-toggle-marker-here)
  (:states 'motion
   "RET" 'evil-multiedit-toggle-or-restrict-region)
  ('evil-multiedit-state-map
   "RET" 'evil-multiedit-toggle-or-restrict-region)
  ('(evil-multiedit-state-map
     evil-multiedit-insert-state-map)
   "C-n" 'evil-multiedit-next
   "C-p" 'evil-multiedit-prev)
  :custom-face
  (iedit-occurrence ((t (:foreground ,(plist-get user-ui/colors :black) :background ,(plist-get user-ui/colors :red))))))

(use-package prodigy
  :after evil-collection
  :config
  (evil-collection-init 'prodigy)
  :general
  ("C-c S" 'prodigy)
  :custom-face
  (prodigy-green-face ((t (:inherit success))))
  (prodigy-red-face ((t (:inherit error))))
  (prodigy-yellow-face ((t (:inherit warning)))))

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

(use-package magit
  :demand t
  :after evil-snipe
  :init
  (setq magit-repository-directories
        '(("~/Projects/" . 2)
          ("~/.emacs.d". 1)))
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
  ("C-c g s" 'magit-status
   "C-c g b" 'magit-blame-addition)
  :ghook
  ('git-commit-mode-hook 'evil-insert-state)
  :gfhook
  #'turn-off-evil-snipe-override-mode
  :custom-face
  (magit-bisect-bad ((t (:inherit magit-signature-error))))
  (magit-bisect-good ((t (:inherit magit-signature-good))))
  (magit-bisect-skip ((t (:inherit magit-signature-untrusted))))
  (magit-diff-added ((t (:inherit diff-added))))
  (magit-diff-added-highlight ((t (:inherit diff-added :background ,(plist-get user-ui/colors :gray1)))))
  (magit-diff-base ((t (:foreground ,(plist-get user-ui/colors :yellow)))))
  (magit-diff-base-highlight ((t (:inherit magit-diff-base :background ,(plist-get user-ui/colors :gray1)))))
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

(use-package forge
  :after magit)

(use-package evil-magit)

(use-package magit-todos
  :init
  (setq magit-todos-section-map (make-sparse-keymap))
  :config
  (magit-todos-mode)
  :general
  ("C-c g t" 'ivy-magit-todos))

(use-package browse-at-remote
  :general
  ("C-c g b" 'browse-at-remote))

(use-package hydra
  :custom-face
  (hydra-face-red ((t (:foreground ,(plist-get user-ui/colors :red)))))
  (hydra-face-teal ((t (:foreground ,(plist-get user-ui/colors :cyan)))))
  (hydra-face-pink ((t (:foreground ,(plist-get user-ui/colors :pink)))))
  (hydra-face-blue ((t (:foreground ,(plist-get user-ui/colors :blue)))))
  (hydra-face-amaranth ((t (:foreground ,(plist-get user-ui/colors :magenta))))))

(use-package flx)

(use-package amx
  :config
  (amx-mode))

(use-package ivy
  :after amx flx evil-collection
  :init
  (setq ivy-use-virtual-buffers t
        ivy-virtual-abbreviate 'full
        ivy-height 17
        ivy-wrap t
        ivy-fixed-height-minibuffer t
        ivy-read-action-format-function #'ivy-read-action-format-columns
        ivy-re-builders-alist '((swiper . ivy--regex-plus)
                                (counsel-projectile-rg . ivy--regex-plus)
                                (t . ivy--regex-fuzzy)))
  :config
  (ivy-mode 1)
  (evil-collection-init 'ivy)

  ;; HACK: redef function
  (defun ivy-switch-buffer (&optional initial-input)
    "Switch to another buffer."
    (interactive)
    (ivy-read "Switch to buffer: " #'internal-complete-buffer
              :keymap ivy-switch-buffer-map
              :preselect (buffer-name (other-buffer (current-buffer)))
              :initial-input initial-input
              :action #'ivy--switch-buffer-action
              :matcher #'ivy--switch-buffer-matcher
              :caller 'ivy-switch-buffer))

  :general
  ("C-c /" 'ivy-resume)
  ('ivy-minibuffer-map
   "<C-return>" 'ivy-dispatching-done
   "C-c RET" 'ivy-immediate-done
   "C-c o" 'ivy-occur
   "C-s" (lambda ()
           (interactive)
           (face-remap-add-relative
            'avy-background-face `(:foreground ,(plist-get user-ui/colors :gray5)))
           (call-interactively 'ivy-avy))
   "C-w" 'ivy-backward-kill-word
   "C-u" 'ivy-scroll-down-command
   "C-d" 'ivy-scroll-up-command))

(use-package ivy-avy)

(use-package ivy-hydra
  :init
  (setq ivy-read-action-function #'ivy-hydra-read-action))

(use-package all-the-icons-ivy-rich
  :init
  (setq all-the-icons-ivy-rich-color-icon t))

(use-package ivy-rich
  :after counsel-projectile all-the-icons-ivy-rich
  :init
  (setq ivy-rich-path-style 'abbrev
        ivy-rich-parse-remote-buffer nil)
  :config
  ;; highlight the whole candidate line
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)

  (all-the-icons-ivy-rich-mode 1)

  ;; HACK: redef function
  (defun counsel-projectile-transformer (str)
    "Fontifies modified, file-visiting buffers as well as non-visited files."
    (if (member str counsel-projectile--buffers)
        (counsel-projectile-switch-to-buffer-transformer str)
      (concat
       (all-the-icons-ivy-rich-file-icon str)
       "\t"
       (counsel-projectile-find-file-transformer str)
       "\t"
       (ivy-rich-counsel-find-file-truename str))))

  (plist-put
   ivy-rich-display-transformers-list
   'counsel-projectile-switch-project
   '(:columns
     ((all-the-icons-ivy-rich-file-icon)
      (ivy-rich-counsel-projectile-switch-project-project-name (:width 0.2 :face success))
      (ivy-rich-candidate))
     :delimiter "\t"))

  (plist-put
   ivy-rich-display-transformers-list
   'ivy-switch-buffer
   '(:columns
     ((all-the-icons-ivy-rich-buffer-icon)
      (ivy-rich-candidate (:width 0.25))
      (ivy-rich-switch-buffer-size (:width 7))
      (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
      (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
      (ivy-rich-switch-buffer-project (:width 15 :face success))
      (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
     :predicate
     (lambda (cand) (get-buffer cand))
     :delimiter "\t"))

  (ivy-rich-mode 1))

(use-package mini-frame
  ;; FIXME: mini-frame popping up unexpectedly
  ;; Running the following code:
  ;; (shell-command "sleep 1; echo $?" t)
  ;; ...causes the mini-frame to pop up off to the left, without focus
  ;; FIXME: mini-frame goes into a loop when using ivy actions (recursive minibuffer?)
  :disabled t
  :init
  (setq
   mini-frame-color-shift-step 11
   mini-frame-internal-border-color (plist-get user-ui/colors :gray5)
   mini-frame-show-parameters
   '((left . 0.5)
     (top . 0.33)
     (width . 0.33)
     (height . 1)
     (internal-border-width . 1)))
  :config
  (mini-frame-mode)
  (general-add-advice
   'ivy--resize-minibuffer-to-fit
   :after (lambda ()
            (when (and (frame-live-p mini-frame-frame)
	                     (frame-visible-p mini-frame-frame))
              (window--resize-mini-frame mini-frame-frame)))))

(use-package helpful
  :after evil-collection
  :config
  (evil-collection-init 'helpful)

  (defun user/helpful-quit-all ()
    "Kill all `dired-mode' buffers."
    (interactive)
    (dolist (buf (buffer-list (current-buffer)))
      (with-current-buffer buf
        (when (eq major-mode 'helpful-mode)
          (kill-buffer buf)))))

  :general
  ('normal
   'helpful-mode-map
   "Q" 'user/helpful-quit-all)
  (:states '(normal visual)
   "K" 'helpful-at-point)
  ("C-h f" 'helpful-callable
   "C-h v" 'helpful-variable
   "C-h k" 'helpful-key
   "C-h F" 'helpful-function
   "C-h C" 'helpful-command))

(use-package swiper
  :init
  (setq swiper-action-recenter t)
  :config
  (general-add-advice
   'swiper--action
   :after
   (lambda (_)
     (goto-char (match-beginning 0))))

  (general-add-advice
   '(ivy-next-line ivy-previous-line)
   :after (lambda (_)
            (add-to-history 'regexp-search-ring (ivy--regex ivy-text))
            (setq isearch-forward t)))

  (general-add-advice
   'swiper--make-overlay
   :around (defun user/around-swiper--make-overlay (f beg end face wnd priority)
             (funcall f beg end face wnd (and priority (+ priority 0)))))

  ;; HACK: redef function
  (defun swiper--avy-candidate ()
    (let ((candidates (swiper--avy-candidates))
          (avy-all-windows nil))
      (unwind-protect
          (prog2
              (avy--make-backgrounds
               (append (avy-window-list)
                       (list (ivy-state-window ivy-last))))
              (if (eq avy-style 'de-bruijn)
                  (avy-read-de-bruijn candidates avy-keys)
                (avy-read (avy-tree candidates avy-keys)
                          ;; overlay-at-full doesn't move the text in the buffer
                          #'avy--overlay-at-full
                          #'avy--remove-leading-chars))
            (avy-push-mark))
        (avy--done))))

  ;; HACK: redef function
  (defun swiper--avy-candidates ()
    (let* (
           ;; We'll have overlapping overlays, so we sort all the
           ;; overlays in the visible region by their start, and then
           ;; throw out non-Swiper overlays or overlapping Swiper
           ;; overlays.
           (visible-overlays (cl-sort (with-ivy-window
                                        (overlays-in (window-start)
                                                     (window-end)))
                                      #'< :key #'overlay-start))
           (min-overlay-start 0)
           (overlays-for-avy
            (cl-remove-if-not
             (lambda (ov)
               (when (and (>= (overlay-start ov)
                              min-overlay-start)
                          (memq (overlay-get ov 'face)
                                (append swiper-faces swiper-background-faces)))
                 (setq min-overlay-start (overlay-start ov))))
             visible-overlays))
           (offset (if (eq (ivy-state-caller ivy-last) 'swiper) 1 0)))
      (nconc
       (mapcar (lambda (ov)
                 ;; Apply the avy-lead-face to any repurposed overlays, otherwise they'd
                 ;; still have a normal swiper-face or swiper-background-faces.
                 (overlay-put ov 'face 'avy-lead-face)
                 (cons (overlay-start ov)
                       (overlay-get ov 'window)))
               overlays-for-avy)
       (save-excursion
         (save-restriction
           (narrow-to-region (window-start) (window-end))
           (goto-char (point-min))
           (forward-line)
           (let ((win (selected-window))
                 cands)
             (while (not (eobp))
               (push (cons (+ (point) offset) win)
                     cands)
               (forward-line))
             cands))))))

  :general
  ('swiper-map
   "C-s" 'swiper-avy)
  (:states '(normal visual)
   "/" (lambda () (interactive) (setq isearch-forward t) (swiper)))
  :custom-face
  (swiper-line-face ((t (:background ,(plist-get user-ui/colors :gray3)
                         :distant-foreground ,(plist-get user-ui/colors :gray5))))))

(use-package counsel
  :init
  (setq counsel-find-file-ignore-regexp nil
        counsel-rg-base-command
        (split-string "rg --sort path -M 240 --no-heading --line-number --color never %s"))
  :config
  (counsel-mode)

  ;; HACK: redef function
  (defun counsel-switch-buffer (&optional initial-input)
    "Switch to another buffer.
Display a preview of the selected ivy completion candidate buffer
in the current window."
    (interactive)
    (let ((ivy-update-fns-alist
           '((ivy-switch-buffer . counsel--switch-buffer-update-fn)))
          (ivy-unwind-fns-alist
           '((ivy-switch-buffer . counsel--switch-buffer-unwind))))
      (ivy-switch-buffer initial-input)))

  (defun user/evil-complete-from-ivy-read-file (string predicate flag)
    (when (eq flag t)
      (let ((ivy-inhibit-action t))
        (list (counsel-find-file string)))))

  (defun user/evil-complete-from-ivy-read-buffer (string predicate flag)
    (when (eq flag t)
      (let ((ivy-inhibit-action t))
        (list (counsel-switch-buffer string)))))

  (evil-ex-define-argument-type file :collection user/evil-complete-from-ivy-read-file)
  (evil-ex-define-argument-type buffer :collection user/evil-complete-from-ivy-read-buffer))

(use-package perspective
  :init
  (setq persp-state-default-file (expand-file-name "perspective-state" user-emacs-directory))
  :config
  (persp-mode)
  (general-add-hook 'kill-emacs-hook #'persp-state-save))

(use-package projectile
  :demand t
  :after magit
  :init
  (setq projectile-completion-system 'ivy
        projectile-enable-caching nil
        projectile-track-known-projects-automatically nil
        projectile-file-exists-local-cache-expire 30
        projectile-globally-ignored-directories
        '(".idea"
          ".ensime_cache"
          ".eunit"
          ".git"
          ".hg"
          ".fslckout"
          "_FOSSIL_"
          ".bzr"
          "_darcs"
          ".tox"
          ".svn"
          ".stack-work"
          "node_modules"))
  :config
  (projectile-mode)

  (setq projectile-known-projects '())
  (-each (-map #'file-name-as-directory (magit-list-repos))
    #'projectile-add-known-project)

  :general
  ('projectile-mode-map
   "C-c p" 'projectile-command-map))

(use-package rg)

(use-package ace-window
  :after evil
  :demand t
  :preface
  (setq aw-dispatch-alist
        ;; TODO: add key for last buffer? maybe w?
        '((?d aw-delete-window "delete window")
          (?x aw-swap-window "swap windows")
          (?m aw-move-window "move window")
          (?y aw-copy-window "copy window")
          (?v user/aw-split-window-horz-go "split window vertically, go")
          (?h user/aw-split-window-vert-go "split window horizontally, go")
          (?V aw-split-window-horz "split window vertically")
          (?H aw-split-window-vert "split window horizontally")
          (?1 delete-other-windows "delete other windows")
          (?? aw-show-dispatch-help)))
  :init
  (setq aw-keys '(?a ?o ?e ?u ?i ?t ?n ?s)
        aw-dispatch-always t
        aw-dispatch-when-more-than 1
        aw-leading-char-style 'path)
  :config

  ;; HACK: redef function
  (defun aw--lead-overlay (path leaf)
    "Create an overlay using PATH at LEAF.
LEAF is (PT . WND)."
    ;; Properly adds overlay in visible region of most windows except for any one
    ;; receiving output while this function is executing, since that moves point,
    ;; potentially shifting the added overlay outside the window's visible region.
    (let ((wnd (cdr leaf))
          ;; Prevent temporary movement of point from scrolling any window.
          (scroll-margin 0))
      (with-selected-window wnd
        (when (= 0 (buffer-size))
          (push (current-buffer) aw-empty-buffers-list)
          (let ((inhibit-read-only t))
            (insert " ")))
        ;; If point is not visible due to horizontal scrolling of the
        ;; window, this next expression temporarily scrolls the window
        ;; right until point is visible, so that the leading-char can be
        ;; seen when it is inserted.  When ace-window's action finishes,
        ;; the horizontal scroll is restored by (aw--done).
        (while (and (not (aw--point-visible-p))
                    (not (zerop (window-hscroll)))
                    (progn (push (cons (selected-window) (window-hscroll)) aw--windows-hscroll) t)
                    (not (zerop (scroll-right)))))
        (let* ((ws (window-start))
               (prev nil)
               (vertical-pos (if (eq aw-char-position 'left) -1 0))
               (horizontal-pos (if (zerop (window-hscroll)) 0 (1+ (window-hscroll))))
               (old-pt (point))
               (pt
                (progn
                  ;; If leading-char is to be displayed at the top-left, move
                  ;; to the first visible line in the window, otherwise, move
                  ;; to the last visible line.
                  (move-to-window-line vertical-pos)
                  (move-to-column horizontal-pos)
                  ;; Find a nearby point that is not at the end-of-line but
                  ;; is visible so have space for the overlay.
                  (setq prev (1- (point)))
                  (while (and (>= prev ws) (/= prev (point)) (eolp))
                    (setq prev (point))
                    (unless (bobp)
                      (line-move -1 t)
                      (move-to-column horizontal-pos)))
                  (recenter vertical-pos)
                  (point)))
               (ol (make-overlay pt (1+ pt) (window-buffer wnd))))
          (if (= (aw--face-rel-height) 1)
              (goto-char old-pt)
            (when (/= pt old-pt)
              (goto-char (+ pt 1))
              (push (cons wnd old-pt) aw--windows-points)))
          ;; Make ace-window play nice with org-indent-mode. Check for a line prefix,
          ;; and if one exists, put the ace-window path string there, to avoid having
          ;; the text in the buffer bounce around.
          (let* ((s (propertize
                     (aw--overlay-str wnd pt path)
                     'face (if (window-minibuffer-p)
                               'aw-minibuffer-leading-char-face
                             'aw-leading-char-face)))
                 (s-len (length s))
                 (p (get-text-property pt 'line-prefix (current-buffer)))
                 (p-len (length p)))
            (if (<= s-len p-len)
                (overlay-put ol 'line-prefix (concat s (substring p s-len)))
              (overlay-put ol 'display s)))
          (overlay-put ol 'window wnd)
          (push ol avy--overlays-lead)))))

  ;; HACK: redef function
  (defun aw--make-backgrounds (wnd-list)
    "Create a dim background overlay for each window on WND-LIST."
    (when aw-background
      (setq aw-overlays-back
            (mapcar (lambda (w)
                      (let ((ol (make-overlay
                                 (window-start w)
                                 (window-end w)
                                 (window-buffer w))))
                        (overlay-put ol 'face 'aw-background-face)
                        ;; Give the overlay a high priority so it sits on top of other ones.
                        (overlay-put ol 'priority 100)
                        ol))
                    wnd-list))))

  (defun user/aw-split-window-horz-go (window)
    (select-window (aw-split-window-horz window)))

  (defun user/aw-split-window-vert-go (window)
    (select-window (aw-split-window-vert window)))

  (defun user/aw-select ()
    ;;  without redisplaying, the ace-window overlay is off by the height of the minibuffer
    (redisplay t)
    (with-selected-window (frame-selected-window)
      (aw-select "select window")))

  :general
  ("C-t" 'ace-window)
  (:states '(normal visual)
   "C-t" 'ace-window)
  :custom-face
  (aw-background-face ((t (:foreground ,(plist-get user-ui/colors :gray3) :underline nil))))
  (aw-leading-char-face ((t (:foreground ,(plist-get user-ui/colors :yellow) :bold t)))))

(use-package counsel-projectile
  :after ivy projectile ace-window
  :demand t
  :init
  (setq counsel-projectile-remove-current-project t
        counsel-projectile-org-capture-templates
        '(("t" "[${name}] Task" checkitem
           (file+headline "${root}/NOTES.org" "Tasks")
           "- [ ] %?\n")))
  :config
  (counsel-projectile-mode)

  ;; HACK: redef function
  (defun counsel-projectile-switch-project (&optional default-action)
    "Switch project.

Optional argument DEFAULT-ACTION is the key, function, name, or
index in the list `counsel-projectile-switch-project-action' (1
for the first action, etc) of the action to set as default."
    (interactive)
    (ivy-read (projectile-prepend-project-name "Switch to project: ")
              (if counsel-projectile-remove-current-project
                  (projectile-relevant-known-projects)
                projectile-known-projects)
              ;; don't preselect the current project if we've removed the current project
              :preselect (and (not counsel-projectile-remove-current-project)
                              (projectile-project-p)
                              (abbreviate-file-name (projectile-project-root)))
              :action (or (and default-action
                               (listp counsel-projectile-switch-project-action)
                               (integerp (car counsel-projectile-switch-project-action))
                               (cons (counsel-projectile--action-index
                                      default-action
                                      counsel-projectile-switch-project-action)
                                     (cdr counsel-projectile-switch-project-action)))
                          counsel-projectile-switch-project-action)
              :require-match t
              :sort counsel-projectile-sort-projects
              :caller 'counsel-projectile-switch-project))

  (defun user/counsel-projectile-switch-to-persp (project)
    "Switch to perspective for PROJECT. Create a perspective and return t if none exists."
    (let* ((name (or projectile-project-name
                     (funcall projectile-project-name-function project)))
           (persp (gethash name (perspectives-hash))))
      (cond ((and persp (not (equal persp (persp-curr)))) (persp-switch name) nil)
            ((not persp) (persp-switch name) t)
            (t nil))))

  (defun user/counsel-projectile-switch-project-action-persp (project)
    (when (user/counsel-projectile-switch-to-persp project)
      (counsel-projectile-switch-project-action project)))

  (-each (cdr counsel-projectile-switch-project-action)
    (lambda (action)
      (let ((name (nth 1 action)))
        (unless (memq name
                      '(counsel-projectile-switch-project-action-save-all-buffers
                        counsel-projectile-switch-project-action-kill-buffers
                        counsel-projectile-switch-project-action-remove-known-project
                        counsel-projectile-switch-project-action-org-capture))
          (general-add-advice
           name
           :before #'user/counsel-projectile-switch-to-persp)))))

  (--> counsel-projectile-switch-project-action
       (-snoc it '("P" user/counsel-projectile-switch-project-action-persp "switch to project persp"))
       (-replace-at 0 (1- (length it)) it)
       (setq counsel-projectile-switch-project-action it))

  (defun user/counsel-projectile-action (name)
    (if (member name counsel-projectile--buffers)
        (switch-to-buffer name nil 'force-same-window)
      (find-file (with-ivy-window (projectile-expand-root name)))))

  (defun user/counsel-projectile-action-go (window name)
    (select-window window)
    (user/counsel-projectile-action name))

  (defun user/counsel-projectile-action-stay (window name)
    (with-selected-window window
      (user/counsel-projectile-action name)))

  (defun user/counsel-projectile-action-ace-go (name)
    (user/counsel-projectile-action-go (user/aw-select) name))

  (defun user/counsel-projectile-action-ace-stay (name)
    (user/counsel-projectile-action-stay (user/aw-select) name))

  (defun user/counsel-projectile-action-horiz-go (name)
    (user/counsel-projectile-action-go (split-window-vertically) name))

  (defun user/counsel-projectile-action-horiz-stay (name)
    (user/counsel-projectile-action-stay (split-window-vertically) name))

  (defun user/counsel-projectile-action-vert-go (name)
    (user/counsel-projectile-action-go (split-window-horizontally) name))

  (defun user/counsel-projectile-action-vert-stay (name)
    (user/counsel-projectile-action-stay (split-window-horizontally) name))

  (ivy-set-actions
   'counsel-projectile
   '(("t" user/counsel-projectile-action-ace-go "to window, go")
     ("T" user/counsel-projectile-action-ace-stay "to window, stay")
     ("h" user/counsel-projectile-action-horiz-go "split horiz, go")
     ("H" user/counsel-projectile-action-horiz-stay "split horiz, stay")
     ("v" user/counsel-projectile-action-vert-go "split vert, go")
     ("V" user/counsel-projectile-action-vert-stay "split vert, stay")))

  :general
  ('projectile-mode-map
   "C-SPC" 'counsel-projectile
   "C-/" 'counsel-projectile-rg
   "C-c p p" 'counsel-projectile-switch-project
   "C-c o c" 'counsel-projectile-org-capture))

(use-package company
  :after evil-collection
  :init
  (setq company-idle-delay 0.5
        company-tooltip-align-annotations t
        company-minimum-prefix-length 2
        company-selection-wrap-around t
        company-transformers '(company-sort-by-occurrence))
  :config
  (global-company-mode)
  (evil-collection-init 'company)
  :general
  ('company-active-map
   "C-w" 'evil-delete-backward-word
   "C-s" 'counsel-company)
  :custom-face
  (company-scrollbar-bg ((t (:background ,(plist-get user-ui/colors :gray3)))))
  (company-scrollbar-fg ((t (:background ,(plist-get user-ui/colors :white)))))
  (company-tooltip-common ((t (:inherit company-tooltip :weight bold :underline nil))))
  (company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold :underline nil)))))

(use-package dumb-jump
  :ghook
  ('xref-backend-function #'dumb-jump-xref-activate))

(use-package term
  :straight nil
  :after evil-collection
  :config
  (evil-collection-init 'term)
  (defun user/after-term-line-mode ()
    (setq evil-emacs-state-cursor `(,(plist-get user-ui/colors :yellow) box)
          evil-insert-state-cursor `(,(plist-get user-ui/colors :green) bar)
          evil-motion-state-cursor `(,(plist-get user-ui/colors :magenta) box)
          evil-normal-state-cursor `(,(plist-get user-ui/colors :blue) box)
          evil-replace-state-cursor `(,(plist-get user-ui/colors :pink) bar)
          evil-visual-state-cursor `(,(plist-get user-ui/colors :orange) box))
    (evil-refresh-cursor))

  (defun user/after-term-char-mode ()
    (setq evil-emacs-state-cursor `(,(plist-get user-ui/colors :yellow) hbar)
          evil-insert-state-cursor `(,(plist-get user-ui/colors :green) hbar)
          evil-motion-state-cursor `(,(plist-get user-ui/colors :magenta) hbar)
          evil-normal-state-cursor `(,(plist-get user-ui/colors :blue) hbar)
          evil-replace-state-cursor `(,(plist-get user-ui/colors :pink) hbar)
          evil-visual-state-cursor `(,(plist-get user-ui/colors :orange) hbar))
    (evil-refresh-cursor))

  (general-add-advice 'term-line-mode :after 'user/after-term-line-mode)
  (general-add-advice 'term-char-mode :after 'user/after-term-char-mode)
  :gfhook
  #'(lambda ()
      (make-local-variable 'evil-emacs-state-cursor)
      (make-local-variable 'evil-insert-state-cursor)
      (make-local-variable 'evil-motion-state-cursor)
      (make-local-variable 'evil-normal-state-cursor)
      (make-local-variable 'evil-replace-state-cursor)
      (make-local-variable 'evil-visual-state-cursor)))

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
  :after evil-collection exec-path-from-shell ace-window
  :init
  (setq-default eshell-path-env (getenv "PATH"))
  (setq eshell-destroy-buffer-when-process-dies nil
        eshell-preoutput-filter-functions
        '(xterm-color-filter)
        eshell-output-filter-functions
        '(eshell-postoutput-scroll-to-bottom
          eshell-handle-control-codes
          eshell-watch-for-password-prompt))
  :config
  (evil-collection-init 'eshell)

  (defun user/eshell-run-command (name dir command)
    "Open a new eshell named NAME in DIR and run COMMAND."
    (let ((current-prefix-arg t))
      (let ((eshell-buffer-name (concat "*eshell " (or name "-") "*")))
        (let ((default-directory (or dir default-directory)))
          (call-interactively 'eshell))))
    (eshell-return-to-prompt)
    (insert command)
    (eshell-send-input))

  (defun user/eshell-run-command-in-window (window stay)
    (eshell-update-markers eshell-last-output-end)
    (let ((name (projectile-project-name))
          (root (projectile-project-root))
          (command (buffer-substring-no-properties eshell-last-input-start eshell-last-input-end)))
      (eshell-add-input-to-history command)
      (eshell-reset)
      (if stay
          (with-selected-window window
            (user/eshell-run-command name root command))
        (select-window window)
        (user/eshell-run-command name root command))))

  (defun user/eshell-run-command-vert-go ()
    (interactive)
    (user/eshell-run-command-in-window (split-window-horizontally) nil))

  (defun user/eshell-run-command-vert-stay ()
    (interactive)
    (user/eshell-run-command-in-window (split-window-horizontally) t))

  (defun user/eshell-run-command-horiz-go ()
    (interactive)
    (user/eshell-run-command-in-window (split-window-vertically) nil))

  (defun user/eshell-run-command-horiz-stay ()
    (interactive)
    (user/eshell-run-command-in-window (split-window-vertically) t))

  (defun user/eshell-run-command-ace-go ()
    (interactive)
    (user/eshell-run-command-in-window (user/aw-select) nil))

  (defun user/eshell-run-command-ace-stay ()
    (interactive)
    (user/eshell-run-command-in-window (user/aw-select) t))

  (defhydra user/eshell-actions-hydra (:color blue :hint nil :idle .3 :columns 3)
    ("RET" eshell-send-input "send input")
    ("t" user/eshell-run-command-ace-go "send input to window, go")
    ("T" user/eshell-run-command-ace-stay "send input to window, stay")
    ("v" user/eshell-run-command-vert-go "send input to vert split, go")
    ("V" user/eshell-run-command-vert-stay "send input to vert split, stay")
    ("h" user/eshell-run-command-horiz-go "send input to horiz split, go")
    ("H" user/eshell-run-command-horiz-stay "send input to horiz split, stay"))

  (defun user/eshell-clear ()
    (interactive)
    (eshell/clear-scrollback)
    (eshell-send-input nil nil t))

  :general
  ("C-c s n" 'user/run-new-eshell)
  :gfhook
  #'(lambda ()
      (setenv "TERM" "xterm-256color")
      (general-def 'insert 'eshell-mode-map
        "C-n" 'eshell-next-matching-input-from-input
        "C-p" 'eshell-previous-matching-input-from-input)
      (general-def 'eshell-mode-map
        "[remap eshell-pcomplete]" 'company-indent-or-complete-common
        "TAB" 'company-indent-or-complete-common
        "C-c s h" 'counsel-esh-history
        "C-c s c" 'user/eshell-clear
        "<C-return>" 'user/eshell-actions-hydra/body))
  ('eshell-before-prompt-hook (lambda ()
                                (setq xterm-color-preserve-properties t)))
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

(use-package eshell-toggle
  :straight (eshell-toggle :type git :host github :repo "4DA/eshell-toggle")
  :custom
  (eshell-toggle-size-fraction 3)
  (eshell-toggle-use-projectile-root t)
  (eshell-toggle-run-command nil)
  :general
  ("C-s" 'eshell-toggle))

(use-package company-fish
  :straight (company-fish :type git :host github :repo "CeleritasCelery/company-fish")
  :config
  (add-to-list 'company-backends 'company-fish))

(use-package vterm
  :after evil-collectionn
  :commands vterm
  :config
  (evil-collection-init 'vterm))

(use-package shx
  :config
  (shx-global-mode 1))

(use-package window-purpose
  :config
  (purpose-mode 1)
  (setq purpose-user-mode-purposes
        '((term-mode . terminal)
          (shell-mode . terminal)
          (ansi-term-mode . terminal)
          (vterm-mode . terminal)
          (eshell-mode . terminal)
          (org-mode . coding)
          (clojure-mode . coding)
          (emacs-lisp-mode . coding)
          (js2-mode . coding)
          (jrsx-mode . coding)
          (compilation-mode . messages)))
  (purpose-compile-user-configuration))

(use-package window-purpose-x
  :straight nil
  :after window-purpose
  :config
  (purpose-x-kill-setup))

(use-package treemacs
  :after user-mode-line
  :config
  (setq treemacs-no-png-images          t
        treemacs-follow-after-init      t
        treemacs-collapse-dirs          (if treemacs-python-executable 3 0)
        treemacs-is-never-other-window  t
        treemacs-project-follow-cleanup t
        treemacs-user-mode-line-format  (user-mode-line/treemacs))
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode t)
  (pcase (cons (not (null (executable-find "git")))
               (not (null treemacs-python-executable)))
    (`(t . t)
     (treemacs-git-mode 'extended))
    (`(t . _)
     (treemacs-git-mode 'simple)))
  :general
  ("C-c t s"     'treemacs-select-window
   "C-c t 1"     'treemacs-delete-other-windows
   "C-c t t"     'treemacs
   "C-c t B"     'treemacs-bookmark
   "C-c t C-t"   'treemacs-find-file
   "C-c t C-S-t" 'treemacs-find-tag)
  :custom-face
  (treemacs-root-face ((t (:inherit font-lock-keyword-face :bold t))))
  (treemacs-git-added-face ((t (:inherit diff-added))))
  (treemacs-git-modified-face ((t (:inherit diff-changed))))
  (treemacs-git-untracked-face ((t (:inherit font-lock-constant-face)))))

(use-package treemacs-evil
  :after treemacs evil)

(use-package treemacs-projectile
  :after treemacs projectile)

(use-package treemacs-magit
  :after treemacs magit)

(use-package treemacs-perspective
  :after treemacs perspective
  :config (treemacs-set-scope-type 'Perspectives))

(use-package beacon
  :init
  (setq beacon-size 7
        beacon-color (plist-get user-ui/colors :blue)
        beacon-blink-delay 0.1
        beacon-blink-duration 0.5
        beacon-blink-when-focused t
        beacon-blink-when-point-moves-vertically 10)
  :config
  (defun user/beacon-blink ()
    (interactive)
    (let ((beacon-size (- (window-body-width) (current-column))))
      (beacon-blink)))

  (general-add-advice
   'beacon-blink
   :around (lambda (f &rest args)
             (let ((beacon-color (face-background 'cursor)))
               (apply f args))))
  ;; FIXME: beacon start sticking, possibly after avy command?
  (beacon-mode 1)
  :general
  ("C-c B" 'user/beacon-blink))

(use-package olivetti
  :general
  ("C-c O" 'olivetti-mode))

(use-package origami
  :config
  (global-origami-mode))

(use-package markdown-mode
  :mode
  (("README\\.md\\'" . gfm-mode)
   ("\\.md\\'" . markdown-mode)
   ("\\.markdown\\'" . markdown-mode)))

(use-package focus
  :general
  ("C-c f" 'focus-mode))

(use-package flycheck
  :after el-patch
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
  (global-flycheck-mode)

  :custom-face
  (flycheck-error-list-info ((t (:inherit success)))))

(use-package flycheck-tip)

;;;  lisp

(defvar user/lisp-mode-hooks
  '(emacs-lisp-mode-hook
    clojure-mode-hook
    clojurec-mode-hook
    clojurescript-mode-hook))

(defun user/prettify-lisp-symbols ()
  "Prettify Lisp symbols."
  (push '("lambda" . "λ") prettify-symbols-alist)
  (prettify-symbols-mode))

(general-add-hook user/lisp-mode-hooks #'eldoc-mode)
(general-add-hook user/lisp-mode-hooks #'flycheck-mode)
(general-add-hook user/lisp-mode-hooks #'highlight-quoted-mode)
(general-add-hook user/lisp-mode-hooks #'user/prettify-lisp-symbols)
(general-add-hook 'emacs-lisp-mode-hook (lambda () (setq-local lisp-indent-function #'user/lisp-indent-function)))

(use-package clojure-mode
  :init
  (setq clojure-indent-style :always-indent))

(use-package ob-clojure
  :straight nil
  :init
  (setq org-babel-clojure-backend 'cider))

(use-package flycheck-clojure
  :after flycheck)

(use-package aggressive-indent
  :ghook
  user/lisp-mode-hooks)

(use-package lispy
  :after hydra
  :init
  (setq lispy-avy-keys '(?a ?o ?e ?u ?i ?d ?h ?t ?n ?s)
        lispy-colon-p nil
        lispy-close-quotes-at-end-p t
        lispy-insert-space-after-wrap nil
        lispy-visit-method 'projectile
        lispy-avy-style-char 'at-full
        lispy-avy-style-paren 'at-full
        lispy-avy-style-symbol 'at-full)
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
    ("Q" lispy-convolute-left "eval other window")
    ("x" hydra-lispy-x/body "x mode")
    ("o" lispy-other-mode "o mode"))

  (defun user/lispy-view ()
    "Recenter current sexp."
    (interactive)
    (lispy-from-left (recenter)))

  (defun user/lispy-flow-backward (arg)
    (interactive "p")
    (lispy-flow (- arg)))

  (general-add-advice
   'lispy-flow
   :override
   (defun user/lispy-flow (arg)
     "Move inside list ARG times.
Don't enter strings or comments.
Return nil if can't move."
     (interactive "p")
     (lispy--remember)
     (let ((pt (point))
           r)
       (cond
        ((and (lispy-bolp)
              (looking-at (lispy-comment-char)))
         (setq r (lispy--re-search-in-code lispy-left 'forward arg)))
        ((lispy-left-p)
         (setq r (lispy--re-search-in-code lispy-left 'forward arg)))
        ((lispy-right-p)
         (backward-char)
         (when (setq r (lispy--re-search-in-code lispy-right 'forward (- arg)))
           (forward-char))))
       (or r
           (progn
             (goto-char pt)
             nil)))))

  :ghook
  user/lisp-mode-hooks
  :general
  ('lispy-mode-map
   :definer 'lispy
   "t" '(lispy-ace-paren
         :override '(cond ((bound-and-true-p view-mode)
                           (View-quit))))
   "f" 'lispy-ace-char
   "c" 'lispy-ace-symbol-replace
   "P" 'lispy-clone
   "C" 'lispy-kill
   "%" 'lispy-different
   "p" 'lispy-paste
   "y" 'lispy-new-copy
   "z" 'user/lispy-view
   "J" 'lispy-join
   "K" 'helpful-at-point
   "H" 'lispy-backward
   "L" 'lispy-forward
   ">" 'lispy-slurp-or-barf-right
   "<" 'lispy-slurp-or-barf-left
   "/" 'lispy-occur
   "w" 'lispy-flow
   "b" 'user/lispy-flow-backward
   "U" 'lispy-back
   "+" 'lispy-widen
   "-" 'lispy-narrow
   "g" 'user/lispy-g-hydra/body
   "S" 'lispy-move-up
   "Q" 'lispy-convolute
   "x" 'lispy-splice
   "T" '(lispy-teleport
         :override '(cond ((looking-at lispy-outline)
                           (end-of-line)))))

  ('lispy-mode-map
   "TAB" 'lispy-tab
   "<C-return>" 'lispy-out-forward-newline
   "]" 'lispy-close-square
   "}" 'lispy-close-curly))

(use-package lispyville
  :after lispy
  :init
  (setq lispyville-motions-put-into-special nil
        lispyville-commands-put-into-special nil)
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
    (when (lispyville--at-left-p)
      (forward-char))
    (call-interactively 'lispyville-insert-at-end-of-list)
    (unless (save-excursion (backward-char) (lispyville--at-left-p))
      (insert " ")))

  (defun user/lispy-insert-at-beginning-of-list ()
    "Backward list and enter insert state."
    (interactive)
    (when (lispyville--at-left-p)
      (forward-char))
    (call-interactively 'lispyville-insert-at-beginning-of-list)
    (unless (lispyville--at-right-p)
      (insert " ")
      (backward-char)))

  (defun user/lispy-space ()
    "Like lispy space, but move the cursor back once."
    (interactive)
    (if (not (lispyville--at-left-p))
        (call-interactively 'lispy-space)
      (call-interactively 'lispy-space)
      (backward-char)
      (when (lispyville--at-left-p)
        (forward-char))))

  (defun user/lispy-open-below ()
    (interactive)
    (if (not (lispyville--at-left-p))
        (call-interactively 'lispy-out-forward-newline)
      (forward-char)
      (call-interactively 'lispy-out-forward-newline)))

  (defun user/lispy-open-above ()
    (interactive)
    (if (not (lispyville--at-left-p))
        (call-interactively 'lispyville-open-above-list)
      (forward-char)
      (call-interactively 'lispyville-open-above-list)))

  (defun user/eval-sexp-fu-eval-sexp-inner-sexp-dwim ()
    (interactive)
    (if (eq major-mode 'emacs-lisp-mode)
        (call-interactively 'eval-sexp-fu-eval-sexp-inner-sexp)
      (call-interactively 'eval-sexp-fu-cider-eval-sexp-inner-sexp)))

  (defun user/eval-defun-dwim ()
    (interactive)
    (cond ((eq major-mode 'emacs-lisp-mode)
           (call-interactively 'eval-defun))
          ((eq major-mode 'clojure-mode)
           (call-interactively 'cider-eval-defun-at-point))))

  (defun user/lispy-delete-char-or-splice ()
    (interactive)
    (call-interactively 'lispyville-delete-char-or-splice)
    (unless (evil-normal-state-p)
      (unless (or (bolp) (eolp))
        (forward-char))
      (evil-normal-state)))

  (defun user/lispy-delete-char-or-splice-backwards ()
    (interactive)
    (call-interactively 'lispyville-delete-char-or-splice-backwards)
    (unless (evil-normal-state-p)
      (unless (or (bolp) (eolp))
        (forward-char))
      (evil-normal-state)))

  (general-add-advice
   '(lispyville-forward-atom-end lispyville-backward-atom-end)
   :around (lambda (f arg)
             (unless (evil-operator-state-p)
               (forward-char))
             (funcall f arg)
             (unless (evil-operator-state-p)
               (backward-char))))

  (defun user/lispy-parens-wrap ()
    (interactive)
    (if (lispyville--at-left-p)
        (call-interactively 'lispy-wrap-round)
      (call-interactively 'lispy-parens)))

  (defun user/lispy-brackets-wrap ()
    (interactive)
    (if (lispyville--at-left-p)
        (call-interactively 'lispy-wrap-brackets)
      (call-interactively 'lispy-brackets)))

  (defun user/lispy-braces-wrap ()
    (interactive)
    (if (lispyville--at-left-p)
        (call-interactively 'lispy-wrap-braces)
      (call-interactively 'lispy-braces)))

  (defun user/current-line-empty-p ()
    (string-match-p "\\`\\s-*$" (thing-at-point 'line)))

  (defun user/kill-empty-line ()
    (when (user/current-line-empty-p)
      (delete-region (line-beginning-position) (1+ (line-end-position)))))

  (defun user/lispy-kill-and-back ()
    (interactive)
    (call-interactively 'lispy-kill)
    (user/kill-empty-line)
    (user/kill-empty-line)
    (call-interactively 'lispy-backward)
    (call-interactively 'lispy-tab))

  (defun user/lispy-kill-and-insert ()
    (interactive)
    (call-interactively 'lispy-kill)
    (evil-insert-state))

  (general-add-advice
   '(lispyville-backward-up-list lispyville-up-list)
   :after (lambda (_) (unless (evil-insert-state-p) (evil-insert-state))))

  :ghook
  'lispy-mode-hook
  :general
  ('lispy-mode-map
   "C-9" 'lispyville-backward-up-list
   "C-0" 'lispyville-up-list
   "(" 'user/lispy-parens-wrap
   "[" 'user/lispy-brackets-wrap
   "{" 'user/lispy-braces-wrap
   "SPC" 'user/lispy-space)
  ('lispy-mode-map
   :definer 'lispy
   "v" 'lispyville-toggle-mark-type
   "o" 'user/lispy-open-below
   "O" 'user/lispy-open-above
   "A" 'user/lispy-insert-at-end-of-list
   "D" 'user/lispy-kill-and-back
   "I" 'user/lispy-insert-at-beginning-of-list
   "e" 'user/eval-sexp-fu-eval-sexp-inner-sexp-dwim
   "E" 'user/eval-defun-dwim)
  ('(normal visual)
   'lispyville-mode-map
   "gC" 'user/lispy-kill-and-insert
   "gD" 'user/lispy-kill-and-back
   "gr" 'lispy-raise-sexp
   "gs" 'lispyville-drag-forward
   "gS" 'lispyville-drag-backward
   "g(" 'lispyville-wrap-round
   "g[" 'lispyville-wrap-brackets
   "g{" 'lispyville-wrap-braces
   "ze" 'user/eval-sexp-fu-eval-sexp-inner-sexp-dwim
   "zE" 'user/eval-defun-dwim)
  ('normal
   'lispyville-mode-map
   "gb" 'lispy-split
   "gQ" 'lispy-convolute-sexp
   "gI" 'user/lispy-insert-at-beginning-of-list
   "gA" 'user/lispy-insert-at-end-of-list
   "go" 'lispyville-open-below-list
   "gO" 'lispyville-open-above-list
   "x" 'user/lispy-delete-char-or-splice
   "X" 'user/lispy-delete-char-or-splice-backwards))

(use-package eval-sexp-fu
  :after lispy
  :custom-face
  (eval-sexp-fu-flash ((t (:inherit isearch))))
  (eval-sexp-fu-flash-error ((t (:inherit error :inverse-video t)))))

(use-package cider
  :after (flycheck evil-collection)
  :init
  (setq cider-repl-pop-to-buffer-on-connect nil)
  :config
  (flycheck-clojure-setup)
  (evil-collection-init 'cider)
  :general
  ("C-c c c" 'cider-jack-in
   "C-c c q" 'cider-quit))

(use-package cider-eval-sexp-fu
  :after cider lispy)

;;;  python

(use-package python
  :after evil-collection
  :mode
  ("\\.py\\'" . python-mode)
  :interpreter
  ("python3" . python-mode)
  :commands python-mode
  :preface
  (setenv "IPY_TEST_SIMPLE_PROMPT" "1")
  :init
  (setq
   python-shell-interpreter "ipython3"
   python-shell-interpreter-args "--deep-reload"
   python-indent-guess-indent-offset nil)
  :config
  (evil-collection-init 'python)
  :general
  ('python-mode-map
   "DEL" nil)                           ; interferes with smartparens
  :gfhook
  #'eldoc-mode
  #'flycheck-mode
  #'smartparens-mode)

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
  (rtog/add-repl 'python-mode (lambda () (run-python python-shell-interpreter t t))))

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
  :after evil-collection
  :config
  (evil-collection-init 'alchemist)
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
  :after (flycheck evil-collection)
  :mode
  ("\\.js\\'")
  :gfhook
  #'flycheck-mode
  :init
  (setq js2-strict-trailing-comma-warning nil)
  :config
  (evil-collection-init 'js2-mode)
  :custom-face
  (js2-error ((t (:inherit font-lock-warning-face))))
  (js2-function-param ((t (:inherit font-lock-variable-name-face))))
  (js2-external-variable ((t (:inherit font-lock-constant-face)))))

(use-package rjsx-mode
  :after (flycheck evil-collection)
  :mode
  ("\\.jsx\\'")
  :config
  (evil-collection-init 'rjsx-mode)
  :gfhook
  #'flycheck-mode)

(use-package jest
  :after (js2-mode rjsx-mode)
  :ghook
  user/js-mode-hooks)

(use-package prettier-js
  :ghook
  user/js-mode-hooks)

(use-package smartparens
  :ghook
  user/js-mode-hooks)

(use-package smartparens-config
  :straight nil)

;; FIXME: for some reason this makes identifiers in js green now
(use-package tide
  :disabled t
  :after evil-collection
  :init
  (setq tide-filter-out-warning-completions t)
  :config
  (evil-collection-init 'tide)
  (flycheck-add-next-checker 'javascript-eslint 'javascript-tide 'append)
  :ghook
  (user/js-mode-hooks (lambda ()
                        (tide-setup)
                        (tide-format-before-save)
                        (tide-hl-identifier-mode 1))))

(use-package add-node-modules-path
  :ghook
  (user/js-mode-hooks #'add-node-modules-path))

(use-package indium
  :after evil-collection
  :config
  (evil-collection-init 'indium))

;; FIXME: need to add some real databases
(use-package sql
  :disabled t
  :gfhook
  ('sql-interactive-mode-hook (lambda () (toggle-truncate-lines t)))
  :general
  (:states '(normal visual)
   :prefix user/leader-key
   "dd" 'user/sql-connect
   "dn" 'user/sql-new-connect)
  ('(normal insert)
   'sql-interactive-mode-map
   "C-n" #'comint-next-input
   "C-p" #'comint-previous-input))

(use-package restclient
  :after evil-collection
  :mode
  ("\\.http\\'")
  :config
  (evil-collection-init 'restclient))

(use-package ob-restclient
  :after (org restclient)
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((restclient . t))))

(use-package company-restclient
  :after (restclient company)
  :config
  (add-to-list 'company-backends 'company-restclient))

(use-package lua-mode)
(use-package typescript-mode)

(use-package indicators)
(use-package sublimity)
(use-package yascroll)

(use-package yasnippet
  :after (company company-restclient)
  :config
  ;; Add yasnippet support for all company backends
  ;; https://github.com/syl20bnr/spacemacs/pull/179
  (defun user/company-mode-backend-with-yas (backend)
    (if (and (listp backend) (member 'company-yasnippet backend))
        backend
      (append (if (consp backend) backend (list backend))
              '(:with company-yasnippet))))

  (setq company-backends (mapcar #'user/company-mode-backend-with-yas company-backends))

  (general-add-advice 'company-tng--supress-post-completion :override #'ignore)

  (yas-global-mode)
  :general
  ('yas-keymap
   "TAB" '(yas-next-field :predicate (not company-my-keymap))
   "S-<tab>" '(yas-prev-field :predicate (not company-my-keymap))))

(use-package yasnippet-snippets
  :after yasnippet)

(use-package dockerfile-mode)
(use-package wgrep)
(use-package highlight2clipboard)

(use-package ialign)

(use-package logview)

(use-package esup
  :init
  (setq esup-depth 0))

(use-package restart-emacs
  :general
  (:states 'normal
   "ZR" 'restart-emacs))

(use-package persistent-scratch
  :init
  (setq persistent-scratch-scratch-buffer-p-function
        (lambda () (string-prefix-p "*scratch*" (buffer-name))))
  :config
  (persistent-scratch-setup-default))

(fset 'yes-or-no-p 'y-or-n-p)
(savehist-mode 1)
(global-auto-revert-mode 1)
(minibuffer-depth-indicate-mode 1)

(general-add-hook 'before-save-hook 'delete-trailing-whitespace)
(general-add-hook 'hack-local-variables-hook (lambda () (setq truncate-lines t)))
(general-add-hook 'find-file-hook 'user/check-large-file)

(defun display-startup-echo-area-message ()
  (message (emacs-init-time)))

(persp-state-load persp-state-default-file)

;;; init.el ends here
