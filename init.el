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
(straight-use-package 'general)

;;

(require 'use-package)
(require 'general)

(use-package f)
(use-package s)
(use-package dash)

;; (straight-use-package 'f)
;; (straight-use-package 's)
;; (straight-use-package 'dash)
;; (straight-use-package 'use-package)
(straight-use-package 'base16-theme)
(straight-use-package 'linum)
(straight-use-package 'powerline)
(straight-use-package 'smartparens)
(straight-use-package 'projectile)
(straight-use-package 'ivy)
(straight-use-package 'counsel)
(straight-use-package 'counsel-projectile)
(straight-use-package 'swiper)
(straight-use-package 'perspective)
(straight-use-package 'persp-projectile)
(straight-use-package 'multi-eshell)
(straight-use-package 'evil)
(straight-use-package 'evil-snipe)
(straight-use-package 'evil-surround)
(straight-use-package 'evil-cleverparens)
(straight-use-package 'evil-commentary)
(straight-use-package 'evil-collection)
;; (straight-use-package 'evil-easymotion)
;; (straight-use-package 'evil-easymotion
;;            :git 'git@github.com:pythonnut/evil-easymotion.git
;;            :ref '8515834580f948021d0e9389f42c6e9f04ccb17a)
(straight-use-package 'exec-path-from-shell)
(straight-use-package 'company)
;; (straight-use-package 'eval-sexp-fu
;;             :git 'git@github.com:danielschranz/eval-sexp-fu.el.git)
(straight-use-package 'eval-sexp-fu)
(straight-use-package 'magit)
(straight-use-package 'evil-magit)
(straight-use-package 'git-gutter)
(straight-use-package 'git-gutter-fringe)
(straight-use-package 'git-messenger)
(straight-use-package 'gitconfig-mode)
(straight-use-package 'gitignore-mode)
(straight-use-package 'browse-at-remote)
(straight-use-package 'shackle)
(straight-use-package 'fringe-helper)
(straight-use-package 'flycheck)
(straight-use-package 'flycheck-tip)
(straight-use-package 'flycheck-pos-tip)
(straight-use-package 'flycheck-popup-tip)
(straight-use-package 'flycheck-clojure)
(straight-use-package 'flycheck-credo)
(straight-use-package 'hl-line)
(straight-use-package 'spinner)
(straight-use-package 'rainbow-delimiters)
(straight-use-package 'hl-todo)
(straight-use-package 'eshell-prompt-extras)
(straight-use-package 'prodigy)
(straight-use-package 'indicators)
(straight-use-package 'yascroll)
(straight-use-package 'sublimity)
(straight-use-package 'quickrun)
(straight-use-package 'cider)
(straight-use-package 'cider-eval-sexp-fu)
(straight-use-package 'which-key)
(straight-use-package 'anaconda-mode)
(straight-use-package 'company-anaconda)
(straight-use-package 'nose)
(straight-use-package 'pip-requirements)
(straight-use-package 'repl-toggle)
(straight-use-package 'dockerfile-mode)
(straight-use-package 'js2-mode)
(straight-use-package 'rjsx-mode)
(straight-use-package 'elixir-mode)
(straight-use-package 'alchemist)
(straight-use-package 'rbenv)
(straight-use-package 'lua-mode)
(straight-use-package 'wgrep)
(straight-use-package 'prettier-js)
(straight-use-package 'xterm-color)
;; (straight-use-package 'heroku)
(straight-use-package 'mmm-mode)
(straight-use-package 'web-mode)
(straight-use-package 'polymode)
;; (straight-use-package 'fence-edit)
(straight-use-package 'typescript-mode)
(straight-use-package 'tide)
(straight-use-package 'highlight2clipboard)
(straight-use-package 'indium)
(straight-use-package 'vterm)
;; (straight-use-package 'undo-fu)
;; (straight-use-package 'undo-fu-session)
;; (straight-use-package 'lsp-mode)
;; (straight-use-package 'lsp-ivy)
;; (straight-use-package 'add-node-modules-path)
(straight-use-package 'el-patch)
(straight-use-package 'lispy)
(straight-use-package 'lispyville)
;; (straight-use-package '(targets :type git :host github :repo "noctuid/targets.el"))
(straight-use-package 'org)
(straight-use-package 'evil-org)
(straight-use-package 'focus)
(straight-use-package 'prescient)
(straight-use-package 'ivy-prescient)
(straight-use-package 'olivetti)
(straight-use-package 'markdown-mode)
(straight-use-package 'shx)
(straight-use-package 'dumb-jump)
(straight-use-package 'aggressive-indent)
;; (straight-use-package 'evil-mc)
;; (straight-use-package 'evil-multiedit

(use-package undo-fu)

(use-package undo-fu-session
  :config
  (global-undo-fu-session-mode))

;;

(declare-function user/projectile-switch-to-project-file "user.core")

;; (add-to-list 'load-path (expand-file-name "user" user-emacs-directory))

;;(require 'user.core)

(dolist (p '(
             "user/core"
             "user/ui"
             "user/evil"
             "user/git"
             "user/completion"
             "user/mode-line"
             "user/shell"
             "user/languages"
             "user/services"
             "user/database"
             "user/bindings"))
  (load-library (concat user-emacs-directory p)))

(user/projectile-switch-to-project-file user-emacs-directory "README.org")

;;; init.el ends here
