;;; user.languages --- language support
;;; Commentary:
;;; Code:

(require 'general)
(require 'eval-sexp-fu)
(require 'smartparens-config)
(require 'evil-cleverparens)
(require 'rainbow-delimiters)
(require 'cider)
(require 'cider-eval-sexp-fu)
(require 'flycheck)
(require 'flycheck-clojure)
;; (require 'flycheck-pos-tip)

(defvar user/lisp-mode-hooks
  '(emacs-lisp-mode-hook
    clojure-mode-hook
    clojurec-mode-hook
    clojurescript-mode-hook))

(general-add-hook user/lisp-mode-hooks #'eldoc-mode)
(general-add-hook user/lisp-mode-hooks #'rainbow-delimiters-mode)

(use-package lispy
  :init
  (general-add-hook user/lisp-mode-hooks (lambda () (lispy-mode 1))))

(use-package lispyville
  :after lispy
  :init
  (general-add-hook user/lisp-mode-hooks #'lispyville-mode)
  :config
  (progn
    (setq lispyville-motions-put-into-special t
          lispyville-commands-put-into-special t)
    (lispyville-enter-visual-when-marking)
    (lispyville-set-key-theme '(operators c-w c-u prettify text-objects commentary (atom-movement t) slurp/barf-lispy esc))
    (general-define-key
     :keymaps 'lispyville-mode-map
     :states 'normal
     "gI" 'lispyville-insert-at-beginning-of-list
     "gA" 'lispyville-insert-at-end-of-list
     "go" 'lispyville-open-below-list
     "gO" 'lispyville-open-above-list
     "g<" 'lispyville-drag-forward
     "g>" 'lispyville-drag-backward
     "gR" 'lispyville-raise-list
     "gr" 'lispy-raise-sexp
     "gJ" 'lispy-join
     "gs" 'lispy-split
     "gS" 'lispy-splice
     "gt" 'transpose-sexps
     "gC" 'lispy-convolute-sexp
     "g(" 'lispyville-wrap-round
     "g[" 'lispyville-wrap-brackets
     "g{" 'lispyville-wrap-braces
     "[{" 'lispyville-previous-opening
     "[}" 'lispyville-previous-closing
     "]{" 'lispyville-next-opening
     "]}" 'lispyville-next-closing
     "(" 'lispyville-backward-up-list
     ")" 'lispyville-up-list)))

 ;; (evil-set-initial-state 'cider-stacktrace-mode 'normal)
 ;; (evil-collection-define-key 'normal 'cider-stacktrace-mode-map
 ;;   (kbd "C-k") 'cider-stacktrace-previous-cause
 ;;   (kbd "C-j") 'cider-stacktrace-next-cause
 ;;   (kbd "gk") 'cider-stacktrace-previous-cause
 ;;   (kbd "gj") 'cider-stacktrace-next-cause
 ;;   (kbd "[[") 'cider-stacktrace-previous-cause
 ;;   (kbd "]]") 'cider-stacktrace-next-cause
 ;;   (kbd "C-p") 'cider-stacktrace-previous-cause
 ;;   (kbd "C-n") 'cider-stacktrace-next-cause
 ;;   "gd" 'cider-stacktrace-jump
 ;;   "q" 'cider-popup-buffer-quit-function
 ;;   "J" 'cider-stacktrace-toggle-java
 ;;   "C" 'cider-stacktrace-toggle-clj
 ;;   "R" 'cider-stacktrace-toggle-repl
 ;;   "T" 'cider-stacktrace-toggle-tooling
 ;;   "D" 'cider-stacktrace-toggle-duplicates
 ;;   "P" 'cider-stacktrace-show-only-project
 ;;   "A" 'cider-stacktrace-toggle-all
 ;;   "1" 'cider-stacktrace-cycle-cause-1
 ;;   "2" 'cider-stacktrace-cycle-cause-2
 ;;   "3" 'cider-stacktrace-cycle-cause-3
 ;;   "4" 'cider-stacktrace-cycle-cause-4
 ;;   "5" 'cider-stacktrace-cycle-cause-5
 ;;   "0" 'cider-stacktrace-cycle-all-causes
 ;;   (kbd "TAB") 'cider-stacktrace-cycle-current-cause
 ;;   [backtab] 'cider-stacktrace-cycle-all-causes)

;; (add-hook 'emacs-lisp-mode-hook (lambda () (lispy-mode 1)))

;; (sp-local-pair 'emacs-lisp-mode "\"" "\"" :actions ())

(setq cider-cljs-lein-repl
      "(do (require 'figwheel-sidecar.repl-api) (figwheel-sidecar.repl-api/cljs-repl))")

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

(use-package flycheck
  :config (progn
            ;; (flycheck-pos-tip-mode)
            ;; (add-hook 'flycheck-mode-hook 'flycheck-popup-tip-mode)
            (flycheck-credo-setup)
            (flycheck-clojure-setup)
            (global-flycheck-mode)
            (setq flycheck-indication-mode 'left-fringe
                  flycheck-display-errors-delay 0)
            (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
              [192 192 192 192 192 192 192 192 192 192 192 192 192 192 192 192 192 192 192 192 192 192 192 192 192]
              nil nil 'center)
            ;; HACK: redef this function to work around a bug trying to use a deleted buffer
            (if nil (defun flycheck-global-teardown ()
                      "Teardown Flycheck in all buffers."
                      (dolist (buffer (buffer-list))
                        (when (buffer-live-p buffer)
                          (with-current-buffer buffer
                            (when flycheck-mode
                              (flycheck-teardown)))))))))

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

(defun user/python-repl ()
  (run-python python-shell-interpreter t t))

(use-package repl-toggle
  :config (progn
            (rtog/add-repl 'python-mode 'user/python-repl)))

(use-package elixir-mode
  :init (progn
          (add-hook 'elixir-mode-hook 'smartparens-mode)
          (add-hook 'elixir-mode-hook
                    #'(lambda () (setq evil-shift-width 2)))))

(require 'tide)
(require 'prettier-js)

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;; (add-hook 'css-mode 'prettier-js-mode)
(add-hook 'js-mode-hook 'prettier-js-mode)
(add-hook 'js-mode-hook 'smartparens-mode)

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode 1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  ;; aligns annotation to the right hand side
  (setq company-tooltip-align-annotations t)
  ;; (eldoc-mode +1)
  (tide-format-before-save)
  (tide-hl-identifier-mode 1)
  (setq eldoc-idle-delay 0.1
        flycheck-display-errors-delay 1.5)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode 1))

;; (add-hook 'js2-mode-hook 'prettier-js-mode)
(add-hook 'js2-mode-hook 'smartparens-mode)
(add-hook 'js2-mode-hook #'setup-tide-mode)
;; configure javascript-tide checker to run after your default javascript checker
(flycheck-add-next-checker 'javascript-eslint 'javascript-tide 'append)
;; (add-to-list 'auto-mode-alist '("\\.jsx?$" . web-mode))

(add-hook 'rjsx-mode-hook 'prettier-js-mode)
(add-hook 'rjsx-mode-hook 'smartparens-mode)

;; (setq-default tab-width 2)
;; (setq-default indent-tabs-mode nil)
;; (setq-default js-indent-level 2)
;; (setq-default typescript-indent-level 2)
;; (setq-default web-mode-indent-style 2)
;; (setq-default web-mode-code-indent-offset 2)

;; (add-hook 'js2-mode-hook 'prettier-js-mode)
;; (add-hook 'web-mode-hook 'prettier-js-mode)
;; (add-hook 'json-mode-hook 'prettier-js-mode)
;; (add-hook 'tide-mode-hook 'prettier-js-mode)
;; (add-hook 'css-mode-hook 'prettier-js-mode)
;; (add-hook 'typescript-mode-hook 'prettier-js-mode)

;; Assign typescript-mode to .tsx files
;; (add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-mode))


;; (add-hook 'typescript-mode-hook #'setup-tide-mode)

;; ;; Assign typescript-mode to .tsx files
;; (add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-mode))
;; (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))

;; ;; Create submodules for multiple major modes
;; (require 'mmm-auto)
;; (setq mmm-global-mode 'maybe)
;; (setq mmm-submode-decoration-level 0) ;; Turn off background highlight

;; ;; Add css mode for CSS in JS blocks
;; (mmm-add-classes
;;   '((mmm-styled-mode
;;     :submode css-mode
;;     :front "[a-pr-zA-PR-Z0-9\(\)_]?[a-km-zA-KM-Z0-9\(\)<>(ul)_]+`\n"
;;     :back "`;")))

;; (mmm-add-mode-ext-class 'typescript-mode nil 'mmm-styled-mode)

;; ;; Add submodule for graphql blocks
;; (mmm-add-classes
;;   '((mmm-graphql-mode
;;     :submode graphql-mode
;;     :front "gr?a?p?h?ql`\n" ;; Add additional aliases like `gql` if needed
;;     :back "`;")))

;; (mmm-add-mode-ext-class 'typescript-mode nil 'mmm-graphql-mode)

;; ;; Add JSX submodule, because typescript-mode is not that great at it
;; (mmm-add-classes
;;   '((mmm-jsx-mode
;;     :front "\s\([\n<]"
;;     :back "[\s>]\);\n"
;;     :submode web-mode)))

;; (mmm-add-mode-ext-class 'typescript-mode nil 'mmm-jsx-mode)

;; (defun mmm-reapply ()
;;   (mmm-mode)
;;   (mmm-mode))

;; (add-hook 'after-save-hook 'mmm-reapply)



;; (use-package polymode
;;    :after (rjsx-mode web-mode)
;;    :config (progn
;;              (define-hostmode poly-web-hostmode :mode 'web-mode)

;;              (define-innermode poly-web-css-innermode
;;                :mode 'css-mode
;;                :head-matcher "[a-pr-zA-PR-Z0-9\(\)_]?[a-km-zA-KM-Z0-9\(\)<>(ul)_]+`\n"
;;                :tail-matcher "\`"
;;                :head-mode 'host
;;                :tail-mode 'host)

;;              (define-polymode poly-web-mode
;;                :hostmode 'poly-web-hostmode
;;                :innermodes '(poly-web-css-innermode))

;;              (add-to-list 'auto-mode-alist '("\\.jsx?$" . poly-web-mode))
;;              ;; (add-to-list 'auto-mode-alist '("\\.jsx?\\'" . poly-web-mode))
;;              ))

;; (use-package polymode
;;   :after rjsx-mode
;;   :config (progn
;;             (define-hostmode poly-rjsx-hostmode :mode 'rjsx-mode)

;;             (define-innermode poly-rjsx-css-innermode
;;               :mode 'css-mode
;;               :head-matcher "[a-pr-zA-PR-Z0-9\(\)_]?[a-km-zA-KM-Z0-9\(\)<>(ul)_]+\.?[^`]+`\n"
;;               :tail-matcher "`;"
;;               :head-mode 'host
;;               :tail-mode 'host)

;;             (define-polymode poly-rjsx-mode
;;               :hostmode 'poly-rjsx-hostmode
;;               :innermodes '(poly-rjsx-css-innermode))

;;             (add-to-list 'auto-mode-alist '("\\.jsx?\\'" . poly-rjsx-mode))

;;             ))

;; (use-package polymode
;;   :after typescript-mode
;;   :config (progn
;;             (define-hostmode poly-typescript-hostmode :mode 'typescript-mode)

;;             (define-innermode poly-typescript-css-innermode
;;               :mode 'css-mode
;;               :head-matcher "[a-pr-zA-PR-Z0-9\(\)_]?[a-km-zA-KM-Z0-9\(\)<>(ul)_]+\.?[^`]+`\n"
;;               :tail-matcher "`;"
;;               :head-mode 'host
;;               :tail-mode 'host)

;;             (define-innermode poly-typescript-web-innermode
;;               :mode 'web-mode
;;               :head-matcher "\s\([\n<]"
;;               :tail-matcher "[\s>]\);\n"
;;               :head-mode 'host
;;               :tail-mode 'host)

;;             (define-polymode poly-typescript-mode
;;               :hostmode 'poly-typescript-hostmode
;;               :innermodes '(poly-typescript-css-innermode poly-typescript-web-innermode))

;;             (add-to-list 'auto-mode-alist '("\\.tsx?\\'" . poly-typescript-mode))

;;             ))

(defun user/setup-typescript ()
  (interactive)

  (save-excursion
    (indent-region (point-min) (point-max) nil))

  (tide-setup)
  (flycheck-mode 1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode 1)
  (tide-format-before-save)
  (tide-hl-identifier-mode 1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode 1))

(use-package polymode
  :after web-mode
  :config (progn
            (define-hostmode poly-web-hostmode :mode 'web-mode)

            (define-innermode poly-web-css-innermode
              :mode 'css-mode
              :head-matcher "[a-pr-zA-PR-Z0-9\(\)_]?[a-km-zA-KM-Z0-9\(\)<>(ul)_]+\.?[^`]+`\n"
              :tail-matcher "`;"
              :head-mode 'host
              :tail-mode 'host)

            (define-polymode poly-web-mode
              :hostmode 'poly-web-hostmode
              :innermodes '(poly-web-css-innermode))

            (add-to-list 'auto-mode-alist '("\\.tsx?\\'" . poly-web-mode))))

(require 'css-mode)
(require 'web-mode)


(add-hook 'web-mode-hook 'prettier-js-mode)
(add-hook 'web-mode-hook 'smartparens-mode)

(add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "tsx" (file-name-extension buffer-file-name))
              (user/setup-typescript))))

(flycheck-add-mode 'typescript-tslint 'web-mode)


;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; formats the buffer before saving
;; (add-hook 'before-save-hook 'tide-format-before-save)

;; Assign typescript-mode to .tsx files
;; (add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-mode))
;; (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))

;; (require 'web-mode)
;; (require 'mmm-auto)
;; (setq mmm-global-mode 'maybe)
;; (setq mmm-submode-decoration-level 0) ;; Turn off background highlight

;; ;; Add css mode for CSS in JS blocks
;; (mmm-add-classes
;;   '((mmm-styled-mode
;;     :submode css-mode
;;     :front "[a-pr-zA-PR-Z0-9\(\)_]?[a-km-zA-KM-Z0-9\(\)<>(ul)_]+\.?[^`]+`\n"
;;     :back "`;")))

;; (mmm-add-mode-ext-class 'web-mode nil 'mmm-styled-mode)

;; (defun mmm-reapply ()
;;   (mmm-mode)
;;   (mmm-mode))

;; (add-hook 'after-save-hook 'mmm-reapply)

;; ;; Add submodule for graphql blocks
;; (mmm-add-classes
;;   '((mmm-graphql-mode
;;     :submode graphql-mode
;;     :front "gr?a?p?h?ql`\n" ;; Add additional aliases like `gql` if needed
;;     :back "`;")))

;; (mmm-add-mode-ext-class 'typescript-mode nil 'mmm-graphql-mode)

;; ;; Add JSX submodule, because typescript-mode is not that great at it
;; (mmm-add-classes
;;   '((mmm-jsx-mode
;;     :front "\s\([\n<]"
;;     :back "[\s>]\);\n"
;;     :submode web-mode)))

;; (mmm-add-mode-ext-class 'typescript-mode nil 'mmm-jsx-mode)

;; (defun user/mmm-reapply ()
;;   (mmm-mode)
;;   (mmm-mode))

;; (use-package mmm-mode
;;   :config (progn
;;             (require 'mmm-auto)

;;             (setq mmm-global-mode 'maybe)
;;             (setq mmm-submode-decoration-level 0) ;; Turn off background highlight

;;             (mmm-add-classes
;;              '((mmm-styled-mode
;;                 :submode css-mode
;;                 :front "[a-pr-zA-PR-Z0-9\(\)_]?[a-km-zA-KM-Z0-9\(\)<>(ul)_]+`\n"
;;                 :back "`;")))

;;             (mmm-add-mode-ext-class 'web-mode nil 'mmm-styled-mode)

;;             ;; ;; Add JSX submodule, because typescript-mode is not that great at it
;;             ;; (mmm-add-classes
;;             ;;  '((mmm-jsx-mode
;;             ;;     :front "\s\([\n<]"
;;             ;;     :back "[\s>]\);\n"
;;             ;;     :submode web-mode)))

;;             ;; (mmm-add-mode-ext-class 'typescript-mode nil 'mmm-jsx-mode)


;;             (add-hook 'after-save-hook 'user/mmm-reapply)))


;; (require 'mmm-auto)

;; (setq mmm-global-mode 'maybe)
;; (setq mmm-submode-decoration-level 0) ;; Turn off background highlight

;; (mmm-add-classes
;;   '((mmm-styled-mode
;;     :submode css-mode
;;     :front "[a-pr-zA-PR-Z0-9\(\)_]?[a-km-zA-KM-Z0-9\(\)<>(ul)_]+`\n"
;;     :back "`;")))

;; (mmm-add-mode-ext-class 'rjsx-mode nil 'mmm-styled-mode)

;; (defun user/mmm-reapply ()
;;   (mmm-mode)
;;   (mmm-mode))

;; (add-hook 'after-save-hook 'user/mmm-reapply)

(provide 'user.languages)
;;; languages.el ends here
