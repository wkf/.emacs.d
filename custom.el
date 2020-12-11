(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(clojure-indent-style :always-indent)
 '(counsel-find-file-ignore-regexp nil)
 '(css-indent-offset 2)
 '(custom-safe-themes
   '("9be1d34d961a40d94ef94d0d08a364c3d27201f3c98c9d38e36f10588469ea57" default))
 '(eshell-output-filter-functions
   '(eshell-postoutput-scroll-to-bottom eshell-handle-control-codes eshell-handle-ansi-color))
 '(flycheck-display-errors-function 'flycheck-display-error-messages)
 '(flycheck-popup-tip-error-prefix "")
 '(git-gutter:window-width -1)
 '(global-hl-todo-mode t)
 '(hl-todo-keyword-faces
   '(("HOLD" . "#ffcb6b")
     ("TODO" . "#c792ea")
     ("NEXT" . "#ffcb6b")
     ("THEM" . "#ffcb6b")
     ("PROG" . "#ffcb6b")
     ("OKAY" . "#c3e88d")
     ("DONT" . "#f07178")
     ("FAIL" . "#f07178")
     ("DONE" . "#c3e88d")
     ("NOTE" . "#ffcb6b")
     ("KLUDGE" . "#f07178")
     ("HACK" . "#f07178")
     ("FIXME" . "#f07178")
     ("XXX" . "#f07178")
     ("XXXX" . "#f07178")))
 '(js2-strict-trailing-comma-warning nil)
 '(package-selected-packages
   '(polymode package-build shut-up epl git commander dash yascroll prodigy eshell-prompt-extras hl-todo rainbow-delimiters eval-sexp-fu f s evil-snipe evil-surround evil-nerd-commenter persp-projectile counsel-projectile swiper general evil-cleverparens spaceline telephone-line use-package powerline evil base16-theme))
 '(prettier-js-args '("prettier" "--single-quote" "--trailing-comma" "all"))
 '(prettier-js-command "npx")
 '(projectile-completion-system 'ivy)
 '(projectile-enable-caching nil)
 '(projectile-file-exists-local-cache-expire 30)
 '(projectile-globally-ignored-directories
   '(".idea" ".ensime_cache" ".eunit" ".git" ".hg" ".fslckout" "_FOSSIL_" ".bzr" "_darcs" ".tox" ".svn" ".stack-work" "node_modules"))
 '(python-indent-guess-indent-offset nil)
 '(tide-filter-out-warning-completions t)
 '(web-mode-code-indent-offset 2)
 '(web-mode-css-indent-offset 2)
 '(web-mode-markup-indent-offset 2)
 '(web-mode-sql-indent-offset 2))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(clojure-keyword-face ((t (:inherit font-lock-keyword-face))))
 '(company-tooltip-common ((t (:inherit company-tooltip :weight bold :underline nil))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold :underline nil))))
 '(diff-hl-change ((t (:foreground "#ffcb6b"))))
 '(diff-hl-delete ((t (:foreground "#f07178"))))
 '(diff-hl-insert ((t (:foreground "#c3e88d"))))
 '(elixir-atom-face ((t (:inherit font-lock-constant-face))))
 '(elixir-attribute-face ((t (:inherit font-lock-keyword-face))))
 '(eshell-prompt ((t (:foreground "#c792ea" :weight bold))))
 '(eval-sexp-fu-flash ((t (:background "#82aaff" :foreground "#ffffff" :weight bold))))
 '(eval-sexp-fu-flash-error ((t (:foreground "#f07178" :weight bold))))
 '(flycheck-error-list-info ((t (:inherit success))))
 '(fringe ((t (:background "#212121"))))
 '(linum ((t (:foreground "#4a4a4a" :background "#212121"))))
 '(prodigy-green-face ((t (:foreground "#c3e88d"))))
 '(prodigy-red-face ((t (:foreground "#f07178"))))
 '(prodigy-yellow-face ((t (:foreground "#ffcb6b"))))
 '(tide-hl-identifier-face ((t (:inherit highlight)))))
