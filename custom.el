(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(clojure-indent-style :always-indent)
 '(counsel-find-file-ignore-regexp nil)
 '(custom-safe-themes
   (quote
    ("9be1d34d961a40d94ef94d0d08a364c3d27201f3c98c9d38e36f10588469ea57" default)))
 '(eshell-output-filter-functions
   (quote
    (eshell-postoutput-scroll-to-bottom eshell-handle-control-codes eshell-handle-ansi-color)))
 '(flycheck-display-errors-function (quote flycheck-display-error-messages))
 '(git-gutter:window-width -1)
 '(global-hl-todo-mode t)
 '(hl-todo-keyword-faces
   (quote
    (("HOLD" . "#ffcc66")
     ("TODO" . "#cc99cc")
     ("NEXT" . "#ffcc66")
     ("THEM" . "#ffcc66")
     ("PROG" . "#ffcc66")
     ("OKAY" . "#99cc99")
     ("DONT" . "#f2777a")
     ("FAIL" . "#f2777a")
     ("DONE" . "#afd8af")
     ("NOTE" . "#ffcc66")
     ("KLUDGE" . "#f2777a")
     ("HACK" . "#f2777a")
     ("FIXME" . "#f2777a")
     ("XXX" . "#f2777a")
     ("XXXX" . "#f2777a")
     ("???" . "#f99157"))))
 '(js-indent-level 2)
 '(js2-strict-trailing-comma-warning nil)
 '(package-selected-packages
   (quote
    (package-build shut-up epl git commander dash yascroll prodigy eshell-prompt-extras hl-todo rainbow-delimiters eval-sexp-fu f s evil-snipe evil-surround evil-nerd-commenter persp-projectile counsel-projectile swiper general evil-cleverparens spaceline telephone-line use-package powerline evil base16-theme)))
 '(prettier-js-args (quote ("--single-quote" "--trailing-comma" "all")))
 '(prettier-js-command "prettier")
 '(projectile-completion-system (quote ivy))
 '(projectile-enable-caching nil)
 '(projectile-file-exists-local-cache-expire 30)
 '(projectile-globally-ignored-directories
   (quote
    (".idea" ".ensime_cache" ".eunit" ".git" ".hg" ".fslckout" "_FOSSIL_" ".bzr" "_darcs" ".tox" ".svn" ".stack-work" "node_modules")))
 '(python-indent-guess-indent-offset nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#2d2d2d" :foreground "#d3d0c8" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 140 :width condensed :foundry "nil" :family "Anonymous Pro"))))
 '(avy-lead-face ((t (:background "#f2777a" :foreground "#2d2d2d"))))
 '(avy-lead-face-0 ((t (:background "#6699cc" :foreground "#2d2d2d"))))
 '(avy-lead-face-1 ((t (:background "#747369" :foreground "#2d2d2d"))))
 '(avy-lead-face-2 ((t (:background "#cc99cc" :foreground "#2d2d2d"))))
 '(clojure-keyword-face ((t (:inherit font-lock-keyword-face))))
 '(company-tooltip-common ((t (:inherit company-tooltip :weight bold :underline nil))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold :underline nil))))
 '(diff-hl-change ((t (:foreground "#ffcc66"))))
 '(diff-hl-delete ((t (:foreground "#f2777a"))))
 '(diff-hl-insert ((t (:foreground "#99cc99"))))
 '(elixir-atom-face ((t (:inherit font-lock-constant-face))))
 '(elixir-attribute-face ((t (:inherit font-lock-keyword-face))))
 '(eshell-ls-archive ((t (:foreground "#cc99cc"))))
 '(eshell-ls-backup ((t (:foreground "#ffcc66"))))
 '(eshell-ls-clutter ((t (:foreground "#f99157"))))
 '(eshell-ls-directory ((t (:foreground "#6699cc"))))
 '(eshell-ls-executable ((t (:foreground "#99cc99"))))
 '(eshell-ls-missing ((t (:foreground "#f2777a"))))
 '(eshell-ls-product ((t (:foreground "#ffcc66"))))
 '(eshell-ls-readonly ((t (:foreground "#a09f93"))))
 '(eshell-ls-special ((t (:foreground "#cc99cc"))))
 '(eshell-ls-symlink ((t (:foreground "#66cccc"))))
 '(eshell-ls-unreadable ((t (:foreground "#515151"))))
 '(eshell-prompt ((t (:foreground "#cc99cc" :weight bold))))
 '(eval-sexp-fu-flash ((t (:background "#6699cc" :foreground "#f2f0ec" :weight bold))))
 '(eval-sexp-fu-flash-error ((t (:foreground "#f2777a" :weight bold))))
 '(flycheck-error-list-info ((t (:inherit success))))
 '(fringe ((t (:background "#2d2d2d" :foreground "#747369"))))
 '(git-gutter:added ((t (:inherit default :foreground "#99cc99" :weight bold))))
 '(git-gutter:deleted ((t (:foreground "#f2777a" :weight bold))))
 '(git-gutter:modified ((t (:inherit default :foreground "#ffcc66" :weight bold))))
 '(hl-todo ((t (:foreground "#cc99cc" :weight bold))))
 '(js2-error ((t (:inherit font-lock-warning-face))))
 '(js2-external-variable ((t (:inherit font-lock-warning-face))))
 '(js2-function-param ((t (:inherit font-lock-variable-name-face))))
 '(js2-instance-member ((t (:inherit font-lock-function-name-face))))
 '(js2-jsdoc-html-tag-delimiter ((t (:inherit font-lock-negation-char-face))))
 '(js2-jsdoc-html-tag-name ((t (:inherit font-lock-keyword-face))))
 '(js2-jsdoc-tag ((t (:inherit font-lock-variable-name-face))))
 '(js2-jsdoc-type ((t (:inherit font-lock-constant-face))))
 '(js2-jsdoc-value ((t (:inherit font-lock-function-name-face))))
 '(js2-private-function-call ((t (:inherit font-lock-function-name-face))))
 '(js2-private-member ((t (:inherit font-lock-variable-name-face))))
 '(linum ((t (:background "#2d2d2d" :foreground "#515151"))))
 '(magit-bisect-bad ((t (:foreground "#f2777a"))))
 '(magit-bisect-good ((t (:foreground "#99cc99"))))
 '(magit-bisect-skip ((t (:foreground "#d27b53"))))
 '(magit-blame-heading ((t (:background "#515151" :foreground "#a09f93"))))
 '(magit-branch-local ((t (:foreground "#6699cc"))))
 '(magit-branch-remote ((t (:foreground "#99cc99"))))
 '(magit-cherry-equivalent ((t (:foreground "#cc99cc"))))
 '(magit-cherry-unmatched ((t (:foreground "#66cccc"))))
 '(magit-diff-added ((t (:foreground "#99cc99"))))
 '(magit-diff-added-highlight ((t (:background "#515151" :foreground "#99cc99"))))
 '(magit-diff-base ((t (:foreground "#ffcc66"))))
 '(magit-diff-base-highlight ((t (:background "#515151" :foreground "#ffcc66"))))
 '(magit-diff-lines-heading ((t (:inherit magit-diff-hunk-heading-highlight :background "#515151" :foreground "#d27b53"))))
 '(magit-diff-removed ((t (:foreground "#f2777a"))))
 '(magit-diff-removed-highlight ((t (:background "#515151" :foreground "#f2777a"))))
 '(magit-diffstat-added ((t (:foreground "#99cc99"))))
 '(magit-diffstat-removed ((t (:foreground "#f2777a"))))
 '(magit-dimmed ((t (:inherit font-lock-comment-face))))
 '(magit-hash ((t (:inherit font-lock-comment-face))))
 '(magit-log-author ((t (:foreground "#f2777a"))))
 '(magit-log-date ((t (:inherit font-lock-comment-face))))
 '(magit-log-graph ((t (:inherit font-lock-comment-face))))
 '(magit-process-ng ((t (:inherit magit-section-heading :foreground "#f2777a"))))
 '(magit-process-ok ((t (:inherit magit-section-heading :foreground "#99cc99"))))
 '(magit-reflog-amend ((t (:foreground "#cc99cc"))))
 '(magit-reflog-checkout ((t (:foreground "#6699cc"))))
 '(magit-reflog-cherry-pick ((t (:foreground "#99cc99"))))
 '(magit-reflog-commit ((t (:foreground "#99cc99"))))
 '(magit-reflog-merge ((t (:foreground "#99cc99"))))
 '(magit-reflog-other ((t (:foreground "#66cccc"))))
 '(magit-reflog-rebase ((t (:foreground "#cc99cc"))))
 '(magit-reflog-remote ((t (:foreground "#66cccc"))))
 '(magit-reflog-reset ((t (:foreground "#f2777a"))))
 '(magit-refname ((t (:inherit default))))
 '(magit-section-heading ((t (:foreground "#ffcc66" :weight bold))))
 '(magit-section-heading-selection ((t (:foreground "#d27b53"))))
 '(magit-section-highlight ((t (:background "#515151"))))
 '(magit-sequence-drop ((t (:foreground "#f2777a"))))
 '(magit-sequence-head ((t (:foreground "#6699cc"))))
 '(magit-sequence-part ((t (:foreground "#ffcc66"))))
 '(magit-sequence-stop ((t (:foreground "#99cc99"))))
 '(magit-signature-bad ((t (:foreground "#f2777a"))))
 '(magit-signature-good ((t (:foreground "#99cc99"))))
 '(magit-signature-untrusted ((t (:foreground "#66cccc"))))
 '(magit-tag ((t (:foreground "#ffcc66"))))
 '(prodigy-green-face ((t (:foreground "#99cc99"))))
 '(prodigy-red-face ((t (:foreground "#f2777a"))))
 '(prodigy-yellow-face ((t (:foreground "#ffcc66"))))
 '(scroll-bar ((t (:background "#2d2d2d"))))
 '(spacemacs-iedit-face ((t (:background "#f2777a" :foreground "#515151" :inherit (quote mode-line)))))
 '(spacemacs-iedit-insert-face ((t (:background "f2777a" :foreground "#515151" :inherit (quote mode-line)))))
 '(vertical-border ((t (:foreground "#515151"))))
 '(vi-tilde-fringe-face ((t (:foreground "#515151"))))
 '(window-divider ((t (:foreground "#515151")))))
