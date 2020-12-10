;;; user.ui --- user ui functions
;;; Commentary:
;;; Code:

(require 'base16-theme)
(require 'shackle)

(load-theme 'base16-material-darker t)

;; base00 - gray 1
;; base01 - gray 2
;; base02 - gray 3
;; base03 - gray 4
;; base04 - light blue
;; base05 - light cyan
;; base06 - light cyan
;; base07 - white
;; base08 - red
;; base09 - orange
;; base0A - yellow
;; base0B - green
;; base0C - cyan
;; base0D - blue
;; base0E - purple
;; base0F - bright red

(defvar user/red
  (plist-get base16-material-darker-colors :base08))

(defvar user/orange
  (plist-get base16-material-darker-colors :base09))

(defvar user/yellow
  (plist-get base16-material-darker-colors :base0A))

(defvar user/green
  (plist-get base16-material-darker-colors :base0B))

(defvar user/cyan
  (plist-get base16-material-darker-colors :base0C))

(defvar user/blue
  (plist-get base16-material-darker-colors :base0D))

(defvar user/purple
  (plist-get base16-material-darker-colors :base0E))

(defvar user/magenta
  (plist-get base16-material-darker-colors :base0E))

(defvar user/black
  (plist-get base16-material-darker-colors :base00))

(defvar user/white
  (plist-get base16-material-darker-colors :base05))

(defvar user/bright-red
  (plist-get base16-material-darker-colors :base09))

(defvar user/bright-yellow
  (plist-get base16-material-darker-colors :base02))

(defvar user/bright-green
  (plist-get base16-material-darker-colors :base01))

(defvar user/bright-cyan
  (plist-get base16-material-darker-colors :base0F))

(defvar user/pink
  (plist-get base16-material-darker-colors :base0F))

(defvar user/bright-blue
  (plist-get base16-material-darker-colors :base04))

(defvar user/bright-magenta
  (plist-get base16-material-darker-colors :base06))

(defvar user/bright-black
  (plist-get base16-material-darker-colors :base03))

(defvar user/light-black "#262626")

(defvar user/bright-white
  (plist-get base16-material-darker-colors :base07))

(custom-set-faces
 '(default ((t (:height 120 :family "Fira Code"))))
 ;; '(default ((t (:height 140 :family "Fira Code"))))
 ;; '(default ((t (:inherit nil))))
 '(clojure-keyword-face ((t (:inherit font-lock-keyword-face))))
 '(company-tooltip-common ((t (:inherit company-tooltip :weight bold :underline nil))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold :underline nil))))
 `(diff-hl-change ((t (:foreground ,user/yellow))))
 `(diff-hl-delete ((t (:foreground ,user/red))))
 `(diff-hl-insert ((t (:foreground ,user/green))))
 '(elixir-atom-face ((t (:inherit font-lock-constant-face))))
 '(elixir-attribute-face ((t (:inherit font-lock-keyword-face))))
 `(eshell-ls-archive ((t (:foreground ,user/purple))))
 `(eshell-ls-backup ((t (:foreground ,user/yellow))))
 `(eshell-ls-clutter ((t (:foreground ,user/bright-red))))
 `(eshell-ls-directory ((t (:foreground ,user/cyan))))
 `(eshell-ls-executable ((t (:foreground ,user/green))))
 `(eshell-ls-missing ((t (:foreground ,user/red))))
 `(eshell-ls-product ((t (:foreground ,user/yellow))))
 `(eshell-ls-readonly ((t (:foreground ,user/bright-blue))))
 `(eshell-ls-special ((t (:foreground ,user/magenta))))
 `(eshell-ls-symlink ((t (:foreground ,user/cyan))))
 `(eshell-ls-unreadable ((t (:foreground ,user/bright-yellow))))
 `(eshell-prompt ((t (:foreground ,user/purple :weight bold))))
 `(eval-sexp-fu-flash ((t (:background ,user/blue :foreground ,user/bright-white :weight bold))))
 `(eval-sexp-fu-flash-error ((t (:foreground ,user/red :weight bold))))
 '(flycheck-error-list-info ((t (:inherit success))))
 `(git-gutter:added ((t (:inherit default :foreground ,user/green :weight bold))))
 `(git-gutter:deleted ((t (:inherit default :foreground ,user/red :weight bold))))
 `(git-gutter:modified ((t (:inherit default :foreground ,user/yellow :weight bold))))
 `(git-gutter-fr:added ((t (:inherit default :foreground ,user/green :weight bold))))
 `(git-gutter-fr:deleted ((t (:inherit default :foreground ,user/red :weight bold))))
 `(git-gutter-fr:modified ((t (:inherit default :foreground ,user/yellow :weight bold))))
 ;; '(hl-todo ((t (:foreground ,user/purple :weight bold))))
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
 `(magit-bisect-bad ((t (:foreground ,user/red))))
 `(magit-bisect-good ((t (:foreground ,user/green))))
 `(magit-bisect-skip ((t (:foreground ,user/bright-cyan))))
 `(magit-blame-heading ((t (:background ,user/bright-yellow :foreground ,user/bright-blue))))
 `(magit-branch-local ((t (:foreground ,user/blue))))
 `(magit-branch-remote ((t (:foreground ,user/green))))
 `(magit-cherry-equivalent ((t (:foreground ,user/purple))))
 `(magit-cherry-unmatched ((t (:foreground ,user/cyan))))
 `(magit-diff-added ((t (:foreground ,user/green))))
 `(magit-diff-added-highlight ((t (:background ,user/bright-yellow :foreground ,user/green))))
 `(magit-diff-base ((t (:foreground ,user/yellow))))
 `(magit-diff-base-highlight ((t (:background ,user/bright-yellow :foreground ,user/yellow))))
 `(magit-diff-lines-heading ((t (:inherit magit-diff-hunk-heading-highlight :background ,user/bright-yellow :foreground ,user/bright-cyan))))
 `(magit-diff-removed ((t (:foreground ,user/red))))
 `(magit-diff-removed-highlight ((t (:background ,user/bright-yellow :foreground ,user/red))))
 `(magit-diffstat-added ((t (:foreground ,user/green))))
 `(magit-diffstat-removed ((t (:foreground ,user/red))))
 `(magit-dimmed ((t (:inherit font-lock-comment-face))))
 `(magit-hash ((t (:inherit font-lock-comment-face))))
 `(magit-log-author ((t (:foreground ,user/red))))
 '(magit-log-date ((t (:inherit font-lock-comment-face))))
 '(magit-log-graph ((t (:inherit font-lock-comment-face))))
 `(magit-process-ng ((t (:inherit magit-section-heading :foreground ,user/red))))
 `(magit-process-ok ((t (:inherit magit-section-heading :foreground ,user/green))))
 `(magit-reflog-amend ((t (:foreground ,user/purple))))
 `(magit-reflog-checkout ((t (:foreground ,user/blue))))
 `(magit-reflog-cherry-pick ((t (:foreground ,user/green))))
 `(magit-reflog-commit ((t (:foreground ,user/green))))
 `(magit-reflog-merge ((t (:foreground ,user/green))))
 `(magit-reflog-other ((t (:foreground ,user/cyan))))
 `(magit-reflog-rebase ((t (:foreground ,user/purple))))
 `(magit-reflog-remote ((t (:foreground ,user/cyan))))
 `(magit-reflog-reset ((t (:foreground ,user/red))))
 '(magit-refname ((t (:inherit default))))
 `(magit-section-heading ((t (:foreground ,user/yellow :weight bold))))
 `(magit-section-heading-selection ((t (:foreground ,user/bright-cyan))))
 `(magit-section-highlight ((t (:background ,user/bright-yellow))))
 `(magit-sequence-drop ((t (:foreground ,user/red))))
 `(magit-sequence-head ((t (:foreground ,user/blue))))
 `(magit-sequence-part ((t (:foreground ,user/yellow))))
 `(magit-sequence-stop ((t (:foreground ,user/green))))
 `(magit-signature-bad ((t (:foreground ,user/red))))
 `(magit-signature-good ((t (:foreground ,user/green))))
 `(magit-signature-untrusted ((t (:foreground ,user/cyan))))
 `(magit-tag ((t (:foreground ,user/yellow))))
 `(prodigy-green-face ((t (:foreground ,user/green))))
 `(prodigy-red-face ((t (:foreground ,user/red))))
 `(prodigy-yellow-face ((t (:foreground ,user/yellow))))
 ;; '(scroll-bar ((t (:background "#2d2d2d"))))
 ;; '(spacemacs-iedit-face ((t (:background ,user/red :foreground ,user/bright-yellow :inherit (quote mode-line)))))
 ;; '(spacemacs-iedit-insert-face ((t (:background "f2777a" :foreground ,user/bright-yellow :inherit (quote mode-line)))))
 ;; '(vertical-border ((t (:foreground ,user/bright-yellow))))
 ;; '(vi-tilde-fringe-face ((t (:foreground ,user/bright-yellow))))
 ;; '(window-divider ((t (:foreground ,user/bright-yellow))))
 `(linum ((t (:foreground ,user/bright-yellow :background ,user/black :underline nil))))
 `(fringe ((t (:foreground ,user/bright-black :background ,user/black :underline nil))))
 `(region ((t (:background ,user/bright-black))))
 `(hl-line ((t (:background ,user/light-black))))
 `(org-todo ((t (:background ,user/black))))
 `(org-done ((t (:background ,user/black :foreground ,user/bright-black))))
 `(avy-lead-face-0 ((t (:background ,user/yellow :foreground ,user/black))))
 `(avy-lead-face-1 ((t (:background ,user/yellow :foreground ,user/black))))
 `(avy-lead-face-2 ((t (:background ,user/yellow :foreground ,user/black))))
 `(avy-lead-face ((t (:background ,user/yellow :foreground ,user/black))))
 `(avy-background-face ((t (:foreground ,user/bright-black))))
 `(avy-goto-char-timer-face ((t (:inherit highlight)))))

(defface org-checkbox-todo-text
  '((t (:inherit org-todo)))
  "Face for the text part of an unchecked org-mode checkbox.")

(font-lock-add-keywords
 'org-mode
 `(("^[ \t]*\\(?:[-+*]\\|[0-9]+[).]\\)[ \t]+\\(\\(?:\\[@\\(?:start:\\)?[0-9]+\\][ \t]*\\)?\\[\\(?: \\|\\([0-9]+\\)/\\2\\)\\][^\n]*\n\\)" 1 'org-checkbox-todo-text prepend))
 'append)

(defface org-checkbox-done-text
  '((t (:inherit org-done)))
  "Face for the text part of a checked org-mode checkbox.")

(font-lock-add-keywords
 'org-mode
 `(("^[ \t]*\\(?:[-+*]\\|[0-9]+[).]\\)[ \t]+\\(\\(?:\\[@\\(?:start:\\)?[0-9]+\\][ \t]*\\)?\\[\\(?:X\\|\\([0-9]+\\)/\\2\\)\\][^\n]*\n\\)" 1 'org-checkbox-done-text prepend))
 'append)

(use-package linum
  :config (progn
            (setq linum-format " %3d ")
            (add-hook 'prog-mode-hook 'linum-mode)
            (add-hook 'text-mode-hook 'linum-mode)))

(use-package hl-line
  :init (add-hook 'prog-mode-hook 'hl-line-mode)
  :config (setq hl-line-sticky-flag nil
                global-hl-line-sticky-flag nil))

(provide 'user-ui)
;;; user-ui.el ends here
