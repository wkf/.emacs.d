;;; user.bindings --- user bindings

;;; Commentary:

;;; Code:

(require 'general)
;; (require 'rjsx-mode)

(defvar user/leader-key ",")

(setq
 general-default-keymaps 'evil-normal-state-map)

(general-define-key
 :keymaps '(evil-normal-state-map
            evil-motion-state-map)
 "j" 'evil-next-visual-line
 "k" 'evil-previous-visual-line
 "gj" 'evil-next-line
 "gk" 'evil-previous-line
 ;; "gr" 'user/evil-quickrun-region
 ;; "gR" 'user/evil-quickrun-region-replace
)

;; (general-define-key
;;  :keymaps '(evil-normal-state-map
;;             evil-insert-state-map)
;;  "C-h" 'windmove-left
;;  "C-l" 'windmove-right
;;  "C-k" 'windmove-up
;;  "C-j" 'windmove-down)

(general-define-key
 :prefix user/leader-key
 :keymaps '(evil-normal-state-map
            evil-visual-state-map)
 "w=" 'balance-windows
 "wo" 'delete-other-windows
 "ws" 'split-window-vertically
 "wv" 'split-window-horizontally
 "gs" 'magit-status
 "gb" 'magit-blame
 "gq" 'magit-blame-quit
 "gc" (lambda ()
        (interactive)
        (shell-command "git log origin/production..production --no-merges --oneline --reverse | pbcopy"))
 "rb" 'quickrun
 "rr" 'quickrun-region
 "ra" 'quickrun-with-arg
 "rs" 'quickrun-shell
 "rp" 'quickrun-replace-region)

(general-define-key
 :keymaps 'quickrun/mode-map
 :states '(normal)
 "q" 'quit-window
 "C-c C-c" 'quickrun/kill-running-process)

(general-define-key
 "\\" 'evil-repeat-find-char-reverse
 "[ p" 'evil-unimpaired/paste-above
 "] p" 'evil-unimpaired/paste-below
 "[ SPC" 'evil-unimpaired/insert-space-above
 "] SPC" 'evil-unimpaired/insert-space-below)

;; (general-define-key
;;  :keymaps '(emacs-lisp-mode-map
;;             clojure-mode-map
;;             clojurec-mode-map
;;             clojurescript-mode-map)
;;  :states '(normal)
;;  :prefix "g"
;;  "j" 'sp-join-sexp
;;  "s" 'sp-split-sexp
;;  "r" 'sp-raise-sexp
;;  "t" 'sp-transponse-sexp)

;; (general-define-key
;;  :keymaps '(rjsx-mode-map)
;;  :states '(insert)
;;  "<" 'rjsx-electric-lt
;;  ">" 'rjsx-electric-gt
;;  "C-d" 'rjsx-delete-creates-full-tag
;;  "C-c C-r" 'rjsx-rename-tag-at-point)

(general-define-key
 :keymaps 'ivy-minibuffer-map
 "<C-return>" 'ivy-immediate-done
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
 "ff" 'counsel-find-file
 "fr" 'user/rename-this-buffer-and-file
 "dd" 'user/sql-connect
 "dn" 'user/sql-new-connect)

(general-define-key
 "SPC" 'counsel-projectile)

(general-define-key
 :prefix user/leader-key
 "F" 'focus-mode)

(general-define-key
 :prefix user/leader-key
 "pf" 'counsel-projectile-find-file
 "pd" 'counsel-projectile-find-dir
 "pb" 'counsel-projectile-switch-to-buffer
 "p/" 'counsel-projectile-ag
 "pp" 'projectile-persp-switch-project)

(add-hook 'eshell-mode-hook
          (lambda ()
            (general-define-key
             :keymaps 'eshell-mode-map
             :states '(normal insert)
             "RET" 'eshell-send-input
             "C-n" 'eshell-next-input
             "C-p" 'eshell-previous-input
             "C-s" 'counsel-esh-history
             ",sc" (lambda () (interactive) (eshell/clear) (eshell-send-input)))))

(general-define-key
 :prefix user/leader-key
 "ss" 'user/run-eshell
 "sn" 'user/run-new-eshell
 "sb" 'multi-eshell-go-back
 "sr" 'rtog/toggle-repl)

(general-define-key
 :keymaps 'company-active-map
 "C-n" 'company-select-next
 "C-p" 'company-select-previous)

(general-define-key
 :keymaps 'emacs-lisp-mode-map
 :states '(normal)
 "X" 'eval-sexp-fu-eval-sexp-inner-sexp)

(general-define-key
 :keymaps 'git-messenger-map
 "q" 'git-messenger:popup-close)

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

(general-define-key
 :keymaps '(clojure-mode-map
            clojurec-mode-map
            clojurescript-mode-map)
 :states '(normal)
 "X" 'eval-sexp-fu-cider-eval-sexp-inner-sexp)

(general-define-key
 :keymaps 'clojure-mode-map
 :states '(normal)
 :prefix user/leader-key
 "mor" 'user/clojure-reload
 "moR" 'user/clojure-reload-all
 "moab" 'user/clojure-aviary-browse
 "moae" 'user/clojure-aviary-export)

(if nil (general-define-key
 :keymaps 'cider-repl-mode-hook
 :states '(normal insert)
 "C-n" (lambda (count)
         (interactive "p") (cider-repl-next-input))
 "C-p" (lambda (count)
         (interactive "p") (cider-repl-previous-input))))

(general-define-key
 :keymaps 'anaconda-mode-map
 :states '(normal)
 "gd" 'anaconda-mode-find-definitions)

(general-define-key
 :keymaps 'anaconda-nav-mode-map
 :states '(normal)
 "ESC" 'anaconda-nav-quit)

(general-define-key
 :keymaps 'alchemist-iex-mode-map
 :states '(normal insert)
 "C-n" #'comint-next-input
 "C-p" #'comint-previous-input)

(general-define-key
 :keymaps 'sql-interactive-mode-map
 :states '(normal insert)
 "C-n" #'comint-next-input
 "C-p" #'comint-previous-input)

(provide 'user.bindings)
;;; bindings.el ends here
