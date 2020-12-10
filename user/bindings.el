;;; user.bindings --- user bindings

;;; Commentary:

;;; Code:

(require 'general)

(setq general-default-keymaps 'evil-normal-state-map)

(defvar user/leader-key ",")

(general-define-key
 :keymaps '(evil-normal-state-map
            evil-motion-state-map)
 "j" 'evil-next-visual-line
 "k" 'evil-previous-visual-line
 "gj" 'evil-next-line
 "gk" 'evil-previous-line)

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
        (shell-command "git log origin/production..production --no-merges --oneline --reverse | pbcopy")))

(general-define-key
 "/" 'swiper
 "\\" 'evil-repeat-find-char-reverse
 "[ p" 'evil-unimpaired/paste-above
 "] p" 'evil-unimpaired/paste-below
 "[ SPC" 'evil-unimpaired/insert-space-above
 "] SPC" 'evil-unimpaired/insert-space-below)

(general-define-key
 :keymaps 'ivy-minibuffer-map
 "<C-return>" 'ivy-immediate-done
 "C-c C-c" 'minibuffer-keyboard-quit
 "ESC ESC ESC" 'minibuffer-keyboard-quit)

(general-define-key
 :keymaps 'company-active-map
 "C-n" 'company-select-next
 "C-p" 'company-select-previous)

(general-define-key
 :prefix user/leader-key
 "S"   'prodigy
 "F"   'focus-mode
 ","   'ivy-resume
 "RET" 'user/switch-to-last-buffer
 "SPC" 'counsel-projectile
 ":"  'counsel-M-x
 "bb" 'ivy-switch-buffer
 "bd" (lambda () (interactive) (kill-buffer))
 "ff" 'counsel-find-file
 "fr" 'user/rename-this-buffer-and-file
 "dd" 'user/sql-connect
 "dn" 'user/sql-new-connect
 "ss" 'user/run-eshell
 "sn" 'user/run-new-eshell
 "sv" 'vterm
 "sb" 'multi-eshell-go-back
 "sr" 'rtog/toggle-repl
 "pf" 'counsel-projectile-find-file
 "pd" 'counsel-projectile-find-dir
 "pb" 'counsel-projectile-switch-to-buffer
 "p/" 'counsel-projectile-ag
 "pp" 'projectile-persp-switch-project)

(general-define-key
 :prefix "SPC"
 :keymaps '(evil-normal-state-map
            evil-visual-state-map)
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

(add-hook
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

(general-define-key
 :keymaps 'git-messenger-map
 "q" 'git-messenger:popup-close)

(general-define-key
 :prefix user/leader-key)

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
 :keymaps 'clojure-mode-map
 :states '(normal)
 :prefix user/leader-key
 "mor" 'user/clojure-reload
 "moR" 'user/clojure-reload-all
 "moab" 'user/clojure-aviary-browse
 "moae" 'user/clojure-aviary-export)

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
