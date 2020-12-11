;;; user.bindings --- user bindings

;;; Commentary:

;;; Code:

(require 'general)



;; (general-define-key
;;  "/" 'swiper
;;  "\\" 'evil-repeat-find-char-reverse
;;  "[ p" 'evil-unimpaired/paste-above
;;  "] p" 'evil-unimpaired/paste-below
;;  "[ SPC" 'evil-unimpaired/insert-space-above
;;  "] SPC" 'evil-unimpaired/insert-space-below)

;; (general-define-key
;;  :keymaps 'ivy-minibuffer-map
;;  "<C-return>" 'ivy-immediate-done
;;  "C-c C-c" 'minibuffer-keyboard-quit
;;  "ESC ESC ESC" 'minibuffer-keyboard-quit)

(general-define-key
 :prefix user/leader-key
 ;; "S"   'prodigy
 ;; "F"   'focus-mode
 ;; ","  'ivy-resume
 ;; "bb" 'ivy-switch-buffer
 ;; "bd" (lambda () (interactive) (kill-buffer))
 ;; ":"  'counsel-M-x
 ;; "ff" 'counsel-find-file
 ;; "SPC" 'counsel-projectile
 ;; "pf" 'counsel-projectile-find-file
 ;; "pd" 'counsel-projectile-find-dir
 ;; "pb" 'counsel-projectile-switch-to-buffer
 ;; "p/" 'counsel-projectile-ag
 ;; "fr" 'user/rename-this-buffer-and-file
 ;; "dd" 'user/sql-connect
 ;; "dn" 'user/sql-new-connect
 ;; "sr" 'rtog/toggle-repl
 ;; "ss" 'user/run-eshell
 ;; "sn" 'user/run-new-eshell
 ;; "sv" 'vterm
 ;; "sb" 'multi-eshell-go-back
 ;; "pp" 'projectile-persp-switch-project
 )

;; (add-hook
;;  'eshell-mode-hook
;;  (lambda ()
;;    (genera-define-key
;;     :keymaps 'eshell-mode-map
;;     :states 'normal
;;     ",sc" (lambda () (interactive) (eshell/clear) (eshell-send-input)))
;;    (if nil (general-define-key
;;             :keymaps 'eshell-mode-map
;;             :states '(normal insert)
;;             "RET" 'eshell-send-input
;;             "C-n" 'eshell-next-input
;;             "C-p" 'eshell-previous-input
;;             "C-s" 'counsel-esh-history))))

;; (general-define-key
;;  :keymaps 'git-messenger-map
;;  "q" 'git-messenger:popup-close)

;; (general-define-key
;;  :prefix user/leader-key)

;; (general-define-key
;;  :keymaps 'prodigy-mode-map
;;  :states '(emacs normal)
;;  "h" 'prodigy-first
;;  "j" 'prodigy-next
;;  "k" 'prodigy-prev
;;  "l" 'prodigy-last
;;  "H" 'prodigy-display-process
;;  "J" 'prodigy-next-with-status
;;  "K" 'prodigy-prev-with-status
;;  "L" 'prodigy-start
;;  "d" 'prodigy-jump-dired
;;  "g" 'prodigy-jump-magit
;;  "Y" 'prodigy-copy-cmd)

(general-define-key)

(general-define-key)

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

(provide 'user-bindings)
;;; user-bindings.el ends here
