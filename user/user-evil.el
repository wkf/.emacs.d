;;; user-evil --- user bindings

;;; Commentary:

;;; Code:

(require 'general)

(defun evil-unimpaired/paste-above ()
  "Paste above current line."
  (interactive)
  (evil-insert-newline-above)
  (evil-paste-after 1))

(defun evil-unimpaired/paste-below ()
  "Paste below current line."
  (interactive)
  (evil-insert-newline-below)
  (evil-paste-after 1))

(defun evil-unimpaired/insert-space-above (count)
  "Insert COUNT newlines above current line."
  (interactive "p")
  (dotimes (_ count) (save-excursion (evil-insert-newline-above))))

(defun evil-unimpaired/insert-space-below (count)
  "Insert COUNT newlines below current line."
  (interactive "p")
  (dotimes (_ count) (save-excursion (evil-insert-newline-below))))


(use-package evil-surround
  :after evil
  :config (global-evil-surround-mode 1))

(use-package evil-snipe
  :after evil
  :config (progn
            (evil-snipe-mode +1)
            (evil-snipe-override-mode +1)))

(use-package evil-commentary
  :after evil
  :config (evil-commentary-mode))

(use-package evil-org
  :after (org evil)
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (evil-org-set-key-theme '(navigation insert textobjects todo)))

;; (use-package evil-mc
;;   :config
;;   (global-evil-mc-mode 1))

;; (use-package evil-multiedit
;;   :config
;;   (general-define-key
;;    :states 'visual
;;    "R" 'evil-multiedit-match-all
;;    "gR" 'evil-multiedit-restore
;;    "RET" 'evil-multiedit-toggle-or-restrict-region)
;;   (general-define-key
;;    :states '(normal visual)
;;    "C-n" 'evil-multiedit-match-symbol-and-next
;;    "C-p" 'evil-multiedit-match-symbol-and-prev)
;;   (general-define-key
;;    :keymaps 'evil-motion-state-map
;;    "RET" 'evil-multiedit-toggle-or-restrict-region)
;;   (general-define-key
;;    :keymaps '(evil-multiedit-state-map evil-multiedit-insert-state-map)
;;    "C-n" 'evil-multiedit-next
;;    "C-p" 'evil-multiedit-next)
;;   (evil-ex-define-cmd "ie[dit]" 'evil-multiedit-ex-match))

(use-package prodigy
  :init (progn
          (add-to-list 'evil-emacs-state-modes 'prodigy-mode)))

(provide 'user-evil)
;;; user-evil.el ends here
