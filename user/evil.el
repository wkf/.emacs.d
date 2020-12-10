;;; user.evil --- user bindings

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

(use-package evil
  :init (setq evil-want-fine-undo t
              evil-want-C-u-scroll t
              evil-want-keybinding nil)
  :config (progn
            (evil-mode 1)

            (setq evil-cross-lines t
                  evil-move-beyond-eol t
                  evil-emacs-state-cursor `(,user/yellow box)
                  evil-insert-state-cursor `(,user/green bar)
                  evil-motion-state-cursor `(,user/purple box)
                  evil-normal-state-cursor `(,user/blue box)
                  evil-replace-state-cursor `(,user/red bar)
                  evil-visual-state-cursor `(,user/orange box))

            ;; workaround for...
            ;; https://github.com/abo-abo/swiper/issues/977
            (defvar user/-evil-ex-minibuffer nil)

            (defun user/-before-evil-ex-setup ()
              (setq user/-evil-ex-minibuffer (current-buffer)))

            (defun user/-around-evil-ex-teardown (f)
              (when (eq user/-evil-ex-minibuffer (current-buffer))
                (funcall f)
                (setq user/-evil-ex-minibuffer nil)))

            (advice-add 'evil-ex-setup :before 'user/-before-evil-ex-setup)
            (advice-add 'evil-ex-teardown :around 'user/-around-evil-ex-teardown)
            ;;

            (evil-set-undo-system 'undo-fu)
            (evil-collection-init 'vterm)
            (evil-collection-init 'cider)
            (evil-collection-init 'eshell)))

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

(provide 'user.evil)
;;; evil.el ends here
