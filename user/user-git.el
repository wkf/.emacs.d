;;; user.git --- git configuration
;;; Commentary:
;;; Code:

(require 'shackle)

(use-package git-gutter
  :commands git-gutter-mode
  :init (progn
          (add-hook 'text-mode-hook 'git-gutter-mode)
          (add-hook 'prog-mode-hook 'git-gutter-mode)
          (add-hook 'conf-mode-hook 'git-gutter-mode))
  :config (progn
            (require 'git-gutter-fringe)
            (setq git-gutter-fr:side 'right-fringe)

            (push
             '("^\\*git-gutter.+\\*$"
               :align below :size 15 :noselect t :regexp t)
             shackle-rules)

            (define-fringe-bitmap 'git-gutter-fr:added
              [3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3]
              nil nil 'center)
            (define-fringe-bitmap 'git-gutter-fr:modified
              [3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3]
              nil nil 'center)
            (define-fringe-bitmap 'git-gutter-fr:deleted
              [3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3]
              nil nil 'center)

            (advice-add 'evil-force-normal-state :after 'git-gutter)
            (add-hook 'focus-in-hook 'git-gutter:update-all-windows)))

(use-package git-messenger
  :commands git-messenger:popup-message
  :init (defvar git-messenger-map (make-sparse-keymap))
  :config (progn
            (push
             '("*git-messenger*" :align left :size 55 :select t)
             shackle-rules)
            (setq git-messenger:show-detail t)))

(use-package magit
  :commands (magit-status)
  :config (progn
            (setq magit-completing-read-function 'ivy-completing-read)
            (add-hook 'magit-mode-hook 'turn-off-evil-snipe-override-mode)
            (require 'evil-magit)
            (setq magit-display-buffer-function
                  (lambda (buffer)
                    (display-buffer
                     buffer (if (and (derived-mode-p 'magit-mode)
                                     (memq (with-current-buffer buffer major-mode)
                                           '(magit-process-mode
                                             magit-revision-mode
                                             magit-diff-mode
                                             magit-stash-mode
                                             magit-status-mode)))
                                nil
                              '(display-buffer-same-window)))))))

(provide 'user-git)
;;; user-git.el ends here
