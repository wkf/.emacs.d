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

            ;; (evil-define-operator user/evil-quickrun-region (beg end)
            ;;   "Evil operator for evaluating code."
            ;;   :move-point nil
            ;;   (interactive "<r>")
            ;;   (quickrun-region beg end))

            ;; (evil-define-operator user/evil-quickrun-region-replace (beg end)
            ;;   "Evil operator for evaluating code."
            ;;   :move-point nil
            ;;   (interactive "<r>")
            ;;   (quickrun-replace-region beg end))
            ))

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

;; (add-hook 'lispy-mode-hook #'lispyville-mode)

;; (evil-define-motion user/evil-cp-up-sexp (count)
;;   "Motion for moving up to the previous level of parenteheses.
;; The same as `sp-up-sexp', but leaves the point on top of the
;; closing paren."
;;   :move-point nil
;;   :type inclusive
;;   (let ((count (or count 1)))
;;     (when (or (evil-cp--looking-at-any-opening-p)
;;               (evil-cp--looking-at-any-closing-p))
;;       (forward-char))
;;     (dotimes (i count)
;;       (sp-up-sexp))
;;     (backward-char)))

;; (evil-define-motion user/evil-cp-forward-sexp-begin (count)
;;   "Motion for moving forward by a sexp via `sp-forward-sexp'."
;;   :type exclusive
;;   (let ((count (or count 1)))
;;     (sp-forward-sexp count)
;;     ;; (sp-forward-sexp 1)
;;     ;; (sp-backward-sexp 1)
;;     ))

;; (evil-define-motion user/evil-cp-backward-sexp-begin (count)
;;   "Motion for moving backwward by a sexp via `sp-backward-sexp'."
;;   :type exclusive
;;   (let ((count (or count 1)))
;;     (sp-backward-sexp count)))

;; (evil-define-motion user/evil-cp-forward-sexp-end(count)
;;   "Motion for moving backwward by a sexp via `sp-backward-sexp'."
;;   :type exclusive
;;   (let ((count (or count 1)))
;;     (when (not  (or (evil-cp--looking-at-opening-p)
;;                     (evil-cp--looking-at-closing-p)))
;;       (forward-char))
;;     (sp-forward-sexp count)
;;     (backward-char)
;;     ;; (backward-char)
;;     ))

;; (evil-define-motion user/evil-cp-backward-sexp-end(count)
;;   "Motion for moving backwward by a sexp via `sp-backward-sexp'."
;;   :type exclusive
;;   (let ((count (or count 1)))
;;     (sp-backward-sexp count)))

;; (use-package evil-cleverparens
;;   :after evil
;;   :init (progn
;;           (setq evil-cleverparens-use-s-and-S nil
;;                 evil-cleverparens-use-additional-bindings nil
;;                 evil-cleverparens-use-additional-movement-keys t
;;                 )

;;           (setq-default evil-cp-regular-movement-keys
;;                         '(("w"  . evil-forward-word-begin)
;;                           ("e"  . evil-forward-word-end)
;;                           ("b"  . evil-backward-word-begin)
;;                           ("ge" . evil-backward-word-end)))

;;           (setq-default evil-cp-regular-bindings
;;                         '(("d"   . evil-cp-delete)
;;                           ("c"   . evil-cp-change)
;;                           ("y"   . evil-cp-yank)
;;                           ("D"   . evil-cp-delete-line)
;;                           ("C"   . evil-cp-change-line)
;;                           ("Y"   . evil-cp-yank-line)
;;                           ("x"   . evil-cp-delete-char-or-splice)
;;                           (">>"  . evil-cp->)
;;                           ("<<"  . evil-cp-<)
;;                           ("><"  . evil-cp-drag-forward)
;;                           ("<>"  . evil-cp-drag-backward)
;;                           ("_"   . evil-cp-first-non-blank-non-opening)
;;                           ("Rf"  . evil-cp-raise-form)
;;                           ("Re"  . sp-raise-sexp)
;;                           ("gI"  . evil-cp-insert-at-beginning-of-form)
;;                           ("gA"  . evil-cp-insert-at-end-of-form)
;;                           ("gJ"  . sp-join-sexp)
;;                           ("gS"  . sp-split-sexp)
;;                           ("gO"  . evil-cp-open-above-form)
;;                           ("go"  . evil-cp-open-below-form)
;;                           ("R)"  . evil-cp-wrap-next-round)
;;                           ("R("  . evil-cp-wrap-previous-round)
;;                           ("R]"  . evil-cp-wrap-next-square)
;;                           ("R["  . evil-cp-wrap-previous-square)
;;                           ("R}"  . evil-cp-wrap-next-curly)
;;                           ("R{"  . evil-cp-wrap-previous-curly)
;;                           ))

;;           (setq-default evil-cp-additional-movement-keys
;;                         '(
;;                           ;; ("[("  . evil-cp-previous-opening)
;;                           ;; ("])"  . evil-cp-next-closing)
;;                           ;; ("]("  . evil-cp-next-opening)
;;                           ;; ("[)"  . evil-cp-previous-closing)
;;                           ("W"  . user/evil-cp-forward-sexp-begin)
;;                           ("E"  . user/evil-cp-forward-sexp-end)
;;                           ("B"  . user/evil-cp-backward-sexp-begin)
;;                           ("gE" . user/evil-cp-backward-sexp-end)
;;                           ("("   . evil-cp-backward-up-sexp)
;;                           (")"   . user/evil-cp-up-sexp)))))

(use-package prodigy
  :init (progn
          (add-to-list 'evil-emacs-state-modes 'prodigy-mode)))

(provide 'user.evil)
;;; evil.el ends here
