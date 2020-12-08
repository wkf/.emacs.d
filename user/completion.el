;;; user.completion --- completion configuration
;;; Commentary:
;;; Code:

(use-package ivy
  :config (progn (ivy-mode 1)
                 (setq ivy-use-virtual-buffers t)))

(defun user/swiper-update-search-ring-forward (&rest args)
  "Update swiper results so n/N bindings work, ignoring ARGS."
  (add-to-history 'regexp-search-ring (ivy--regex ivy-text))
  (setq isearch-forward t))

(defun user/swiper-update-search-ring-backward (&rest args)
  "Update swiper results so n/N bindings work, ignoring ARGS."
  (add-to-history 'regexp-search-ring (ivy--regex ivy-text))
  (setq isearch-forward nil))

  (advice-add 'ivy-next-line :after #'user/swiper-update-search-ring-forward)
  (advice-add 'ivy-previous-line :after #'user/swiper-update-search-ring-backward)

(use-package projectile
  :config (projectile-mode))

(use-package perspective
  :config (persp-mode))

(use-package counsel-projectile
  :config (progn
            (require 'persp-projectile)
            (counsel-projectile-mode)))

(use-package company
  :config (progn (global-company-mode)
                 (setq company-idle-delay 0.2)))

(defun user/rename-this-buffer-and-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (cond ((get-buffer new-name)
               (error "A buffer named '%s' already exists!" new-name))
              (t
               (rename-file filename new-name 1)
               (rename-buffer new-name)
               (set-visited-file-name new-name)
               (set-buffer-modified-p nil)
               (message "File '%s' successfully renamed to '%s'" name (file-name-nondirectory new-name))))))))

(provide 'user.completion)
;;; completion.el ends here
