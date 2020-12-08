;;; init.el -- emacs configuration file

;;; Commentary:

;;; Code:

(declare-function user/projectile-switch-to-project-file "user.core")

(dolist (p '("user/packages"
             "user/core"
             "user/ui"
             "user/evil"
             "user/git"
             "user/completion"
             "user/mode-line"
             "user/shell"
             "user/languages"
             "user/services"
             "user/database"
             "user/bindings"))
  (load-library (concat user-emacs-directory p)))

(user/projectile-switch-to-project-file "~/.emacs.d/" "welcome.org")

;;; init.el ends here
