;;; init.el -- emacs configuration file

;;; Commentary:

;; TODO:
;;  [x] evil leader
;;  [x] cleverparens
;;  [x] completion framework
;;  [x] window management/switching
;;  [x] perspective
;;  [x] evil extras
;;  [ ] mode line
;;  [x] configure backup files
;;  [x] fix line numbers
;;  [x] highlight current line
;;  [x] tweak all the colors
;;  [x] eshell (change color of lambda with evil state change)
;;  [x] flycheck
;;  [x] code completion
;;  [ ] quick run
;;  [ ] repls
;;  [x] git diff
;;  [ ] clojure
;;  [x] highlight todos
;;  [x] prodigy
;;  [ ] shackle
;;  [ ] magit
;;  [ ] lispy
;;  [ ] hydra
;;  [ ] org
;;  [ ] project specific configuration
;;  [ ] clippy
;;  [ ] focus (plugin name)
;;  [x] fix fringe indicators for long lines
;;  [ ] remove useless buffers from completion list
;;  [ ] disable flycheck in scratch buffer
;;  [x] fix flash colors for eval sexp
;;  [ ] debug occasional "wrong argument type stringp" error when using ivy
;;  [ ] start spinner in mode line for long running com
;;  [ ] add quickrun motion to eval regions

;;; Code:

(dolist (p '("user/core"
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

;;; init.el ends here
