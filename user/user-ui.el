;;; user.ui --- user ui functions
;;; Commentary:
;;; Code:

(require 'base16-theme)

(eval-when-compile
  (defvar base16-material-darker-colors))

;; base00 - gray 1
;; base01 - gray 2
;; base02 - gray 3
;; base03 - gray 4
;; base04 - light blue
;; base05 - light cyan
;; base06 - light cyan
;; base07 - white
;; base08 - red
;; base09 - orange
;; base0A - yellow
;; base0B - green
;; base0C - cyan
;; base0D - blue
;; base0E - purple
;; base0F - bright red

(defvar user/theme-colors
  `(:black ,(plist-get base16-material-darker-colors :base00)
    :gray0 ,(plist-get base16-material-darker-colors :base01)
    :ray1 ,(plist-get base16-material-darker-colors :base02)
    :ray2 ,(plist-get base16-material-darker-colors :base03)
    :blueish ,(plist-get base16-material-darker-colors :base04)
    :cyanish ,(plist-get base16-material-darker-colors :base05)
    :white ,(plist-get base16-material-darker-colors :base07)
    :red ,(plist-get base16-material-darker-colors :base08)
    :orange ,(plist-get base16-material-darker-colors :base09)
    :yellow ,(plist-get base16-material-darker-colors :base0A)
    :green ,(plist-get base16-material-darker-colors :base0B)
    :cyan ,(plist-get base16-material-darker-colors :base0C)
    :blue ,(plist-get base16-material-darker-colors :base0D)
    :purple ,(plist-get base16-material-darker-colors :base0E)
    :pink ,(plist-get base16-material-darker-colors :base0F)))

;; (plist-get user/theme-colors :red)

(defvar user/red
  (plist-get base16-material-darker-colors :base08))

(defvar user/orange
  (plist-get base16-material-darker-colors :base09))

(defvar user/yellow
  (plist-get base16-material-darker-colors :base0A))

(defvar user/green
  (plist-get base16-material-darker-colors :base0B))

(defvar user/cyan
  (plist-get base16-material-darker-colors :base0C))

(defvar user/blue
  (plist-get base16-material-darker-colors :base0D))

(defvar user/purple
  (plist-get base16-material-darker-colors :base0E))

(defvar user/magenta
  (plist-get base16-material-darker-colors :base0E))

(defvar user/black
  (plist-get base16-material-darker-colors :base00))

(defvar user/white
  (plist-get base16-material-darker-colors :base05))

(defvar user/bright-red
  (plist-get base16-material-darker-colors :base09))

(defvar user/bright-yellow
  (plist-get base16-material-darker-colors :base02))

(defvar user/bright-green
  (plist-get base16-material-darker-colors :base01))

(defvar user/bright-cyan
  (plist-get base16-material-darker-colors :base0F))

(defvar user/pink
  (plist-get base16-material-darker-colors :base0F))

(defvar user/bright-blue
  (plist-get base16-material-darker-colors :base04))

(defvar user/bright-magenta
  (plist-get base16-material-darker-colors :base06))

(defvar user/bright-black
  (plist-get base16-material-darker-colors :base03))

(defvar user/bright-white
  (plist-get base16-material-darker-colors :base07))

(provide 'user-ui)
;;; user-ui.el ends here
