;;; user.ui --- user ui functions -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'base16-material-darker-theme)

(defvar user-ui/colors
  `(:black ,(plist-get base16-material-darker-colors :base00)
    :gray0 "gray15"
    :gray1 "gray17"
    :gray2 "gray23"
    :gray3 ,(plist-get base16-material-darker-colors :base01)
    :gray4 ,(plist-get base16-material-darker-colors :base02)
    :gray5 ,(plist-get base16-material-darker-colors :base03)
    :blueish ,(plist-get base16-material-darker-colors :base04)
    :cyanish ,(plist-get base16-material-darker-colors :base05)
    ;; :base05 and :base06 are the same
    :white ,(plist-get base16-material-darker-colors :base07)
    :red ,(plist-get base16-material-darker-colors :base08)
    :orange ,(plist-get base16-material-darker-colors :base09)
    :yellow ,(plist-get base16-material-darker-colors :base0A)
    :green ,(plist-get base16-material-darker-colors :base0B)
    :cyan ,(plist-get base16-material-darker-colors :base0C)
    :blue ,(plist-get base16-material-darker-colors :base0D)
    :magenta ,(plist-get base16-material-darker-colors :base0E)
    :pink ,(plist-get base16-material-darker-colors :base0F)))

(defvar user-ui/grid-width 9)
(defvar user-ui/grid-height 1)

(defun user-ui/reposition-frame (frame x-pos y-pos)
  "Reposition FRAME to X-POS and Y-POS on grid."
  (set-frame-position frame
                      (round (* (/ (display-pixel-width) (float user-ui/grid-width)) x-pos))
                      (round (* (/ (display-pixel-height) (float user-ui/grid-height)) y-pos))))

(defun user-ui/resize-frame (frame width height)
  "Resize FRAME to HEIGHT and WIDTH on grid."
  (set-frame-size frame
                  ;; HACK: it is unclear to me why subtracting 15 pixels is necessary to match moom's grid
                  (- (round (* (/ (display-pixel-width) (float user-ui/grid-width)) width)) 15)
                  (round (* (/ (display-pixel-height) (float user-ui/grid-height)) height))
                  t))

(provide 'user-ui)
;;; user-ui.el ends here
