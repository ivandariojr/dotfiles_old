(setq frame-title-format (concat invocation-name "@" system-name ": %b"))

;;; tell emacs to use a dark background
(add-to-list 'default-frame-alist '(background-mode . dark))

;;; unclutter the gui when in our own window
(tool-bar-mode -1)                      ;no toolbar
(menu-bar-mode -1)                      ;no menu bar
(set-scroll-bar-mode nil)               ;no scroll bars
(mouse-wheel-mode nil)

;;; these make emacs prefer to split side-by-side when possible.
(setq split-height-threshold nil)       ;not allowed to split
                                        ;vertically (over-under)
(setq split-width-threshold 200)        ;split left-right only if
                                        ;window is wider than this
                                        ;many chars

;;; tell emacs how to read ansi terminal colors
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;;; set font size
(set-face-attribute 'default nil :height 110)

;;load solarize theme
(load-theme 'solarized-dark t)
