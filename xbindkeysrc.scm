;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; Start of xbindkeys guile configuration ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; This configuration is guile based.
;; ;;   http://www.gnu.org/software/guile/guile.html
;; ;; any functions that work in guile will work here.
;; ;; see EXTRA FUNCTIONS:

;; ;; A list of keys is in /usr/include/X11/keysym.h and in
;; ;; /usr/include/X11/keysymdef.h
;; ;; The XK_ is not needed.

;; ;; List of modifier:
;; ;;   Release, Control, Shift, Mod1 (Alt), Mod2 (NumLock),
;; ;;   Mod3 (CapsLock), Mod4, Mod5 (Scroll).


;; ;; The release modifier is not a standard X modifier, but you can
;; ;; use it if you want to catch release instead of press events

;; ;; By defaults, xbindkeys does not pay attention to modifiers
;; ;; NumLock, CapsLock and ScrollLock.
;; ;; Uncomment the lines below if you want to use them.
;; ;; To dissable them, call the functions with #f


;; ;;;;EXTRA FUNCTIONS: Enable numlock, scrolllock or capslock usage
;; ;;(set-numlock! #t)
;; ;;(set-scrolllock! #t)
;; ;;(set-capslock! #t)

;; ;;;;; Scheme API reference
;; ;;;;
;; ;; Optional modifier state:
;; ;; (set-numlock! #f or #t)
;; ;; (set-scrolllock! #f or #t)
;; ;; (set-capslock! #f or #t)
;; ;; 
;; ;; Shell command key:
;; ;; (xbindkey key "foo-bar-command [args]")
;; ;; (xbindkey '(modifier* key) "foo-bar-command [args]")
;; ;; 
;; ;; Scheme function key:
;; ;; (xbindkey-function key function-name-or-lambda-function)
;; ;; (xbindkey-function '(modifier* key) function-name-or-lambda-function)
;; ;; 
;; ;; Other functions:
;; ;; (remove-xbindkey key)
;; ;; (run-command "foo-bar-command [args]")
;; ;; (grab-all-keys)
;; ;; (ungrab-all-keys)
;; ;; (remove-all-keys)
;; ;; (debug)

;; tell nuvola to go backward one track
(xbindkey '(XF86HomePage)
          "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.nuvolaplayer /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Next > /dev/null")

;; mute firefox's flash-plugin. Take /that/, hulu ads! :D
(xbindkey '(XF86Search)
          "$HOME/.local/bin/mute-application -a plugin-container; $HOME/.local/bin/mute-application -a Chromium")

;; tell nuvola to go forward one track
(xbindkey '(XF86Mail)
          "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.nuvolaplayer /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Prev > /dev/null")

;; On some machines these get bound to the favorites buttons across
;; the top of the microsoft ergonomic keyboards
(xbindkey '(XF86Launch5)
          "")
(xbindkey '(XF86Launch6)
          "")
(xbindkey '(XF86Launch7)
          "")
(xbindkey '(XF86Launch8)
          "")
(xbindkey '(XF86Launch9)
          "")

