; -*-lisp-*-
; vim: set filetype=lisp : 

;; this is where we'll save everything
(add-to-list 'load-path "~/.emacs.d")

;; require common lisp
(require 'cl)

;; First, load up some packages
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)                    ;load everything







;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                         ;;
;;  CONFIGURATION                                                          ;;
;;                                                                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; behavior
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq make-backup-files nil)            ;no backups to clutter things up
(setq inhibit-startup-message t)        ;don't put up the splash page
(iswitchb-mode)                         ;better buffer-switching

;; global settings for tabs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 8)

;; use utf-8 everywhere
(set-language-environment "UTF-8")
(set-buffer-file-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)

;; put temporary files somewhere else
(defvar user-temporary-file-directory
  (concat temporary-file-directory user-login-name "/"))
(make-directory user-temporary-file-directory t)
(setq backup-by-copying t)
(setq backup-directory-alist
      `(("." . ,user-temporary-file-directory)
        (,tramp-file-name-regexp nil)))
(setq auto-save-list-file-prefix
      (concat user-temporary-file-directory ".auto-saves-"))
(setq auto-save-file-name-transforms
      `((".*" ,user-temporary-file-directory t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; appearance ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; tell emacs to use a dark background
(add-to-list 'default-frame-alist '(background-mode . dark))

;; unclutter the gui when in our own window
(tool-bar-mode -1)                      ;no toolbar
(menu-bar-mode -1)                      ;no menu bar
(set-scroll-bar-mode nil)               ;no scroll bars
(mouse-wheel-mode nil)

;; these make emacs prefer to split side-by-side when possible.
(setq split-height-threshold nil)       ;no limit on maximum window height
(setq split-width-threshold 130)        ;split over-under only if
                                        ;splitting left-right would
                                        ;make the window too thin.

;; tell emacs how to read ansi terminal colors
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; load up solarized from the ~/src directory where we've cloned the
;; appropriate repo
(add-to-list 'custom-theme-load-path "~/src/emacs-color-theme-solarized" t)

(add-hook 'server-visit-hook
          '(lambda ()
             (load-theme 'solarized-dark)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; keybindings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "C-c k") 'compile)

;; I give up - we'll just reload the theme manually whenever we need to
(global-set-key (kbd "C-c C-l") 
                (lambda ()
                  (interactive)
                  (load-theme 'solarized-dark)))













;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                         ;;
;;  IMPROVEMENTS                                                           ;;
;;                                                                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dired+ ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; improves dired-mode

(load "dired+.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; emacs-helm ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; improves buffer-switching, autocomplete, and a lot of
;; other things

(add-to-list 'load-path "~/src/emacs-helm")
(require 'helm-config)
(global-set-key (kbd "C-c h") 'helm-mini)
(global-set-key (kbd "C-c C-h") 'helm-mini)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                         ;;
;;  ADDITIONS                                                              ;;
;;                                                                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; user functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; use wmctrl to set X fullscreen
(defun switch-full-screen ()
  (interactive)
  (shell-command "wmctrl -r :ACTIVE: -btoggle,fullscreen"))
(global-set-key [f11] 'switch-full-screen)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-c\C-a" 'org-agenda)
(global-set-key [f5] 'org-capture)
(global-set-key [f6] 'org-agenda-list)
(global-set-key [f7] 'org-todo-list)
(setq org-log-done t)

(setq org-todo-keywords
      '((sequence "TODO" "IN-PROGRESS" "|" "DONE" "WILL-NOT-DO" "REVISIT-LATER")))

(defadvice org-capture-finalize (after delete-capture-frame activate)
  "Advise capture-finalize to close the frame if it is the capture frame"
  (if (equal "capture" (frame-parameter nil 'name))
      (delete-frame)))

(defadvice org-capture-destroy (after delete-capture-frame activate)
  "Advise capture-destroy to close the frame if it is the capture frame"
  (if (equal "capture" (frame-parameter nil 'name))
      (delete-frame)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; appt-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun saul-notification-send (title msg &optional icon sound)
  "Show a popup if we're on X, otherwise echo it; TITLE is the title
  of the message, MSG is the content. Optionally provide a SOUND that
  will be played and an ICON that will be displayed."
  (interactive)
  (when sound (shell-command
               (concat "mplayer --really-quiet " sound " 2> /dev/null")))
  (if (eq window-system 'x)
      ;; visual version
      (shell-command (concat "notify-send "
                             (if icon (concat "-i " icon) "")
                             " '" title "' '" msg "'"))
    ;; text-only version
    (message (concat title ": " msg))))

;; set up the appoinment notification facility
(setq
 appt-message-warning-time 30           ;warn 30 min in advance
 appt-display-mode-line t
 appt-display-format 'window
 diary-file "~/org/diary")
(appt-activate 1)

;; update appt every time we open our agenda
(add-hook 'org-finalize-agenda-hook 'org-agenda-to-appt)

;; and hook our display notifier into the system
(defun saul-appt-display (min-to-app new-time msg)
  (saul-notification-send (format "Appointment in %s minutes" min-to-app) msg
                          "/usr/share/icons/gnome/scalable/status/appoinment-soon-symbolic.png"
                          "/usr/share/sounds/ubuntu/stereo/message.ogg"))
(setq appt-disp-window-function (function saul-appt-display))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org-babel ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (sh . t)
   (python . t)
   (octave . t)
   (emacs-lisp . t)
   (lisp . t)))
        

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs IRC Client ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq erc-auto-query 'frame) ; on PM, open in new frame.

(add-hook 'erc-after-connect ; same behavior for notices
          (lambda (server nick)
            (add-hook 'erc-server-NOTICE-hook 'erc-auto-query)))

(require 'erc-match)                    ; highlight on nick said
(setq erc-keywords '("vebyast"))

; change fill width when the window changes size
(make-variable-buffer-local 'erc-fill-column)
(add-hook 'window-configuration-change-hook 
          '(lambda ()
             (save-excursion
               (walk-windows
                (lambda (w)
                  (let ((buffer (window-buffer w)))
                    (set-buffer buffer)
                    (when (eq major-mode 'erc-mode)
                      (setq erc-fill-column (- (window-width w) 2)))))))))

;(load "~/.irc.el")              ;load passwords
;(setq erc-nickserv-passwords
      ;`(("mibbit"        (("vebyast" . ,mibbit-nickone-pass)))
        ;("foonetic"      (("vebyast" . ,foonetic-nickone-pass)))))

;(add-hook 'erc-after-connect            ;use passwords to autoconnect
          ;'(lambda (SERVER NICK)
             ;(cond
              ;((string-match "mibbit.net" SERVER)
               ;(erc-message "PRIVMSG" (concat "nickserv identify " mibbit-nickone-pass)))
              ;((string-match "foonetic.net" SERVER)
               ;(erc-message "PRIVMSG" (concat "nickserv identify " foonetic-nickone-pass))))))

(load "~/.emacs.d/erc-highlight-nicknames.el") ; turn on nick coloring
(add-hook 'erc-join-hook 'erc-highlight-nicknames-enable)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Git ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Produce syntax highlighting in git commit mode
(require 'git-commit)
;; (load "~/.emacs.d/magit-commit.el")








;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                         ;;
;;  LANGUAGES                                                              ;;
;;                                                                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; use 4 spaces by default, select formatting style
(add-hook 'c-mode-common-hook 
          (lambda ()
            (c-set-style "bsd") ; I've written too much java
            (setq c-basic-offset 4)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C++ ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(push '("\\.ino$" . c++-mode)  auto-mode-alist)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; haskell ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-decl-scan)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; common lisp ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(push '("\\.sbclrc$" . lisp-mode)  auto-mode-alist )
(push '("\\.asdf-install$" . lisp-mode) auto-mode-alist )
(push '("\\.asd$" . lisp-mode) auto-mode-alist )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; backus-naur form  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-generic-mode 'bnf-mode
  () ;; comment char: inapplicable because # must be at start of line
  nil ;; keywords
  '(
    ("^#.*" . 'font-lock-comment-face) ;; comments at start of line
    ("^<.*?>" . 'font-lock-function-name-face) ;; LHS nonterminals
    ("<.*?>" . 'font-lock-builtin-face) ;; other nonterminals
    ("::=" . 'font-lock-const-face) ;; "goes-to" symbol
    ("\|" . 'font-lock-warning-face) ;; "OR" symbol
    ("\{:\\|:\}" . 'font-lock-keyword-face) ;; special pybnf delimiters
   )
  '("\\.bnf\\'" "\\.pybnf\\'") ;; filename suffixes
  nil ;; extra function hooks
  "Major mode for BNF highlighting.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; octave-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; start octave without the giant blurb thing
(setq inferior-octave-startup-args `("-q"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; common lisp  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(add-to-list 'load-path "/home/saul/src/slime/slime")
(setq inferior-lisp-program "/usr/local/bin/sbcl")
;;(setq common-lisp-hyperspec-root "file:/home/saul/documents/source/hyperspec/")

(eval-after-load "slime"
  '(progn
     (setq slime-lisp-implementations '((sbcl ("/usr/local/bin/sbcl"))))
     (slime-setup '(slime-asdf
                    slime-autodoc
                    slime-editing-commands
                    slime-fancy-inspector
                    slime-fontifying-fu
                    slime-fuzzy
                    slime-indentation
                    slime-mdot-fu
                    slime-package-fu
                    slime-references
                    slime-repl
                    slime-sbcl-exts
                    slime-scratch
                    slime-xref-browser))
     (slime-autodoc-mode)
     (setq slime-complete-symbol*-fancy t
           slime-complete-sumbol-function 'slime-fuzzy-complete-symbol)))

;; (require 'slime)
;; (slime-setup)

(add-hook 'slime-mode-hook
          (lambda ()
            (local-set-key "C-c C-c" slime-eval-buffer)
            (local-set-key "C-c d" slime-eval-buffer)
            (local-set-key "C-c C-d" slime-eval-defun)
            (local-set-key "C-c r" slime-eval-region)
            (local-set-key "C-c C-r" slime-eval-region)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; python  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(require 'pymacs)
;;(pymacs-load "ropemacs" "rope-")







;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                         ;;
;;  CUSTOM                                                                 ;;
;;  do not edit manually!                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 113 :width normal :foundry "misc" :family "fixed"))))
 '(slime-highlight-edits-face ((((class color) (background dark)) (:background "#333"))) t))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(case-fold-search t)
 '(current-language-environment "English")
 '(custom-safe-themes (quote ("501caa208affa1145ccbb4b74b6cd66c3091e41c5bb66c677feda9def5eab19c" "7b4a6cbd00303fc53c2d486dfdbe76543e1491118eba6adc349205dbf0f7063a" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" default)))
 '(default-input-method "rfc1345")
 '(erc-format-nick-function (quote erc-format-@nick))
 '(erc-modules (quote (button completion fill irccontrols log match menu netsplit noncommands readonly ring scrolltobottom services stamp track)))
 '(erc-nickserv-alist (quote ((Foonetic "NickServ!services@foonetic.net" "This nickname is registered and protected\\.  If it is your nickname, type /msg NickServ IDENTIFY password\\.  Otherwise, please choose a different nickname\\." "NickServ" "IDENTIFY" nil nil) (Ars nil nil "Census" "IDENTIFY" nil nil) (Austnet "NickOP!service@austnet.org" "/msg\\s-NickOP@austnet.org\\s-identify\\s-<password>" "nickop@austnet.org" "identify" nil nil) (Azzurra "NickServ!service@azzurra.org" "/ns\\s-IDENTIFY\\s-password" "NickServ" "IDENTIFY" nil nil) (BitlBee nil nil "&bitlbee" "identify" nil nil) (BRASnet "NickServ!services@brasnet.org" "/NickServ\\s-IDENTIFY\\s-senha" "NickServ" "IDENTIFY" nil "") (DALnet "NickServ!service@dal.net" "/msg\\s-NickServ@services.dal.net\\s-IDENTIFY\\s-<password>" "NickServ@services.dal.net" "IDENTIFY" nil nil) (freenode "NickServ!NickServ@services." "/msg\\s-NickServ\\s-IDENTIFY\\s-<password>" "NickServ" "IDENTIFY" nil nil) (GalaxyNet "NS!nickserv@galaxynet.org" "Please\\s-change\\s-nicks\\s-or\\s-authenticate." "NS@services.galaxynet.org" "AUTH" t nil) (iip "Trent@anon.iip" "type\\s-/squery\\s-Trent\\s-identify\\s-<password>" "Trent@anon.iip" "IDENTIFY" nil "SQUERY") (OFTC "NickServ!services@services.oftc.net" "type\\s-/msg\\s-NickServ\\s-IDENTIFY\\s-password." "NickServ" "IDENTIFY" nil nil) (QuakeNet nil nil "Q@CServe.quakenet.org" "auth" t nil) (SlashNET "NickServ!services@services.slashnet.org" "/msg\\s-NickServ\\s-IDENTIFY\\s-password" "NickServ@services.slashnet.org" "IDENTIFY" nil nil))))
 '(org-agenda-files (quote ("~/org/todo.org")))
 '(org-agenda-ndays 21)
 '(org-agenda-show-all-dates t)
 '(org-agenda-skip-deadline-if-done t)
 '(org-agenda-skip-scheduled-if-done t)
 '(org-agenda-start-on-weekday nil)
 '(org-agenda-warning-days 14)
 '(org-capture-templates (quote (("s" "school todo" entry (file+headline "~/org/todo.org" "School") "* TODO %?
  %u" :prepend t) ("t" "todo" entry (file+headline "~/org/todo.org" "Uncategorized") "* TODO %?
  %u" :prepend t) ("i" "idea" entry (file+headline "~/org/todo.org" "Ideas") "* TODO %?
  %u" :prepend t) ("p" "project" entry (file+headline "~/org/todo.org" "Projects") "* TODO %?
  %u" :prepend t) ("r" "to-read" entry (file+headline "~/org/readinglist.org" "Uncategorized") "* TODO %?
  %u" :prepend t) ("n" "Note" entry (file+headline "~/org/notes/notes.org" "Uncategorized") "* %?
  %u" :prepend t))))
 '(org-default-notes-file "~/org/notes.org")
 '(org-priority-faces (quote ((65 . "red") (66 . "yellow") (67 . "blue"))))
 '(org-remember-store-without-prompt t)
 '(org-remember-templates (quote (("todo" 116 "* TODO %?
  %u" "~/org/todo.org" "Uncategorized" nil) ("idea" 105 "* TODO %?
  %u" "~/org/todo.org" "Ideas" nil) ("project" 112 "* TODO %?
  %u" "~/org/todo.org" "Projects" nil) ("to-read" 114 "* TODO %?
  %u" "~/org/readinglist.org" "Uncategorized" nil))))
 '(org-reverse-note-order t)
 '(show-paren-mode t nil (paren))
 '(solarized-degrade nil)
 '(solarized-italic nil)
 '(solarized-termcolors 256)
 '(tex-dvi-view-command (quote (cond ((eq window-system (quote x)) "evince") ((eq window-system (quote w32)) "yap") (t "dvi2tty * | cat -s"))))
 '(transient-mark-mode t))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                         ;;
;;  COMMANDS TO EXECUTE AT RUNTIME                                         ;;
;;                                                                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (color-theme-solarized-dark)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; enabled commands
(put 'downcase-region 'disabled nil)
(put 'scroll-left 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
