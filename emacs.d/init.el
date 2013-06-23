;;; this is where we'll save everything
(add-to-list 'load-path "~/.emacs.d")

;;; require common lisp
(require 'cl)

;;; First, load up some packages
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)                    ;load everything

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                         ;;
;;;  README                                                                 ;;
;;;                                                                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; things required to use this .emacs:
;;; 
;;; * install from package manager:
;;;   - emacs24
;;;     - ubuntu: ppa:cassou/emacs and emacs-snapshot
;;;   - cscope
;;;   - sbcl
;;;   - cl-swank
;;;   - w3m
;;;   - wl-beta
;;;   - gnutls-bin
;;; * install using package-list-packages:
;;;   - ascope
;;;   - auto-complete
;;;   - desktop-registry
;;;   - elpy
;;;   - find-file-in-project
;;;   - helm
;;;   - highlight-indentation
;;;   - idomenu
;;;   - kpm-list
;;;   - minimap
;;;   - nose
;;;   - paredit
;;;   - solarized-theme
;;;   - undo-tree
;;;   - virtualenv
;;;   - w3m
;;;   - yasnippet
;;; * install to src/ using git:
;;; * set up in other ways
;;;   - pip install --user elpy rope pyflakes pep8 virtualenvwrapper
;;;   - install swank and slime using quicklisp:
;;;       cd ~
;;;       wget http://beta.quicklisp.org/quicklisp.lisp -O /tmp/quicklisp.lisp
;;;       sbcl --load /tmp/quicklisp.lisp --eval '(quicklisp-quickstart:install :path ".quicklisp/")' --eval '(ql:quickload "quicklisp-slime-helper")' --eval '(ql:quickload "swank")' --eval '(ql:add-to-init-file)' --eval '(quit)'
;;; 
;;; other things that I like to have:
;;; * install from package manager:
;;; * install using package-list-packages:
;;;   - magit
;;; * install to src/ using git:
;;; * set up in other ways



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                         ;;
;;;  CONFIGURATION                                                          ;;
;;;                                                                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; behavior
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq make-backup-files nil)            ;no backups to clutter things up
(setq inhibit-startup-message t)        ;don't put up the splash page

;;; global settings for tabs - use spaces!
(setq-default indent-tabs-mode nil)
(setq-default tab-width 8)

;;; use utf-8 everywhere
(set-language-environment "UTF-8")
(set-buffer-file-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)

;;; put temporary files somewhere else
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

;;; save some stuff across sessions, most importantly the command
;;; history
(savehist-mode 1)
(setq savehist-additional-variables '(kill-ring search-ring regexp-search-ring))
(setq savehist-file "~/.emacs.d/tmp/savehist")

;;; save file-open history across sessions with slightly nicer
;;; behavior
(require 'recentf)
(setq recentf-auto-cleanup 'never)
(recentf-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; appearance ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;; I give up. I'll just toggle the theme manually whenever I need to.
(defun toggle-theme (th)
  (if (member th custom-enabled-themes)
      (disable-theme th)
    (load-theme th)))

(global-set-key (kbd "C-z")
                (lambda () (interactive) (toggle-theme 'solarized-dark)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; keybindings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; better compiling - make it easy to reuse the last line without
;;; having to renavigate to the build directory
(global-set-key (kbd "C-c k") 'compile)
(global-set-key (kbd "C-c C-k") 'recompile)

;;; We're using emacsclient exclusively, so we want the user to be
;;; slightly more aware of what's happening - use <C-c #>, <C-x 5 0>,
;;; or 'save-buffers-kill-emacs' as appropriate.
(global-unset-key (kbd "C-x C-c"))

;;; we want to regexp-search by default because regexp search is
;;; awesome.
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

;;; bind recentf
(global-set-key (kbd "C-x f") 'recentf-open-files)

;;; properly handle control-arrows in the terminal
(define-key input-decode-map (kbd "M-[ A") [C-up])
(define-key input-decode-map (kbd "M-[ B") [C-down])
(define-key input-decode-map (kbd "M-[ C") [C-right])
(define-key input-decode-map (kbd "M-[ D") [C-left])








;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                         ;;
;;;  IMPROVEMENTS                                                           ;;
;;;                                                                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; kpm-list ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; improves the buffer list by trying to group intelligently
(require 'kpm-list)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; emacs-helm ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; improves buffer-switching, autocomplete, and a lot of other things.

;;; (require 'helm-config)
;;; (global-set-key (kbd "C-c h") 'helm-mini)
;;; (global-set-key (kbd "C-c C-h") 'helm-mini)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; iswitchb ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; improves buffer-switching
(iswitchb-mode)
(defun iswitchb-local-keys ()
  (mapc (lambda (K)
          (let* ((key (car K)) (fun (cdr K)))
            (define-key iswitchb-mode-map (kbd key) fun)))
        '(("C-b" . iswitchb-prev-match)
          ("C-f" . iswitchb-next-match))))
(add-hook 'iswitchb-define-mode-map-hook 'iswitchb-local-keys)
(setq iswitchb-buffer-ignore '(" *Minibuf-1*" "*elpy-rpc*"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ido mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq ido-enable-flex-matching t)
(ido-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; undo-tree ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(undo-tree-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; desktop registry ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(desktop-registry-auto-register)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                         ;;
;;;  ADDITIONS                                                              ;;
;;;                                                                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; user functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; use wmctrl to set X fullscreen
(defun switch-full-screen ()
  (interactive)
  (shell-command "wmctrl -r :ACTIVE: -btoggle,fullscreen"))
(global-set-key [f11] 'switch-full-screen)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; org-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load "org-rc.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Emacs IRC Client ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; to use this, you'll need to create an ~/.irc.el file that, when
;;; executed, populates a list of irc servers passwords.

;; ; change fill width when the window changes size
;; (make-variable-buffer-local 'erc-fill-column)
;; (add-hook 'window-configuration-change-hook 
;;           '(lambda ()
;;              (save-excursion
;;                (walk-windows
;;                 (lambda (w)
;;                   (let ((buffer (window-buffer w)))
;;                     (set-buffer buffer)
;;                     (when (eq major-mode 'erc-mode)
;;                       (setq erc-fill-column (- (window-width w) 2)))))))))

;; (load "~/.irc.el")              ;load passwords

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Git ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Produce syntax highlighting in git commit mode
(require 'git-commit)
;;; (load "~/.emacs.d/magit-commit.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; cscope ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Produce syntax highlighting in git commit mode
(require 'ascope)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; w3m ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'w3m)
(require 'mime-w3m)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; bbdb ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; (setq bbdb-file "~/.emacs.d/bbdb")
;;; (require 'bbdb)
;;; (bbdb-initialize)

;;; ;; save without asking
;;; (setq bbdb-offer-save 1)

;;; ;; popups
;;; (setq bbdb-pop-up-window-size 2)

;;; ;; interactive?

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; paredit ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; handle parentheses in lispy languages better
(when (require 'paredit nil 'noerror)
  (add-hook 'emacs-lisp-mode-hook (lambda () (paredit-mode 1)))
  (add-hook 'slime-mode-hook (lambda () (paredit-mode 1)))
  (add-hook 'slime-repl-mode-hook (lambda () (paredit-mode 1)))
  (add-hook 'lisp-mode-hook (lambda () (paredit-mode 1)))
  (add-hook 'lisp-interaction-mode-hook (lambda () (paredit-mode 1)))
  (add-hook 'scheme-mode-hook (lambda () (paredit-mode 1))))


;; Stop SLIME's REPL from grabbing DEL, which is annoying when
;; backspacing over a '('
(add-hook 'slime-repl-mode-hook
          (lambda ()
            (define-key slime-repl-mode-map (read-kbd-macro paredit-backward-delete-key) nil)))

;;; (add-hook 'paredit-mode-hook
;;;           (lambda ()
;;;             (local-set-key "C-c C-c" slime-eval-buffer)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; minimap ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'minimap)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                         ;;
;;;  LANGUAGES                                                              ;;
;;;                                                                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; C ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; use 4 spaces by default, select formatting style
(add-hook 'c-mode-common-hook 
          (lambda ()
            (c-set-style "bsd") ; I've written too much java
            (setq c-basic-offset 4)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; C++ ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(push '("\\.ino$" . c++-mode)  auto-mode-alist)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; xml ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(push '("\\.urdf$" . nxml-mode)  auto-mode-alist)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; haskell ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-decl-scan)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; common lisp ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(push '("\\.sbclrc$" . lisp-mode)  auto-mode-alist )
(push '("\\.asdf-install$" . lisp-mode) auto-mode-alist )
(push '("\\.asd$" . lisp-mode) auto-mode-alist )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; backus-naur form  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; octave-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; start octave without the giant blurb thing
(setq inferior-octave-startup-args `("-q"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; common lisp  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load (expand-file-name "~/.quicklisp/slime-helper.el"))
(setq inferior-lisp-program "sbcl --noinform --no-linedit")

(eval-after-load "slime"
  '(progn
     (slime-setup '(;slime-asdf
                    ;slime-autodoc
                    ;slime-editing-commands
                    ;; slime-fancy
                    ;slime-fontifying-fu
                    ;slime-fuzzy
                    ;; slime-indentation
                    ;; slime-mdot-fu
                    ;slime-package-fu
                    ;slime-references
                    slime-repl
                    ;; slime-sbcl-exts
                    ;slime-scratch
                    ;slime-xref-browser
                    ))))

(require 'slime)
(slime-setup)

(add-hook 'slime-mode-hook
          (lambda ()
            (local-set-key "C-c C-c" slime-eval-buffer)
            (local-set-key "C-c d" slime-eval-buffer)
            (local-set-key "C-c C-d" slime-eval-defun)
            (local-set-key "C-c r" slime-eval-region)
            (local-set-key "C-c C-r" slime-eval-region)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; python  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(elpy-enable)
;;; (setq python-check-command "pylint")
;;; (setq python-check-command "pep8")
(elpy-clean-modeline)
(elpy-use-ipython)





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                         ;;
;;;  PER-MACHINE                                                            ;;
;;;                                                                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; lanning ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (string= system-name "lanning.lan")
  (setq ack-executable "/usr/local/bin/ack")
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; hermes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (string= system-name "hermes.lan")
  )


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
 '(minimap-semantic-function-face ((t (:inherit (font-lock-function-name-face minimap-font-face) :background "gray10" :box (:line-width 1 :color "white") :height 2.5))))
 '(slime-highlight-edits-face ((((class color) (background dark)) (:background "#333"))) t))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(case-fold-search t)
 '(column-number-mode t)
 '(current-language-environment "English")
 '(custom-safe-themes
   (quote
    ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "501caa208affa1145ccbb4b74b6cd66c3091e41c5bb66c677feda9def5eab19c" "7b4a6cbd00303fc53c2d486dfdbe76543e1491118eba6adc349205dbf0f7063a" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" default)))
 '(default-input-method "rfc1345")
 '(desktop-path (quote ("~/.emacs.d/" "~" "~/Desktop")))
 '(desktop-registry-registry
   (quote
    (("tech-13-macgyver-graph" . "/home/saul/src/humanoids/papers/2013/tech-13-macgyver-graph"))))
 '(elpy-default-minor-modes
   (quote
    (eldoc-mode flymake-mode yas-minor-mode auto-complete-mode)))
 '(erc-auto-query (quote window))
 '(erc-beep-match-types (quote (current-nick)))
 '(erc-format-nick-function (quote erc-format-@nick))
 '(erc-highlight-nicknames-mode t)
 '(erc-identd-mode nil)
 '(erc-match-mode t)
 '(erc-modules
   (quote
    (button completion fill irccontrols log match menu netsplit networks noncommands readonly ring scrolltobottom services stamp track truncate notifications highlight-nicknames)))
 '(erc-nick "Vebyast")
 '(erc-nickserv-alist
   (quote
    ((Mibbit "NickServ!services@mibbit.net" "" "" "" nil nil nil)
     (Foonetic "NickServ@services.foonetic.net" "/msg[:space:]*NickServ[:space:]*identify[:space:]*<password>" "NickServ" "identify" nil nil nil)
     (Ars nil nil "Census" "IDENTIFY" nil nil nil)
     (Austnet "NickOP!service@austnet.org" "/msg\\s-NickOP@austnet.org\\s-identify\\s-<password>" "nickop@austnet.org" "identify" nil nil nil)
     (Azzurra "NickServ!service@azzurra.org" "/ns\\s-IDENTIFY\\s-password" "NickServ" "IDENTIFY" nil nil nil)
     (BitlBee nil nil "&bitlbee" "identify" nil nil nil)
     (BRASnet "NickServ!services@brasnet.org" "/NickServ\\s-IDENTIFY\\s-senha" "NickServ" "IDENTIFY" nil "" nil)
     (DALnet "NickServ!service@dal.net" "/msg\\s-NickServ@services.dal.net\\s-IDENTIFY\\s-<password>" "NickServ@services.dal.net" "IDENTIFY" nil nil nil)
     (freenode "NickServ!NickServ@services." "/msg\\s-NickServ\\s-IDENTIFY\\s-<password>" "NickServ" "IDENTIFY" nil nil nil)
     (GalaxyNet "NS!nickserv@galaxynet.org" "Please\\s-change\\s-nicks\\s-or\\s-authenticate." "NS@services.galaxynet.org" "AUTH" t nil nil)
     (iip "Trent@anon.iip" "type\\s-/squery\\s-Trent\\s-identify\\s-<password>" "Trent@anon.iip" "IDENTIFY" nil "SQUERY" nil)
     (OFTC "NickServ!services@services.oftc.net" "type\\s-/msg\\s-NickServ\\s-IDENTIFY\\s-password." "NickServ" "IDENTIFY" nil nil nil)
     (QuakeNet nil nil "Q@CServe.quakenet.org" "auth" t nil nil)
     (SlashNET "NickServ!services@services.slashnet.org" "/msg\\s-NickServ\\s-IDENTIFY\\s-password" "NickServ@services.slashnet.org" "IDENTIFY" nil nil nil))))
 '(erc-nickserv-identify-mode (quote both))
 '(erc-notifications-mode t)
 '(erc-paranoid t)
 '(erc-prompt-for-nickserv-password nil)
 '(erc-server-reconnect-attempts 0)
 '(erc-text-matched-hook (quote (erc-log-matches erc-beep-on-match)))
 '(erc-truncate-mode t)
 '(erc-user-full-name "Vebyast Kranm")
 '(erc-verbose-server-ping nil)
 '(gdb-many-windows t)
 '(helm-ff-auto-update-initial-value nil)
 '(ido-everywhere nil)
 '(ido-mode (quote file) nil (ido))
 '(inhibit-eol-conversion nil)
 '(iswitchb-default-method (quote samewindow))
 '(minimap-dedicated-window t)
 '(minimap-window-location (quote right))
 '(org-agenda-files (quote ("~/org/todo.org" "~/.mobileorg/from-mobile.org")))
 '(org-agenda-include-diary t)
 '(org-agenda-ndays 21)
 '(org-agenda-show-all-dates t)
 '(org-agenda-skip-deadline-if-done t)
 '(org-agenda-skip-scheduled-if-done t)
 '(org-agenda-start-on-weekday nil)
 '(org-agenda-warning-days 14)
 '(org-capture-templates
   (quote
    (("s" "school todo" entry
      (file+headline "~/org/todo.org" "School")
      "* TODO %?
  %u" :prepend t)
     ("t" "todo" entry
      (file+headline "~/org/todo.org" "Uncategorized")
      "* TODO %?
  %u" :prepend t)
     ("m" "Money Todo" entry
      (file+headline "~/org/todo.org" "Money")
      "* TODO %?
  %u")
     ("i" "idea" entry
      (file+headline "~/org/ideas.org" "Ideas")
      "* IDEA %?
  %u" :prepend t)
     ("p" "project" entry
      (file+headline "~/org/todo.org" "Projects")
      "* TODO %?
  %u" :prepend t)
     ("r" "to-read" entry
      (file+headline "~/org/readinglist.org" "Uncategorized")
      "* RECOMMENDED %?" :prepend t)
     ("n" "Note" entry
      (file+headline "~/org/notes/notes.org" "Uncategorized")
      "* %?
  %u" :prepend t)
     ("w" "Wait" entry
      (file+headline "~/org/todo.org" "Wait")
      "* TODO %?
  SCHEDULED: <%(org-read-date nil nil \"+24h\")>
  %u")
     ("b" "To buy at store" entry
      (file "~/org/shopping-list.org")
      "* %?"))))
 '(org-default-notes-file "~/org/notes.org")
 '(org-mobile-files (quote (org-agenda-files "~/org/readinglist.org")))
 '(org-priority-faces (quote ((65 . "red") (66 . "yellow") (67 . "blue"))))
 '(org-remember-store-without-prompt t)
 '(org-remember-templates
   (quote
    (("todo" 116 "* TODO %?
  %u" "~/org/todo.org" "Uncategorized" nil)
     ("idea" 105 "* TODO %?
  %u" "~/org/todo.org" "Ideas" nil)
     ("project" 112 "* TODO %?
  %u" "~/org/todo.org" "Projects" nil)
     ("to-read" 114 "* TODO %?
  %u" "~/org/readinglist.org" "Uncategorized" nil))))
 '(org-reverse-note-order t)
 '(password-cache-expiry nil)
 '(show-paren-mode t nil (paren))
 '(solarized-degrade nil)
 '(solarized-italic nil)
 '(solarized-termcolors 256)
 '(tex-dvi-view-command
   (quote
    (cond
     ((eq window-system
          (quote x))
      "evince")
     ((eq window-system
          (quote w32))
      "yap")
     (t "dvi2tty * | cat -s"))))
 '(transient-mark-mode t)
 '(virtualenv-root "~/.python-virtualenvs/")
 '(wl-draft-config-alist
   (quote
    (("^From: .*saulrh@gatech.edu"
      (wl-smtp-authenticate-type . "plain")
      (wl-smtp-posting-user . "sarh3")
      (wl-smtp-posting-port . 465)
      (wl-smtp-posting-server . "mail.gatech.edu")
      (wl-smtp-connection-type quote ssl))
     ("^From: .*sreynoldshaertle@gmail.com"
      (wl-smtp-authenticate-type . "login")
      (wl-smtp-posting-user . "sreynoldshaertle")
      (wl-smtp-posting-server . "smtp.gmail.com")
      (wl-local-domain . "gmail.com")
      (wl-smtp-posting-port . 587)
      (wl-smtp-connection-type quote starttls)
      (wl-message-id-domain . "smtp.gmail.com"))
     ("^From: .*vebyast@gmail.com"
      (wl-smtp-posting-user . "vebyast")
      (wl-smtp-posting-server . "smtp.gmail.com")
      (wl-smtp-posting-port . 587)
      (wl-smtp-connection-type quote starttls)
      (wl-smtp-authenticate-type . "login")
      (wl-local-domain . "gmail.com")
      (wl-message-id-domain . "smtp.gmail.com")))))
 '(wl-template-alist
   (quote
    (("sreynoldshaertle@gmail"
      (wl-from . "Saul Reynolds-Haertle <sreynoldshaertle@gmail.com>")
      ("From" . wl-from))
     ("saulrh@gatech.edu"
      (wl-from . "Saul Reynolds-Haertle <saulrh@gatech.edu>")
      ("From" . wl-from))
     ("vebyast@gmail.com"
      (wl-from . "Vebyast Kranm <vebyast@gmail.com>")
      ("From" . wl-from))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                         ;;
;;;  COMMANDS TO EXECUTE AT RUNTIME                                         ;;
;;;                                                                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; enabled commands
(put 'downcase-region 'disabled nil)
(put 'scroll-left 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
