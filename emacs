;;;Copied from Neil Dantam, some modifications

(add-to-list 'load-path "~/.emacs.d")

(require 'cl)
;(require 'w3m-load)


;(add-to-list 'vc-handled-backends 'Git)


;;;;;;;;;;;;
;;  DEFS  ;;
;;;;;;;;;;;;

(defun all (args)
  (cond
   ((null args) t)
   ((car args) (all (cdr args)))
   (t nil)))

(defun any (args)
  (cond
   ((null args) nil)
   ((car args) t)
   (t (all (cdr args)))))

(defmacro when-host (name &rest forms)
  (declare (indent 1))
  `(when ,(if (atom name)
              `(string= (system-name) ,name)
            `(any (mapcar (lambda (name) 
                            (string= (system-name) name))
                          (quote ,name))))
     ,@forms))

;;;;;;;;;;;;;;;;
;;  SEMANTIC  ;;
;;;;;;;;;;;;;;;;
;(setq semantic-load-turn-everything-on t)
;(require 'semantic-load)
;(require 'semantic-ia)
;(add-hook 'c-mode-common-hook 
;          (lambda ()
;            (define-key c-mode-base-map (kbd "\C-c TAB") 
;                        'semantic-complete-analyze-inline)
;            (define-key c-mode-base-map (kbd "\C-c m") 
;                        'semantic-ia-complete-symbol-menu)))

;; Semantic projects

;(when-host "daneel"
;  (setq semanticdb-project-roots 
;        (list "~/cc/sparky/src")))


;(semantic-load-enable-code-helpers)

;(setq semantic-load-turn-useful-things-on t)


;;;;;;;;;;;;;;;;;;;
;;  GLOBAL KEYS  ;;
;;;;;;;;;;;;;;;;;;;
(global-set-key "\C-c\k" 'compile)
(global-set-key "\C-c\C-c" 'comment-region)
(global-set-key "\C-c\M-c" 'uncomment-region)
(global-set-key "\C-c#" 'server-start)
(global-set-key "\C-cbe" (lambda () (interactive) 
                           (switch-to-buffer "*eshell*")))
(global-set-key "\C-xvp" 'vc-update)

;;;;;;;;;;
;; MISC ;;
;;;;;;;;;;

(setq make-backup-files nil) 
(setq inhibit-startup-message t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(set-scroll-bar-mode nil)



;;;;;;;;;;;;;;;;;;;
;;  Remote File  ;;
;;;;;;;;;;;;;;;;;;;
(setq tramp-default-method "ssh")
;(add-to-list 'tramp-remote-path "~/bin")
;(pushnew "/opt/csw/bin" tramp-remote-path) ;niagara path to cvs

;;;;;;;;;;;;
;;  FONT  ;;
;;;;;;;;;;;;

;; (cond ((or (eq window-system 'x) (eq window-system 'w32))
;;        (set-face-background 'region                   "#555555")
;;        (set-face-foreground 'modeline             "white")
;;        (set-face-background 'modeline             "#333333")
;;        (set-background-color "black")
;;        (add-to-list 'default-frame-alist '(background-color . "black"))
;;        (set-foreground-color "green")
;;        (add-to-list 'default-frame-alist '(foreground-color . "green"))
;;        (set-cursor-color "green"))
;;       ((eq window-system 'pc))
;;       ((eq window-system 'nil)

       (set-face-background 'region                   "#555555")
       (set-face-foreground 'modeline             "#FFFFFF")
       (set-face-background 'modeline             "#333333")
       (set-background-color "black")
       (add-to-list 'default-frame-alist '(background-color . "black"))
       (set-foreground-color "green")
       (add-to-list 'default-frame-alist '(foreground-color . "green"))
       (set-cursor-color "green")

       ;; ))


(set-face-attribute 'default nil :height 100)
(mouse-wheel-mode t)
(add-to-list 'default-frame-alist '(background-mode . dark))
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)


;;;;;;;;;
;;  C  ;;
;;;;;;;;;

;; Other people are annoyed by emacs 2-space default
(add-hook 'c-mode-common-hook 
          (lambda ()
            (c-set-style "bsd")
            (setq c-basic-offset 4))) ; I've written to much java, 
                                        ; but then so have many other people...u 
;(setq c-mode-common-hook nil)



;;;;;;;;;;;;;;
;;  PYTHON  ;;
;;;;;;;;;;;;;;
;(push "~/src/elisp/python-mode" load-path)
;(load "~/src/elisp/python-mode/python-mode.el")
;(setq auto-mode-alist (cons '("\\.py$" . python-mode) auto-mode-alist))
;(setq interpreter-mode-alist (cons '("python" . python-mode)
;				   			interpreter-mode-alist))
;(autoload 'python-mode "python-mode" "Python editing mode." t)




;;;;;;;;;;;;
;;  TEXT  ;;
;;;;;;;;;;;;
(setq-default ispell-program-name "C:\\Program Files (x86)\\Aspell\\bin\\aspell.exe")
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)
;; (add-hook 'text-mode-hook 'flyspell-mode)


;;;;;;;;;;;;
;; SLIME  ;;
;;;;;;;;;;;;


(add-to-list 'load-path "/home/saul/src/slime/slime")
(setq inferior-lisp-program "/usr/local/bin/sbcl")
(require 'slime)
(slime-setup)


(add-hook 'slime-mode-hook
          (lambda ()
            (local-set-key "C-c C-c" slime-eval-buffer)
            (local-set-key "C-c d" slime-eval-buffer)
            (local-set-key "C-c C-d" slime-eval-defun)
            (local-set-key "C-c r" slime-eval-region)
            (local-set-key "C-c C-r" slime-eval-region)))


;;;;;;;;;;;;
;; CL  ;;
;;;;;;;;;;;;
(push '("\\.sbclrc$" . lisp-mode)  auto-mode-alist )
(push '("\\.asdf-install$" . lisp-mode) auto-mode-alist )
(push '("\\.asd$" . lisp-mode) auto-mode-alist )



;;;;;;;;;;;
;;  XML  ;;
;;;;;;;;;;;
;(autoload 'sgml-mode "psgml" "Major mode to edit SGML files." t)
;(autoload 'xml-mode "psgml" "Major mode to edit XML files." t)

(add-to-list 'auto-mode-alist
			 (cons (concat "\\." (regexp-opt '( "xml" "xsd"  "rng" "xslt" "svg" "rss") t) "\\'")
				   'nxml-mode))
(push '("\\.mxsl$" . nxml-mode) auto-mode-alist )


;;;;;;;;;
;; LUA ;;
;;;;;;;;;

(load "lua-mode" t)



;;;;;;;;;;
;; BNF ;;;
;;;;;;;;;;
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




;;;;;;;;;;;;
;;  TABS  ;;
;;;;;;;;;;;;
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)



;;;;;;;;;;;
;; UTF-8 ;;
;;;;;;;;;;;
(set-language-environment "UTF-8")
(set-buffer-file-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)

;;;;;;;;;;;;;;
;; ORG-MODE ;;
;;;;;;;;;;;;;;

(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

(setq org-agenda-files (list "/media/sharepart/configs/org/school.org"))



;;;;;;;;;;;
;; SHELL ;;
;;;;;;;;;;;
(setq explicit-shell-file-name "/bin/bash" )



;;;;;;;;;;;;;
;; FORTRAN ;;
;;;;;;;;;;;;;
(defun f90-return( ) (interactive) (f90-indent-line) (newline-and-indent))

(add-hook 'f90-mode-hook
          '(lambda()
             (local-set-key [13] 'f90-return)    ; RET with automatic indent
             (imenu-add-to-menubar "Program-Units") ; Add index of func. to menu bar
             ))

;;;;;;;;;;;;;
;; OCTAVE  ;;
;;;;;;;;;;;;;
;;;
;(autoload 'octave-mode "octave-mod" nil t)
(autoload 'run-octave "octave-inf" nil t)
(add-hook 'inferior-octave-mode-hook
          (lambda ()
            (abbrev-mode 1)
            (auto-fill-mode 1)
            (if (eq window-system 'x)
                (font-lock-mode 1))
					;(turn-on-font-lock)
            (define-key inferior-octave-mode-map [up]
              'comint-previous-input)
            (define-key inferior-octave-mode-map [down]
              'comint-next-input)))

(add-hook 'octave-mode-hook
          (lambda ()
            (run-octave)))

(setq auto-mode-alist
      (cons '("\\.m$" . octave-mode) auto-mode-alist))

;; (define-key octave-mode-map "\C-m"
;;   'octave-reindent-then-newline-and-indent)

(setq inferior-octave-startup-args `("-q"))

;;;;;;;;;;;;;;;;;;;
;; EMACS SERVER  ;;
;;;;;;;;;;;;;;;;;;;
(add-hook 'server-switch-hook
	  (lambda nil
	    (let ((server-buf (current-buffer)))
	      (bury-buffer)
	      (switch-to-buffer-other-frame server-buf))))
(add-hook 'server-done-hook 'delete-frame)
(add-hook 'server-done-hook (lambda nil (kill-buffer nil)))

;(server-start)


;;;;;;;;;
;; ERC ;;
;;;;;;;;;

(setq erc-auto-query 'frame) ; on PM, open in new frame.

(add-hook 'erc-after-connect ; same behavior for notices
          (lambda (server nick)
            (add-hook 'erc-server-NOTICE-hook 'erc-auto-query)))

(require 'erc-match)                    ; highlight on nick said
(setq erc-keywords '("vebyast"))

(defun my-add-erc-keys ()
  (local-set-key (kbd "<f7>") 'ispell-word))

(add-hook 'erc-mode-hook 'my-add-erc-keys)

(make-variable-buffer-local 'erc-fill-column) ;change fill width when the window changes size
(add-hook 'window-configuration-change-hook 
          '(lambda ()
             (save-excursion
               (walk-windows
                (lambda (w)
                  (let ((buffer (window-buffer w)))
                    (set-buffer buffer)
                    (when (eq major-mode 'erc-mode)
                      (setq erc-fill-column (- (window-width w) 2)))))))))

(load "~/.irc.el")              ;load passwords
(setq erc-nickserv-passwords
      `(("mibbit"        (("vebyast" . ,mibbit-nickone-pass)))
        ("foonetic"      (("vebyast" . ,foonetic-nickone-pass)))))
(add-hook 'erc-after-connect            ;use passwords to autoconnect
          '(lambda (SERVER NICK)
             (cond
              ((string-match "mibbit.net" SERVER)
               (erc-message "PRIVMSG" (concat "nickserv identify " mibbit-nickone-pass)))
              ((string-match "foonetic.net" SERVER)
               (erc-message "PRIVMSG" (concat "nickserv identify " foonetic-nickone-pass))))))

(load "~/.emacs.d/erc-highlight-nicknames.el") ; turn on nick coloring
(add-hook 'erc-join-hook 'erc-highlight-nicknames-enable)
          
(defun erc-cmd-TP (text)
  "/TP text - topic prepend, adds |"
  (let ((oldtopic channel-topic))
    (when (string-match "\\(.*\\)\C-o" oldtopic)
      (erc-cmd-TOPIC (concat text " | " (match-string 1 oldtopic))))))


;;;;;;;;;;;;;
;; MAXIMA  ;;
;;;;;;;;;;;;;
(setq load-path (cons  "/usr/share/maxima/5.21.1/emacs" load-path ))

(autoload 'maxima "maxima" "Running Maxima interactively" t)

(autoload 'maxima-mode "maxima" "Maxima editing mode" t)

(setq load-path (cons "/usr/share/maxima/5.9.0/emacs" load-path))
(autoload 'maxima-mode "maxima" "Maxima mode" t)
(autoload 'maxima "maxima" "Maxima interactive" t)
(setq auto-mode-alist (cons '("\.max" . maxima-mode) auto-mode-alist))
(autoload 'emaxima-mode "emaxima" "EMaxima" t)
(add-hook 'emaxima-mode-hook 'emaxima-mark-file-as-emaxima)


;;;;;;;;;;;;
;; CUSTOM ;;
;;;;;;;;;;;;

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(slime-highlight-edits-face ((((class color) (background dark)) (:background "#333")))))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(case-fold-search t)
 '(current-language-environment "English")
 '(default-input-method "rfc1345")
 '(erc-format-nick-function (quote erc-format-@nick))
 '(erc-modules (quote (button completion fill irccontrols log match menu netsplit noncommands readonly ring scrolltobottom services stamp track)))
 '(erc-nickserv-alist (quote ((Foonetic "NickServ!services@foonetic.net" "This nickname is registered and protected\\.  If it is your nickname, type /msg NickServ IDENTIFY password\\.  Otherwise, please choose a different nickname\\." "NickServ" "IDENTIFY" nil nil) (Ars nil nil "Census" "IDENTIFY" nil nil) (Austnet "NickOP!service@austnet.org" "/msg\\s-NickOP@austnet.org\\s-identify\\s-<password>" "nickop@austnet.org" "identify" nil nil) (Azzurra "NickServ!service@azzurra.org" "/ns\\s-IDENTIFY\\s-password" "NickServ" "IDENTIFY" nil nil) (BitlBee nil nil "&bitlbee" "identify" nil nil) (BRASnet "NickServ!services@brasnet.org" "/NickServ\\s-IDENTIFY\\s-senha" "NickServ" "IDENTIFY" nil "") (DALnet "NickServ!service@dal.net" "/msg\\s-NickServ@services.dal.net\\s-IDENTIFY\\s-<password>" "NickServ@services.dal.net" "IDENTIFY" nil nil) (freenode "NickServ!NickServ@services." "/msg\\s-NickServ\\s-IDENTIFY\\s-<password>" "NickServ" "IDENTIFY" nil nil) (GalaxyNet "NS!nickserv@galaxynet.org" "Please\\s-change\\s-nicks\\s-or\\s-authenticate." "NS@services.galaxynet.org" "AUTH" t nil) (iip "Trent@anon.iip" "type\\s-/squery\\s-Trent\\s-identify\\s-<password>" "Trent@anon.iip" "IDENTIFY" nil "SQUERY") (OFTC "NickServ!services@services.oftc.net" "type\\s-/msg\\s-NickServ\\s-IDENTIFY\\s-password." "NickServ" "IDENTIFY" nil nil) (QuakeNet nil nil "Q@CServe.quakenet.org" "auth" t nil) (SlashNET "NickServ!services@services.slashnet.org" "/msg\\s-NickServ\\s-IDENTIFY\\s-password" "NickServ@services.slashnet.org" "IDENTIFY" nil nil))))
 '(global-font-lock-mode t nil (font-lock))
 '(ispell-program-name "/usr/bin/aspell" t)
 '(js2-basic-offset 2)
 '(js2-bounce-indent-flag nil)
 '(js2-mirror-mode nil)
 '(show-paren-mode t nil (paren))
 '(tex-dvi-view-command (quote (cond ((eq window-system (quote x)) "evince") ((eq window-system (quote w32)) "yap") (t "dvi2tty * | cat -s"))))
 '(transient-mark-mode t))


;;;;;;;;;;;;;;;;
;; RUN ESHELL ;;
;;;;;;;;;;;;;;;;
;(eshell)
(put 'downcase-region 'disabled nil)
