;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                         ;;
;;  Initial config                                                         ;;
;;                                                                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(autoload 'wl "wl" "Wanderlust" t)
(autoload 'wl-draft "wl-draft" "Write draft with Wanderlust." t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                         ;;
;;  Behavior config                                                        ;;
;;                                                                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Use "Fwd: " instead of "Forward: "
(setq wl-forward-subject-prefix "Fwd: ")

;; Don't split large attachments
(setq mime-edit-split-message nil)

;; Add key for going to drafts folder
(define-key wl-folder-mode-map "\C-c\C-d" 'wl-folder-goto-draft-folder)

;; remap the keys for template selection a bit
(define-key wl-template-mode-map "\C-n" 'wl-template-next)
(define-key wl-template-mode-map "\C-p" 'wl-template-prev)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                         ;;
;;  Display config                                                         ;;
;;                                                                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; configure header display
(setq wl-message-ignored-field-list
      '(".*Received:"
        ".*Path:"
        ".*Id:"
        "^References:"
        "^Replied:"
        "^Errors-To:"
        "^Lines:"
        "^Sender:"
        ".*Host:"
        "^Xref:"
        "^Content-Type:"
        "^Precedence:"
        "^Status:"
        "^X-VM-.*:"
        "^X-.*:"
        "^List-Unsubscribe:"
        "^List-Subscribe:"
        "^List-Help"
        "^List-Archive:"
        "^List-Owner:"
        "^MIME-Version:"
        "^Thread-Index:"
        "^Message-ID:"
        "^Accept-Language:"
        "^Thread-Topic:"
        "^Content-Transfer-Encoding:"
        "^Content-Language:"
        "^Content-Disposition:"
        "^Content-Class:"
        "^User-Agent:"
        "^DomainKey-Signature:"
        "^In-Reply-To:"
        "^DKIM-Signature:"
        "^Received-SPF:"
        "^Mailing-list:"
        "^Delivered-To:"
        "^Authentication-Results:"
        "^Auto-Submitted:"
        "^Bounces-to:"))
(setq wl-message-visible-field-list
      '("^Dnas.*:"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                         ;;
;;  BBDB                                                                   ;;
;;                                                                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;; configure BBDB
;; (require 'bbdb-wl)

;; ;; ;; only get addresses from these folders:
;; ;; (seq bbdb-wl-folder-regexp

;; (define-key wl-draft-mode-map (kbd "<C-tab>" 'bbdb-complete-name))     

;; Local Variables:
;; mode: lisp
;; End:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                         ;;
;;  Gravatar integration                                                   ;;
;;                                                                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'wl-gravatar)
(setq wl-highlight-x-face-function 'wl-gravatar-insert)
(setq gnus-gravatar-directory "~/.emacs-gravatar/")
(setq gravatar-unregistered-icon 'identicon)
(setq wl-gravatar-retrieve-once t)
