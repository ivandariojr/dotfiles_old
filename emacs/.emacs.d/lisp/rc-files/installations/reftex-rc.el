;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; RefTEx ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq reftex-plug-into-AUCTeX t)

;;; Add Reftex bibliography link change path on New 
;;; So that RefTeX finds my bibliography
(setq reftex-default-bibliography '("/home/ivan/Documents/Latex_Stuff/BibDB/AllEntries.bib"))

;; So that RefTeX also recognizes \addbibresource. Note that you
;; can't use $HOME in path for \addbibresource but that "~"
;; works.
(setq reftex-bibliography-commands '("bibliography" "nobibliography" "addbibresource"))

