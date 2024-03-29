

(setq prettify-symbols-unprettify-at-point t)

(defconst pragmatapro-font-lock-symbols-alist
  (mapcar (lambda (s)
            (cons (car s) (decode-char 'ucs (car (cdr s)))))
          (list '("!!" #XE720)
                '("!=" #XE721)
                '("!==" #XE722)
                '("!!!" #XE723)
                ;; '("!===" #XE724)
                ;; '("!===" #XE725)
                '("!>" #XE726)
                '("#(" #XE740)
                '("#_" #XE741)
                '("#{" #XE742)
                '("#?" #XE743)
                '("#>" #XE744)
                '("%=" #XE750)
                '("%>" #XE751)
                '("<~" #XE75F)
                '("&%" #XE760)
                '("&&" #XE761)
                '("&*" #XE762)
                '("&+" #XE763)
                '("&-" #XE764)
                '("&/" #XE765)
                '("&=" #XE766)
                '("&&&" #XE767)
                '("&>" #XE768)
                '("$>" #XE775)
                '("~>" #XE77F)
                '("***" #XE780)
                '("*=" #XE781)
                '("*/" #XE782)
                '("*>" #XE783)
                '("++" #XE790)
                '("+++" #XE791)
                '("+=" #XE792)
                '("+>" #XE793)
                '("--" #XE7A0)
                '("-<" #XE7A1)
                '("-<<" #XE7A2)
                '("-=" #XE7A3)
                '("->" #XE7A4)
                '("->>" #XE7A5)
                '("---" #XE7A6)
                '("-->" #XE7A7)
                '(".." #XE7B0)
                '("..." #XE7B1)
                '("..<" #XE7B2)
                '(".>" #XE7B3)
                '(".~" #XE7B4)
                '(".=" #XE7B5)
                '("/*" #XE7C0)
                '("//" #XE7C1)
                '("/>" #XE7C2)
                '("/=" #XE7C3)
                '("/==" #XE7C4)
                '("///" #XE7C5)
                '("/**" #XE7C6)
                '("::" #XE7D0)
                '(":=" #XE7D1)
                '(":=>" #XE7D4)
                '("<$>" #XE7E0)
                '("<*" #XE7E1)
                '("<*>" #XE7E2)
                '("<+>" #XE7E3)
                '("<-" #XE7E4)
                '("<<" #XE7E5)
                '("<<<" #XE7E6)
                '("<<=" #XE7E7)
                '("<=" #XE7E8)
                '("<=>" #XE7E9)
                '("<>" #XE7EA)
                '("<|>" #XE7EB)
                '("<<-" #XE7EC)
                '("<|" #XE7ED)
                '("<=<" #XE7EE)
                '("<~" #XE7EF)
                '("<~~" #XE7F0)
                '("<<~" #XE7F1)
                '("<$" #XE7F2)
                '("<+" #XE7F3)
                '("<!>" #XE7F4)
                '("<@>" #XE7F5)
                '("<#>" #XE7F6)
                '("<%>" #XE7F7)
                '("<^>" #XE7F8)
                '("<&>" #XE7F9)
                '("<?>" #XE7FA)
                '("<.>" #XE7FB)
                '("</>" #XE7FC)
                '("<\>" #XE7FD)
                '("<\">" #XE7FE)
                '("<:>" #XE7FF)
                '("<~>" #XE800)
                '("<**>" #XE801)
                '("<<^" #XE802)
                '("<!" #XE803)
                '("<@" #XE804)
                '("<#" #XE805)
                '("<%" #XE806)
                '("<^" #XE807)
                '("<&" #XE808)
                '("<?" #XE809)
                '("<." #XE80A)
                '("</" #XE80B)
                '("<\\" #XE80C)
                '("<\"" #XE80D)
                '("<:" #XE80E)
                '("<->" #XE80F)
                '("<!--" #XE810)
                '("<--" #XE811)
                '("==<" #XE820)
                '("==" #XE821)
                '("===" #XE822)
                '("==>" #XE823)
                '("=>" #XE824)
                '("=~" #XE825)
                '("=>>" #XE826)
                ;; '("" #XE830)
                ;; '("" #XE831)
                ;; '("" #XE832)
                '(">-" #XE840)
                '(">=" #XE841)
                '(">>" #XE842)
                '(">>-" #XE843)
                '(">==" #XE844)
                '(">>>" #XE845)
                '(">=>" #XE846)
                '(">>^" #XE847)
                '("??" #XE860)
                '("?~" #XE861)
                '("?=" #XE862)
                '("?>" #XE863)
                '("^=" #XE868)
                '("^." #XE869)
                '("^?" #XE86A)
                '("^.." #XE86B)
                '("^<<" #XE86C)
                '("^>>" #XE86D)
                '("^>" #XE86E)
                '("\\\\" #XE870)
                '("\\>" #XE871)
                '("@>" #XE877)
                '("|=" #XE880)
                '("||" #XE881)
                '("|>" #XE882)
                '("|||" #XE883)
                '("|+|" #XE884)
                '("~=" #XE890)
                '("~>" #XE891)
                '("~~>" #XE892)
                '("~>>" #XE893))))

(add-hook 'prog-mode-hook
          (lambda ()
            (dolist (alias pragmatapro-font-lock-symbols-alist)
              (push alias prettify-symbols-alist))))
