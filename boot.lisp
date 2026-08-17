(let ((lookahd    (cons 0 0))
      (lblcnt     (cons 0 0))
      (macenv     (cons 0 0))
      (cmpenv     (cons 0 0))
      (globtab    (cons 0 0))
      (lams       (cons 0 0))
      (irbuf      (cons 0 0)) ;; Our in-memory IR list!
      (strtab     (cons 0 0)) 
      (ismode     (cons 0 0)) ;; 1 = AOT/ELF Mode, 0 = JIT Mode

      ;; --- 1. Utilities ---
      (nxtchar (lambda () (let ((c (peek lookahd))) (cond (c (poke lookahd 0) c) (1 (getchar))))))
      (ungtchar (lambda (c) (poke lookahd c)))
      (skpcom (lambda () (let ((c (nxtchar))) (cond ((or (eql c 10) (eql c 0)) (skpws)) (1 (skpcom))))))
      (skpws  (lambda () (let ((c (nxtchar))) (cond ((isspace c) (skpws)) ((eql c 59)  (skpcom)) (1 (ungtchar c))))))
      (getid (lambda () (let ((id (peek lblcnt))) (poke lblcnt (add id 1)) id)))

      (revlist (lambda (lst acc)
        (cond ((eql lst 0) acc)
              (1 (revlist (cdr lst) (cons (car lst) acc))))))

      ;; --- 2. Lexer/Parser ---
      (prsint (lambda (acc)
        (let ((c (nxtchar)))
          (cond ((isdigit c) (prsint (add (mul acc 10) (sub c 48))))
                (1 (ungtchar c) acc)))))
      (prsnumc (lambda (acc shift)
        (let ((c (nxtchar)))
          (cond ((isdelim c) (ungtchar c) acc)
                ((ge shift 64) (prsnumc acc shift))
                (1 (prsnumc (logior acc (ash c shift)) (add shift 8)))))))
      (prssym (lambda () (prsnumc 0 0)))
      (prsstrl (lambda (ptr count)
        (let ((c (nxtchar)))
          (cond
            ((or (eql c 34) (eql c 0))
             (let ((aligncnt (mul (div (add count 8) 8) 8)))
               (padzeros ptr count aligncnt)
               (alloc (div aligncnt 8))
               (cons ptr count)))
            ((eql c 92)
             (let ((esc (nxtchar)))
               (cond ((eql esc 110) (pokebyte (add ptr count) 10))
                     ((eql esc 116) (pokebyte (add ptr count) 9))
                     ((eql esc 114) (pokebyte (add ptr count) 13))
                     ((eql esc 92)  (pokebyte (add ptr count) 92))
                     ((eql esc 34)  (pokebyte (add ptr count) 34))
                     (1 (pokebyte (add ptr count) esc)))
               (prsstrl ptr (add count 1))))
            (1 (pokebyte (add ptr count) c) (prsstrl ptr (add count 1)))))))
      (prsstr (lambda () (prsstrl (getheap) 0)))
      (prslist (lambda () (skpws)
        (let ((c (nxtchar)))
          (cond ((or (eql c 41) (eql c 0)) 0)
                (1 (ungtchar c) (cons (prsexpr) (prslist)))))))
      (prsexpr (lambda () (skpws)
        (let ((c (nxtchar)))
          (cond ((eql c 40)  (cons 2 (prslist)))
                ((eql c 34)  (cons 3 (prsstr)))
                ((eql c 39)  (cons 2 (cons (cons 1 435745158513) (cons (prsexpr) 0))))
                ((isdigit c) (ungtchar c) (cons 0 (prsint 0)))
                ((eql c 45)  (cons 0 (sub 0 (prsint 0))))
                (1 (ungtchar c) (cons 1 (prssym)))))))

      ;; --- 3. AST Macro Expander ---

      ;; Wraps a raw AST node in (quote <node>) so the JIT treats it as data!
      (qtast (lambda (ast) 
        (cons 2 (cons (cons 1 'quote) (cons ast 0)))))

      ;; Builds the binding list: ((param1 (quote arg1)) (param2 (quote arg2)))
      (bndjit (lambda (params args)
        (cond (params 
               (cons (cons 2 (cons (cons 1 (cdr (car params))) (cons (qtast (car args)) 0))) 
                     (bndjit (cdr params) (cdr args))))
              (1 0))))
      
      (lkmac (lambda (symint env)
        (cond ((eql env 0) 0)
              (1 (let ((binding (sfcar env)))
                   (cond ((eql symint (car binding)) (cdr binding))
                         (1 (lkmac symint (cdr env)))))))))
      (bndmac (lambda (params args env)
        (cond (params (cons (cons (cdr (car params)) (car args)) (bndmac (cdr params) (cdr args) env)))
              (1 env))))
      (lkenv (lambda (symint env)
        (cond ((eql env 0) 0)
              (1 (let ((binding (sfcar env)))
                   (cond ((eql symint (car binding)) (cdr binding))
                         (1 (lkenv symint (cdr env)))))))))
      ;; (evlet (lambda (bnds env)
      ;;   (cond ((eql bnds 0) env)
      ;;         (1 (let ((bndlst (cdr (car bnds))))
      ;;              (evlet (cdr bnds) (cons (cons (cdr (car bndlst)) (evalast (car (cdr bndlst)) env)) env)))))))
      ;; (evalast (lambda (ast env)
      ;;   (cond
      ;;     ((eql ast 0) 0)
      ;;     (1 (let ((tag (car ast)) (val (cdr ast)))
      ;;          (cond
      ;;            ((eql tag 0) val) ((eql tag 3) val) ((eql tag 1) (lkmac val env))
      ;;            ((eql tag 2)
      ;;             (let ((funcname (cdr (car val))) (args (cdr val)))
      ;;               (cond
      ;;                 ((symeq funcname "quote") (car args))
      ;;                 ((symeq funcname "car")   (car (evalast (car args) env)))
      ;;                 ((symeq funcname "cdr")   (cdr (evalast (car args) env)))
      ;;                 ((symeq funcname "cons")  (cons (evalast (car args) env) (evalast (car (cdr args)) env)))
      ;;                 ((symeq funcname "eql")   (cond ((eql (evalast (car args) env) (evalast (car (cdr args)) env)) 1) (1 0)))
      ;;                 ((symeq funcname "if")    (cond ((evalast (car args) env) (evalast (car (cdr args)) env)) (1 (evalast (car (cdr (cdr args))) env))))
      ;;                 ((symeq funcname "add") (add (evalast (car args) env) (evalast (car (cdr args)) env)))
      ;;                 ((symeq funcname "sub") (sub (evalast (car args) env) (evalast (car (cdr args)) env)))
      ;;                 ((symeq funcname "mul") (mul (evalast (car args) env) (evalast (car (cdr args)) env)))
      ;;                 ((symeq funcname "div") (div (evalast (car args) env) (evalast (car (cdr args)) env)))
      ;;                 ((symeq funcname "ash") (ash (evalast (car args) env) (evalast (car (cdr args)) env)))
      ;;                 ((symeq funcname "let") (evalast (car (cdr args)) (evlet (cdr (car args)) env)))
      ;;                 (1 0))))
      ;;            (1 0)))))))
      (normlst (lambda (lst) (cond ((eql lst 0) 0) (1 (cons (normast (car lst)) (normlst (cdr lst)))))))
      (normast (lambda (ast) (cond ((eql ast 0) 0) ((gt (car ast) 3) (cons 2 (normlst ast))) ((eql (car ast) 2) (cons 2 (normlst (cdr ast)))) (1 ast))))
      (ismacro (lambda (ast) (cond ((eql (car ast) 2) (let ((func (car (cdr ast)))) (cond ((eql (car func) 1) (symeq (cdr func) "macro")) (1 0)))) (1 0))))
      (mapxnd (lambda (lst) (cond ((eql lst 0) 0) (1 (let ((expd (expand (car lst)))) (cond ((eql expd 0) (mapxnd (cdr lst))) (1 (cons expd (mapxnd (cdr lst))))))))))
      (xndletb (lambda (bnds)
        (cond ((eql bnds 0) 0)
              (1 (let ((bndlst (cdr (car bnds))))
                   (let ((sym (car bndlst)) (val (car (cdr bndlst))))
                     (cond
                       ((ismacro val)
                        (let ((valist (cdr val)))
                          (let ((macargs (cdr (car (cdr valist)))) (macbody (car (cdr (cdr valist)))))
                            (poke macenv (cons (cons (cdr sym) (cons macargs macbody)) (peek macenv)))
                            (xndletb (cdr bnds)))))
                       (1 (cons (cons 2 (cons sym (cons (expand val) 0))) (xndletb (cdr bnds)))))))))))
      (xndcnd (lambda (clauses)
        (cond ((eql clauses 0) 0)
              (1 (let ((clauselst (cdr (car clauses))))
                   (cons (cons 2 (cons (expand (car clauselst)) (mapxnd (cdr clauselst)))) (xndcnd (cdr clauses))))))))

      (expand (lambda (ast)
        (cond
          ((eql ast 0) 0)
          (1 (let ((tag (car ast)) 
                   (val (cdr ast)))
               (cond
                 ((eql tag 2)
                  (let ((func (car val)) 
                        (args (cdr val)))
                    (cond
                      ((eql (car func) 1)
                       (let ((fname (cdr func)))
                         (cond
                           ((lkmac fname (peek macenv))
                            
                            ;; 1. Sequential binding allows letast to instantly use mdef!
                            (let ((mdef   (lkmac fname (peek macenv)))
                                  (letast (cons 2 (cons (cons 1 'let) 
                                                  (cons (cons 2 (bndjit (car mdef) args)) 
                                                        (cdr mdef)))))
                                  (oldir  (peek irbuf)))
                              
                              ;; 2. Isolate state mutations in the body
                              (poke irbuf 0)
                              (emitir (cons (cons 'enter 0) 0))
                              (cmpprog letast)
                              (emitir (cons (cons 'ret 0) 0))
                              
                              ;; 3. Sequentially bind execution dependencies
                              (let ((finlir (revlist (peek irbuf) 0))
                                    (exemem (syscall 9 0 65536 7 34 -1 0))
                                    (labels (jitsizes finlir (dict) 0 0)))
                                
                                (emitjit finlir (dict) labels exemem 0)
                                
                                ;; 4. Execute, cleanup, and recurse!
                                (let ((jitptr (exemem)))
                                  (poke irbuf oldir)
                                  (syscall 11 exemem 65536) ;; sys_munmap prevents the leak!
                                  (expand (normast jitptr))))))
                                  
                           ;; ((symeq fname "defmacro")
                           ;;  (let ((msig (cdr (car args))))
                           ;;    (poke macenv (cons (cons (cdr (car msig)) (cons (cdr msig) (car (cdr args)))) (peek macenv))) 
                           ;;    0))
                           ((symeq fname "let")    (cons 2 (cons func (cons (cons 2 (xndletb (cdr (car args)))) (mapxnd (cdr args))))))
                           ((symeq fname "cond")   (cons 2 (cons func (xndcnd args))))
                           ((symeq fname "lambda") (cons 2 (cons func (cons (car args) (mapxnd (cdr args))))))
                           ((symeq fname "quote")  ast)
                           (1 (cons 2 (cons func (mapxnd args)))))))
                      (1 (cons 2 (mapxnd val))))))
                 (1 ast)))))))

      ;; (expand (lambda (ast)
      ;;   (cond
      ;;     ((eql ast 0) 0)
      ;;     (1 (let ((tag (car ast)) (val (cdr ast)))
      ;;          (cond
      ;;            ((eql tag 2)
      ;;             (let ((func (car val)) (args (cdr val)))
      ;;               (cond
      ;;                 ((eql (car func) 1)
      ;;                  (let ((fname (cdr func)))
      ;;                    (cond
      ;;                      ((lkmac fname (peek macenv))
      ;;                       (let ((mdef (lkmac fname (peek macenv))))
      ;;                         (let ((letast (cons 2 (cons (cons 1 'let) 
      ;;                                                     (cons (cons 2 (bndjit (car mdef) args)) 
      ;;                                                           (cdr mdef))))))
      ;;                           ;; Save IR state
      ;;                           (let ((oldir (peek irbuf)))
      ;;                             (poke irbuf 0)
      ;;                             (emitir (cons (cons 'enter 0) 0))
      ;;                             (cmpprog letast)
      ;;                             (emitir (cons (cons 'ret 0) 0))
      ;;                             
      ;;                             ;; JIT Compile and Execute the Macro!
      ;;                             (let ((finalir (revlist (peek irbuf) 0)))
      ;;                               (let ((exemem (syscall 9 0 65536 7 34 -1 0)))
      ;;                                 (let ((labels (jitsizes finalir (dict) 0 0)))
      ;;                                   (emitjit finalir (dict) labels exemem 0)
      ;;                                   (let ((jitptr (exemem)))
      ;;                                     ;; Restore state and expand result
      ;;                                     (poke irbuf oldir)
      ;;                                     (expand (normast jitptr))))))))))
      ;;                      ((symeq fname "defmacro")
      ;;                       (let ((msig (cdr (car args))))
      ;;                         (poke macenv (cons (cons (cdr (car msig)) (cons (cdr msig) (car (cdr args)))) (peek macenv))) 0))
      ;;                      ((symeq fname "let") (cons 2 (cons func (cons (cons 2 (xndletb (cdr (car args)))) (mapxnd (cdr args))))))
      ;;                      ((symeq fname "cond") (cons 2 (cons func (xndcnd args))))
      ;;                      ((symeq fname "lambda") (cons 2 (cons func (cons (car args) (mapxnd (cdr args))))))
      ;;                      ((symeq fname "quote") ast)
      ;;                      (1 (cons 2 (cons func (mapxnd args)))))))
      ;;                 (1 (cons 2 (mapxnd val))))))
      ;;            (1 ast)))))))

      ;; --- 4. IR Emitter & AST Compiler ---
      (emitir (lambda (ir)
        (cond ((eql ir 0) 0)
              (1 (poke irbuf (cons (car ir) (peek irbuf)))
                 (emitir (cdr ir))))))

      (emitelf (lambda (fd codeptr codesize)
        (let ((hdr (getheap)))
          (alloc 15) ;; Allocate 120 bytes (15 * 8)
          
          ;; --- ELF64 Header (64 bytes) ---
          (poke (add hdr 0)  11794036474041231151) ; 0x7F 'E' 'L' 'F' 02 01 01 00
          (poke (add hdr 8)  0)
          (poke (add hdr 16) 4294967302)           ; e_type=2 (EXEC), e_machine=62 (x86-64), e_version=1
          (poke (add hdr 24) 4194424)              ; e_entry = 0x400078 (Base 0x400000 + 120 byte header)
          (poke (add hdr 32) 64)                   ; e_phoff = 64
          (poke (add hdr 40) 0)                    ; e_shoff = 0
          (poke (add hdr 48) 0)                    ; e_flags = 0
          (poke (add hdr 56) 4294967360)           ; e_ehsize=64, e_phentsize=56, e_phnum=1, e_shentsize=0...
          
          ;; --- Program Header (56 bytes) ---
          (poke (add hdr 64)  7)                   ; p_type=1 (PT_LOAD), p_flags=7 (R|W|X)
          (poke (add hdr 72)  0)                   ; p_offset=0
          (poke (add hdr 80)  4194304)             ; p_vaddr=0x400000
          (poke (add hdr 88)  4194304)             ; p_paddr=0x400000
          
          ;; File Size & Memory Size (Header + Code + 8MB for BSS Heap)
          (let ((filesz (add 120 codesize))
                (memsz  (add filesz 8388608)))
            (poke (add hdr 96) filesz)             ; p_filesz
            (poke (add hdr 104) memsz))            ; p_memsz
            
          (poke (add hdr 112) 4096)                ; p_align=0x1000

          ;; 1. Write the 120-byte header
          (syscall 1 fd hdr 120)
          ;; 2. Write the executable machine code!
          (syscall 1 fd codeptr codesize))))

      (cmpnum (lambda (val) (cons (cons 'ldimm val) 0)))
      (cntlocs (lambda (env) (cond ((eql env 0) 0) (1 (let ((val (cdr (car env)))) (cond ((lt val 0) (add 1 (cntlocs (cdr env)))) (1 (cntlocs (cdr env)))))))))
      (cmpif (lambda (args compenv istail pararity) 
        (let ((cnd (car args)) (thn (car (cdr args))) (els (car (cdr (cdr args)))) (elsid (getid)) (endid (getid))) 
          (cmpexpr cnd compenv 0 pararity) 
          (emitir (cons (cons 'jz elsid) 0))
          (cmpexpr thn compenv istail pararity) 
          (emitir (cons (cons 'jmp endid) 0))
          (emitir (cons (cons 'label elsid) 0))
          (cmpexpr els compenv istail pararity) 
          (emitir (cons (cons 'label endid) 0)))))
      (cmpcndc (lambda (clauses endid compenv istail pararity)
        (cond ((eql clauses 0) (emitir (cons (cons 'ldimm 0) 0)) (emitir (cons (cons 'label endid) 0)))
              (1 (let ((clause (car clauses)) (nextid (getid)) (clauslst (cdr clause)) (cnd (car clauslst)) (body (cdr clauslst)))
                   (cmpexpr cnd compenv 0 pararity) 
                   (emitir (cons (cons 'jz nextid) 0))
                   (cmpseq body compenv istail pararity)
                   (emitir (cons (cons 'jmp endid) 0))
                   (emitir (cons (cons 'label nextid) 0))
                   (cmpcndc (cdr clauses) endid compenv istail pararity))))))
      (cmpbnd (lambda (binding offset crrenv pararity)
        (let ((bnditems (cdr binding)) (varnode (car bnditems)) (valnode (car (cdr bnditems))))
          (cmpexpr valnode crrenv 0 pararity)
          (emitir (cons (cons 'push 0) 0))
          (cons (cons (cdr varnode) offset) crrenv))))
      (cmplet (lambda (args compenv istail pararity)
        (let ((bndslst (cdr (car args))) (bodystmt (cdr args)) (numbnds (cntbnds bndslst))
              (offset (mul -8 (add 1 (cntlocs compenv))))
              (newenv (cmpbnds bndslst offset compenv pararity)))
          (cmpseq bodystmt newenv istail pararity)
          ;; Replace the two old cleanup IR instructions with this:
          (emitir (cons (cons 'addsp (mul 8 numbnds)) 0)))))
      ;; (cmplet (lambda (args compenv istail pararity)
      ;;   (let ((bndslst (cdr (car args))) (bodystmt (cdr args)) (numbnds (cntbnds bndslst))
      ;;         (offset (mul -8 (add 1 (cntlocs compenv))))
      ;;         (newenv (cmpbnds bndslst offset compenv pararity)))
      ;;     (cmpseq bodystmt newenv istail pararity)
      ;;     (emitir (cons (cons 'ldimm (mul 8 numbnds)) 0))
      ;;     (emitir (cons (cons (OP_ADDSD) 0) 0)))))
      (cmpgbnds (lambda (bindings compenv)
        (cond (bindings
               (let ((bnditems (cdr (car bindings))) (varnode (car bnditems)) (valnode (car (cdr bnditems))) (symint (cdr varnode)))
                 (let ((existing (lksymi symint (peek globtab))))
                   (cond (existing 0)
                         (1 (let ((ptr (alloc 1))) (poke globtab (cons (cons symint ptr) (peek globtab)))))))
                 (emitir (cons (cons 'ldimm (lksymi symint (peek globtab))) 0))
                 (emitir (cons (cons 'push 0) 0))
                 (cmpexpr valnode compenv 0 0)
                 (emitir (cons (cons 'set2 0) 0))
                 (emitir (cons (cons 'poke 0) 0))
                 (cmpgbnds (cdr bindings) compenv)))
              (1 0))))
      ;; (cmpgbnds (lambda (bindings compenv)
      ;;   (cond (bindings
      ;;          (let ((bnditems (cdr (car bindings))) (varnode (car bnditems)) (valnode (car (cdr bnditems))) (symint (cdr varnode)))
      ;;            (let ((existing (lksymi symint (peek globtab))))
      ;;              (cond (existing 0)
      ;;                    (1 (let ((ptr (alloc 1))) (poke globtab (cons (cons symint ptr) (peek globtab)))))))
      ;;            (cmpexpr valnode compenv 0 0)
      ;;            (emitir (cons (cons 'push 0) 0))
      ;;            (emitir (cons (cons 'ldimm (lksymi symint (peek globtab))) 0))
      ;;            (emitir (cons (cons 'set2 0) 0))
      ;;            (emitir (cons (cons 'poke 0) 0))
      ;;            (cmpgbnds (cdr bindings) compenv)))
      ;;         (1 0))))
      (lkop (lambda (dict opsym)
        (let ((sym (peek dict)))
          (cond
            ((eql sym 0) 0) ;; End of dictionary marker
            ((eql sym opsym) dict)
            (1 (let ((size (peek (add dict 8))))
                 (let ((algnd (mul (div (add size 7) 8) 8)))
                   (lkop (add dict (add 16 algnd)) opsym))))))))
      (cmplam (lambda (args compenv) 
        (let ((id (getid))) 
          (poke lams (cons (cons id args) (peek lams)))
          (emitir (cons (cons 'ldfnc id) 0)))))
      (alllams (lambda ()
        (let ((lambdas (peek lams)))
          (cond (lambdas
                 (poke lams (cdr lambdas))
                 (let ((entry (car lambdas)) (id (car entry)) (args (cdr entry))
                       (params (cdr (car args))) (bodystmt (cdr args)) (numparam (cntbnds params)) (lamenv (bldpenv params -8 0)))
                   (emitir (cons (cons 'label id) 0))
                   (emitir (cons (cons 'enter 0) 0))
                   (pushargs numparam 0)
                   (cmpseq bodystmt lamenv 1 numparam) 
                   (emitir (cons (cons 'ret 0) 0))
                   (alllams)))
                (1 0)))))
      (cmpapp (lambda (funcnode args compenv istail pararity) 
        (let ((numargs (cntbnds args)))
          (cmpargs args compenv pararity) 
          (cmpexpr funcnode compenv 0 pararity)
          (popargs numargs 0) 
          (cond (istail (emitir (cons (cons 'tcall 0) 0))) (1 (emitir (cons (cons 'call 0) 0)))))))
      (cmpargs (lambda (args compenv pararity) 
        (cond (args (cmpargs (cdr args) compenv pararity) (cmpexpr (car args) compenv 0 pararity) (emitir (cons (cons 'push 0) 0))) (1 0))))
      (popargs (lambda (n i)
        (cond ((eql i n) 0) (1 (emitir (cons (cons 'popar i) 0)) (popargs n (add i 1))))))
      (pushargs (lambda (n i)
        (cond ((eql i n) 0) (1 (emitir (cons (cons 'pshar i) 0)) (pushargs n (add i 1))))))
      (cmpsys (lambda (args compenv pararity)
        (let ((numargs (cntbnds args)))
          (cmpargs args compenv pararity)
          (emitir (cons (cons 'popra 0) 0))
          (popargs (sub numargs 1) 0)
          (emitir (cons (cons 'sys numargs) 0)))))
      (cmpbnds (lambda (bindings offset crrenv pararity)
        (cond (bindings (let ((newenv (cmpbnd (car bindings) offset crrenv pararity))) (cmpbnds (cdr bindings) (sub offset 8) newenv pararity))) (1 crrenv))))
      (cmpglet (lambda (args compenv) (cmpgbnds (cdr (car args)) compenv) (cmpseq (cdr args) compenv 0 0)))
      (cmpcond (lambda (args compenv istail pararity) (let ((endid (getid))) (cmpcndc args endid compenv istail pararity))))
      (cmpseq (lambda (stmts compenv istail pararity)
        (cond ((eql stmts 0) 0)
              (1 (let ((islast (eql (cdr stmts) 0)) (stmttail (cond (istail islast) (1 0))))
                   (cmpexpr (car stmts) compenv stmttail pararity)
                   (cmpseq (cdr stmts) compenv istail pararity))))))
      (cntbnds (lambda (bindings) (cond (bindings (add 1 (cntbnds (cdr bindings)))) (1 0))))
      (bldpenv (lambda (params offset baseenv)
        (cond (params (cons (cons (cdr (car params)) offset) (bldpenv (cdr params) (sub offset 8) baseenv))) (1 baseenv))))

      (isnul (lambda (s) (cond ((symeq s "getchar") 1) ((symeq s "getheap") 1) ((symeq s "dict") 1) (1 0))))
      (isun (lambda (s)
        (cond ((symeq s "putint") 1) ((symeq s "puthex") 1) ((symeq s "putchar") 1) ((symeq s "putchunk") 1)
              ((symeq s "peek") 1) ((symeq s "alloc") 1) ((symeq s "setheap") 1) ((symeq s "car") 1) ((symeq s "cdr") 1) ((symeq s "not") 1) (1 0))))
      (isvarm (lambda (s)
        (cond ((symeq s "add") 1) ((symeq s "sub") 1) ((symeq s "mul") 1) ((symeq s "div") 1) ((symeq s "logior") 1) ((symeq s "or") 1) ((symeq s "logand") 1) ((symeq s "and") 1) (1 0))))
      (isbin (lambda (s)
        (cond ((symeq s "eql") 1) ((symeq s "lt") 1) ((symeq s "gt") 1) ((symeq s "le") 1) ((symeq s "ge") 1) ((symeq s "poke") 1) ((symeq s "pokebyte") 1) ((symeq s "peekidx") 1) ((symeq s "cons") 1) ((symeq s "ash") 1) (1 0))))
      (cmpvar (lambda (symint args compenv pararity)
        (cond (args (cmpexpr (car args) compenv 0 pararity) (cmpvarl symint (cdr args) compenv pararity))
              (1 (cond ((symeq symint "mul") (emitir (cons (cons 'ldimm 1) 0))) (1 (emitir (cons (cons 'ldimm 0) 0))))))))
      
      (cmpnul (lambda (symint)
        (cond ((symeq symint "dict") (emitir (cons (cons 'ldimm (dict)) 0)))
              (1 (emitir (cons (cons symint 0) 0))))))

      (cmpun (lambda (symint arg compenv pararity)
        (cmpexpr arg compenv 0 pararity)
        (emitir (cons (cons symint 0) 0))))

      (cmpbin (lambda (symint arg2 compenv pararity)
        (emitir (cons (cons 'push 0) 0))
        (cmpexpr arg2 compenv 0 pararity)
        (emitir (cons (cons 'set2 0) (cons (cons symint 0) 0)))))

      ;; (cmpnul (lambda (symint)
      ;;   (cond ((symeq symint "dict") (emitir (cons (cons 'ldimm (dict)) 0)))
      ;;         (1 (let ((opir (cond ((symeq symint "getchar") (OP_GTCHR)) ((symeq symint "getheap") (OP_GETHP)) (1 0))))
      ;;              (emitir (cons (cons opir 0) 0)))))))
      ;; (cmpun (lambda (symint arg compenv pararity)
      ;;   (cmpexpr arg compenv 0 pararity)
      ;;   (let ((opir (cond ((symeq symint "putint") (OP_PTINT)) ((symeq symint "puthex") (OP_PTHEX)) ((symeq symint "putchar") (OP_PTCHR)) ((symeq symint "putchunk") (OP_PTCHK)) ((symeq symint "peek") 'peek) ((symeq symint "alloc") (OP_ALLOC)) ((symeq symint "setheap") (OP_STHEP)) ((symeq symint "car") 'car) ((symeq symint "cdr") 'cdr) ((symeq symint "not") (OP_NOT)) (1 0))))
      ;;     (emitir (cons (cons opir 0) 0)))))
      ;; (cmpbin (lambda (symint arg2 compenv pararity)
      ;;   (emitir (cons (cons 'push 0) 0))
      ;;   (cmpexpr arg2 compenv 0 pararity)
      ;;   (let ((opir (cond ((symeq symint "add") (OP_ADD)) ((symeq symint "sub") (OP_SUB)) ((symeq symint "mul") (OP_MUL)) ((symeq symint "div") (OP_DIV)) ((symeq symint "eql") (OP_EQL)) ((symeq symint "lt")  (OP_LT)) ((symeq symint "gt")  (OP_GT)) ((symeq symint "le")  (OP_LE)) ((symeq symint "ge")  (OP_GE)) ((symeq symint "and") (OP_AND)) ((symeq symint "logand") (OP_AND)) ((symeq symint "or")  (OP_OR)) ((symeq symint "logior") (OP_OR)) ((symeq symint "ash") (OP_ASH)) ((symeq symint "poke") 'poke) ((symeq symint "pokebyte") (OP_POKEB)) ((symeq symint "peekidx") (OP_PEEKI)) ((symeq symint "cons") (OP_CONS)) (1 0))))
      ;;     (emitir (cons (cons 'set2 0) (cons (cons opir 0) 0))))))
      (cmptern (lambda (symint arg1 arg2 arg3 compenv pararity)
        (cmpexpr arg1 compenv 0 pararity)
        (emitir (cons (cons 'push 0) 0))
        (cmpexpr arg2 compenv 0 pararity)
        (emitir (cons (cons 'push 0) 0))
        (cmpexpr arg3 compenv 0 pararity)
        (emitir (cons (cons 'set2 0) 0))
        (emitir (cons (cons 'popr8 0) 0))
        (let ((opir (cond ((symeq symint "pokeidx") 'pokeidx) (1 0))))
          (emitir (cons (cons opir 0) 0)))))
      (emitcadr (lambda (symint)
        (let ((c (logand symint 255)))
          (cond ((eql c 114) 1)
                (1 (emitcadr (ash symint -8))
                   (cond ((eql c 97)  (emitir (cons (cons 'car 0) 0)))
                         ((eql c 100) (emitir (cons (cons 'cdr 0) 0)))
                         (1 0)))))))
      (cmpvarl (lambda (symint args compenv pararity)
        (cond (args (cmpbin symint (car args) compenv pararity) (cmpvarl symint (cdr args) compenv pararity)) (1 0))))
      (iscadrc (lambda (symint)
        (let ((c (logand symint 255)))
          (cond ((eql c 0) 0) ((eql c 114) (eql (ash symint -8) 0)) ((or (eql c 97) (eql c 100)) (iscadrc (ash symint -8))) (1 0)))))
      (iscadr (lambda (symint)
        (let ((c (logand symint 255)))
          (cond ((eql c 99) (let ((c1 (logand (ash symint -8) 255))) (cond ((or (eql c1 97) (eql c1 100)) (iscadrc (ash symint -8))) (1 0)))) (1 0)))))

      (cmplist (lambda (astlist compenv istail pararity)
        (let ((funcnode (car astlist)) (args (cdr astlist)))
          (cond
            ((eql (car funcnode) 1)
             (let ((funcname (cdr funcnode)))
               (cond
                 ((symeq funcname "let") (cmplet args compenv istail pararity))
                 ((symeq funcname "lambda") (cmplam args compenv))
                 ((symeq funcname "cond") (cmpcond args compenv istail pararity))
                 ((symeq funcname "if") (cmpif args compenv istail pararity))
                 ((symeq funcname "quote")
                  (let ((qarg (car args)))
                    (cond ((eql (car qarg) 1) (emitir (cons (cons 'ldimm (cdr qarg)) 0)))
                          (1 (cmpexpr qarg compenv 0 pararity)))))
                 ((eql (iscadr funcname) 1) (cmpexpr (car args) compenv 0 pararity) (emitcadr (ash funcname -8)))
                 ((isnul funcname) (cmpnul funcname))
                 ((isun funcname) (cmpun funcname (car args) compenv pararity))
                 ((isvarm funcname) (cmpvar funcname args compenv pararity))
                 ((isbin funcname) (cmpvar funcname (cons (car args) (cons (car (cdr args)) 0)) compenv pararity))
                 ((symeq funcname "pokeidx") (cmptern funcname (car args) (car (cdr args)) (car (cdr (cdr args))) compenv pararity))
                 ((symeq funcname "syscall") (cmpsys args compenv pararity))
                 (1 (cmpapp funcnode args compenv istail pararity)))))
            (1 (cmpapp funcnode args compenv istail pararity))))))

      (cmpexpr (lambda (ast compenv istail pararity)
        (cond
          ((eql ast 0) 0)
          (1 (let ((tag (car ast)) (val (cdr ast)))
               (cond
                 ((eql tag 0) (emitir (cmpnum val)))
                 ((eql tag 1)
                  (let ((offset (lkenv val compenv)))
                    (cond
                      ((eql offset 0)
                       (cond ((lksymi val (peek globtab))
                              (emitir (cons (cons 'ldimm (lksymi val (peek globtab))) 0))
                              (emitir (cons (cons 'peek 0) 0)))
                             (1 (eputstr "ERR: UNBOUND VARIABLE ") (eputsym val) (eputstr "\n") (exit 1))))
                      (1 (emitir (cons (cons 'ldimm offset) 0))
                         (emitir (cons (cons 'ldmd 0) 0))))))
                 ((eql tag 2) (cmplist val compenv istail pararity))
                 ((eql tag 3) (emitir (cons (cons 'ldimm (car val)) 0))) ;; Load absolute string memory pointer!
                 (1 0)))))))

      (cmpprog (lambda (ast)
        (cond ((eql (car ast) 2)
               (let ((val (cdr ast)) (funcnode (car val)))
                 (cond ((eql (car funcnode) 1) (cond ((symeq (cdr funcnode) "let") (cmpglet (cdr val) 0)) (1 (cmpexpr ast 0 0 0))))
                       (1 (cmpexpr ast 0 0 0)))))
              (1 (cmpexpr ast 0 0 0)))))

      ;; --- 5. JIT Engine ---
      (getlbl (lambda (id env)
        (cond (env (cond ((eql id (car (car env))) (cdr (car env))) (1 (getlbl id (cdr env))))) (1 -1))))

      (jitsizes (lambda (ir dict offset labels)
        (cond
          (ir
           (let ((node (car ir)) (op (car node)) (arg (cdr node)))
             (cond
               ((eql op 'label) (jitsizes (cdr ir) dict offset (cons (cons arg offset) labels)))
               ((or (eql op 'ldimm) (eql op 'ldfnc)) (jitsizes (cdr ir) dict (add offset 10) labels))
               ((eql op 'jmp)   (jitsizes (cdr ir) dict (add offset 13) labels))
               ((eql op 'jz)    (jitsizes (cdr ir) dict (add offset 18) labels))
               ((eql op 'popar) (cond ((ge arg 4) (jitsizes (cdr ir) dict (add offset 2) labels)) (1 (jitsizes (cdr ir) dict (add offset 1) labels))))
               ((eql op 'pshar) (cond ((ge arg 4) (jitsizes (cdr ir) dict (add offset 2) labels)) (1 (jitsizes (cdr ir) dict (add offset 1) labels))))
               ((eql op 'addsp) (jitsizes (cdr ir) dict (add offset 7) labels))
               ((eql op 'sys)   (jitsizes (cdr ir) dict (add offset 5) labels))
               (1
                (let ((entry (lkop dict op)))
                  (cond
                    ((eql entry 0) (eputstr "JIT ERR: ") (eputsym op) (eputstr "\n") (exit 1))
                    (1 (let ((size (peek (add entry 8))))
                         (jitsizes (cdr ir) dict (add offset size) labels)))))))))
          (1 labels))))
      
      ;; (jitsizes (lambda (ir dict offset labels)
      ;;   (cond
      ;;     (ir
      ;;      (let ((node (car ir)) (op (car node)) (arg (cdr node)))
      ;;        (cond
      ;;          ((eql op 'label) (jitsizes (cdr ir) dict offset (cons (cons arg offset) labels)))
      ;;          ((or (eql op 'ldimm) (eql op 'ldfnc)) (jitsizes (cdr ir) dict (add offset 10) labels))
      ;;          ((eql op 'jmp)   (jitsizes (cdr ir) dict (add offset 13) labels))
      ;;          ((eql op 'jz)    (jitsizes (cdr ir) dict (add offset 18) labels))
      ;;          ((eql op 'popar) (cond ((ge arg 4) (jitsizes (cdr ir) dict (add offset 2) labels)) (1 (jitsizes (cdr ir) dict (add offset 1) labels))))
      ;;          ((eql op 'sys)   (jitsizes (cdr ir) dict (add offset 5) labels))
      ;;          (1
      ;;           (let ((size (peekidx dict (add (mul op 2) 1))))
      ;;             (jitsizes (cdr ir) dict (add offset size) labels))))))
      ;;     (1 labels))))

      (emitjit (lambda (ir dict labels ptr offset)
        (cond
          (ir
           (let ((node (car ir)) (op (car node)) (arg (cdr node)))
             (cond
               ((eql op 'label) (emitjit (cdr ir) dict labels ptr offset))

               ((or (eql op 'ldimm) (eql op 'ldfnc))
                (let ((val (cond ((eql op 'ldfnc) (add ptr (getlbl arg labels))) (1 arg))))
                  (pokebyte (add ptr offset) 72)
                  (pokebyte (add ptr (add offset 1)) 184)
                  (poke (add ptr (add offset 2)) val)
                  (emitjit (cdr ir) dict labels ptr (add offset 10))))

               ((eql op 'jmp)
                (let ((target (add ptr (getlbl arg labels))))
                  (pokebyte (add ptr offset) 73)
                  (pokebyte (add ptr (add offset 1)) 187)
                  (poke (add ptr (add offset 2)) target)
                  (pokebyte (add ptr (add offset 10)) 65) (pokebyte (add ptr (add offset 11)) 255) (pokebyte (add ptr (add offset 12)) 227)
                  (emitjit (cdr ir) dict labels ptr (add offset 13))))

               ((eql op 'jz)
                (let ((target (add ptr (getlbl arg labels))))
                  (pokebyte (add ptr offset) 72) (pokebyte (add ptr (add offset 1)) 133) (pokebyte (add ptr (add offset 2)) 192)
                  (pokebyte (add ptr (add offset 3)) 117) (pokebyte (add ptr (add offset 4)) 13) (pokebyte (add ptr (add offset 5)) 73)
                  (pokebyte (add ptr (add offset 6)) 187) (poke (add ptr (add offset 7)) target) (pokebyte (add ptr (add offset 15)) 65)
                  (pokebyte (add ptr (add offset 16)) 255) (pokebyte (add ptr (add offset 17)) 227)
                  (emitjit (cdr ir) dict labels ptr (add offset 18))))

               ((eql op 'popar)
                (cond
                  ((eql arg 0) (pokebyte (add ptr offset) 95) (emitjit (cdr ir) dict labels ptr (add offset 1))) 
                  ((eql arg 1) (pokebyte (add ptr offset) 94) (emitjit (cdr ir) dict labels ptr (add offset 1))) 
                  ((eql arg 2) (pokebyte (add ptr offset) 90) (emitjit (cdr ir) dict labels ptr (add offset 1))) 
                  ((eql arg 3) (pokebyte (add ptr offset) 89) (emitjit (cdr ir) dict labels ptr (add offset 1))) 
                  ((eql arg 4) (pokebyte (add ptr offset) 65) (pokebyte (add ptr (add offset 1)) 88) (emitjit (cdr ir) dict labels ptr (add offset 2))) 
                  ((eql arg 5) (pokebyte (add ptr offset) 65) (pokebyte (add ptr (add offset 1)) 89) (emitjit (cdr ir) dict labels ptr (add offset 2)))))

               ((eql op 'pshar)
                (cond
                  ((eql arg 0) (pokebyte (add ptr offset) 87) (emitjit (cdr ir) dict labels ptr (add offset 1))) ; push rdi
                  ((eql arg 1) (pokebyte (add ptr offset) 86) (emitjit (cdr ir) dict labels ptr (add offset 1))) ; push rsi
                  ((eql arg 2) (pokebyte (add ptr offset) 82) (emitjit (cdr ir) dict labels ptr (add offset 1))) ; push rdx
                  ((eql arg 3) (pokebyte (add ptr offset) 81) (emitjit (cdr ir) dict labels ptr (add offset 1))) ; push rcx
                  ((eql arg 4) (pokebyte (add ptr offset) 65) (pokebyte (add ptr (add offset 1)) 80) (emitjit (cdr ir) dict labels ptr (add offset 2))) ; push r8
                  ((eql arg 5) (pokebyte (add ptr offset) 65) (pokebyte (add ptr (add offset 1)) 81) (emitjit (cdr ir) dict labels ptr (add offset 2))))) ; push r9

               ((eql op 'addsp)
                (pokebyte (add ptr offset) 72)
                (pokebyte (add ptr (add offset 1)) 129) ;; 0x81 instead of 0x83
                (pokebyte (add ptr (add offset 2)) 196)
                ;; Write the 32-bit integer (little endian)
                (pokebyte (add ptr (add offset 3)) (logand arg 255))
                (pokebyte (add ptr (add offset 4)) (logand (ash arg -8) 255))
                (pokebyte (add ptr (add offset 5)) (logand (ash arg -16) 255))
                (pokebyte (add ptr (add offset 6)) (logand (ash arg -24) 255))
                (emitjit (cdr ir) dict labels ptr (add offset 7)))

               ;; ((eql op 'addsp)
               ;;  (pokebyte (add ptr offset) 72)
               ;;  (pokebyte (add ptr (add offset 1)) 131)
               ;;  (pokebyte (add ptr (add offset 2)) 196)
               ;;  (pokebyte (add ptr (add offset 3)) arg) ; Note: Assumes arg < 128
               ;;  (emitjit (cdr ir) dict labels ptr (add offset 4)))

               ((eql op 'sys)
                (pokebyte (add ptr offset) 73) (pokebyte (add ptr (add offset 1)) 137) (pokebyte (add ptr (add offset 2)) 202) 
                (pokebyte (add ptr (add offset 3)) 15) (pokebyte (add ptr (add offset 4)) 5)   
                (emitjit (cdr ir) dict labels ptr (add offset 5)))

               (1
                (let ((entry (lkop dict op)))
                  (cond
                    ((eql entry 0) 
                     (eputstr "JIT ERR: UNKNOWN OPCODE ") (eputsym op) (eputstr "\n") (exit 1))
                    (1 
                     (let ((size (peek (add entry 8))))
                       (cpybytes (add entry 16) (add ptr offset) size)
                       (emitjit (cdr ir) dict labels ptr (add offset size))))))))))
          (1 offset))))
          ;;      (1
          ;;       (let ((entry (lkop dict op)))
          ;;         (let ((size (peek (add entry 8))))
          ;;           ;; entry + 0 = Symbol, entry + 8 = Size, entry + 16 = Machine Code
          ;;           (cpybytes (add entry 16) (add ptr offset) size)
          ;;           (emitjit (cdr ir) dict labels ptr (add offset size))))))))
          ;; (1 offset))))

      (runjit (lambda (ir dict)
        (let ((exemem (syscall 9 0 2097152 7 34 -1 0))) ;; 2MB R|W|X
          (let ((labels (jitsizes ir dict 0 0)))
            (emitjit ir dict labels exemem 0)
            exemem))))

      ;; --- 6. The JIT REPL ---
      (cmploop (lambda (dictptr)
        (cond ((eql (peek ismode) 0) (putstr "bml> ")) (1 0))
        (let ((rawast (prsexpr)))
          (cond
            ((and (eql (car rawast) 1) (eql (cdr rawast) 0)) ;; EOF
             (eputline "Reached EOF! Finalizing ELF...") 
             (cond ((peek ismode)
                    ;; 1. Emit exit(0)
                    (emitir (cons (cons 'ldimm 0) 0))
                    (emitir (cons (cons 'push 0) 0))
                    (emitir (cons (cons 'popar 0) 0))  ;; Pop 0 to RDI (Exit status code)
                    (emitir (cons (cons 'ldimm 60) 0)) ;; Load 60 into RAX (sys_exit syscall number)
                    (emitir (cons (cons 'sys 1) 0))    ;; Exit syscall
                    
                    ;; 2. Append lambdas
                    (alllams)
                    
                    ;; 3. Generate Machine Code to buffer
                    (let ((finalir (revlist (peek irbuf) 0)))
                      (let ((exemem (syscall 9 0 2097152 7 34 -1 0)))
                        (let ((labels (jitsizes finalir dictptr 0 0)))
                          (let ((codesize (emitjit finalir dictptr labels exemem 0)))
                            ;; 4. Write to "a.out" (O_CREAT|O_WRONLY|O_TRUNC)
                            (let ((fn (getheap)))
                              (pokebyte fn 97) (pokebyte (add fn 1) 46) (pokebyte (add fn 2) 111)
                              (pokebyte (add fn 3) 117) (pokebyte (add fn 4) 116) (pokebyte (add fn 5) 0) 
                              (alloc 1)
                              (let ((fd (syscall 2 fn 577 511)))
                                (emitelf fd exemem codesize)
                                (syscall 3 fd))))))))
                   (1 0)))
            (1
             (let ((expast (expand rawast)))
               (cond ((eql expast 0) 0)
                     (1
                      (cond ((peek ismode)
                             (eputstr "AOT Compiling Node Tag: ")
                             (let ((ptr (getheap))) 
                               (pokebyte ptr (add (car expast) 48)) 
                               (syscall 1 2 ptr 1))
                             (eputstr "\n")
                             (cmpprog expast)) ;; AOT: Accumulate IR
                            (1
                             ;; JIT: Reset, Compile, Run
                             (poke irbuf 0)
                             (emitir (cons (cons 'enter 0) 0))
                             (cmpprog expast)
                             (emitir (cons (cons 'ret 0) 0))
                             (alllams)
                             (let ((finalir (revlist (peek irbuf) 0)))
                               (let ((exemem (syscall 9 0 2097152 7 34 -1 0)))
                                 (let ((labels (jitsizes finalir dictptr 0 0)))
                                   (emitjit finalir dictptr labels exemem 0)
                                   (let ((func_ptr (exemem)))
                                     (putstr "=> ") (putint func_ptr) (putchar 10))))))))))
             (cmploop dictptr)))))))
      ;; (cmploop (lambda (dictptr)
      ;;   (putstr "bml> ")
      ;;   (let ((rawast (prsexpr)))
      ;;     (cond
      ;;       ((and (eql (car rawast) 1) (eql (cdr rawast) 0)) 0) ;; EOF
      ;;       (1
      ;;        (let ((expast (expand rawast)))
      ;;          (cond ((eql expast 0) 0)
      ;;                (1
      ;;                 (poke irbuf 0) ;; Reset Buffer
      ;;                 
      ;;                 ;; Compile expression and setup function stack frame!
      ;;                 (emitir (cons (cons 'enter 0) 0))
      ;;                 (cmpprog expast)
      ;;                 (emitir (cons (cons 'ret 0) 0))
      ;;                 (alllams) ;; Lambdas are safely appended AFTER the return!
      ;; 
      ;;                 (let ((finalir (revlist (peek irbuf) 0)))
      ;;                   (let ((funcptr (runjit finalir dictptr)))
      ;;                     (let ((result (funcptr)))
      ;;                       (putstr "=> ")
      ;;                       (putint result)
      ;;                       (putchar 10)))))))
      ;;          (cmploop dictptr)))))))

  ;; Start the REPL Native Environment!
  ;; (putline "===============================================")
  ;; (putline " BML JIT Compiler - Natively Executing in RAM ")
  ;; (putline "===============================================")
  ;; (cmploop (dict)))
        
  ;; Check if stdin is piped (AOT Mode)
  (poke ismode (lt (syscall 16 0 21505 (getheap)) 0))
  
  (cond
    ((peek ismode)
     ;; AOT BOOT STUB: Inject 'enter' and mmap 8MB heap to r15 natively
     (emitir (cons (cons 'enter 0) 0))
     (cmpprog (expand (cons 2 (cons (cons 1 'setheap)
                        (cons (cons 2 (cons (cons 1 'syscall)
                                      (cons (cons 0 9)
                                      (cons (cons 0 0)
                                      (cons (cons 0 8388608)
                                      (cons (cons 0 7)
                                      (cons (cons 0 34)
                                      (cons (cons 0 -1)
                                      (cons (cons 0 0) 0)))))))))
                              0))))))
    (1
     (putline "===============================================")
     (putline " BML Native Compiler - Direct to ELF (JIT/AOT)")
     (putline "===============================================")))
  
  (cmploop (dict)))
