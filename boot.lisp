(let ((lookahd    (cons 0 0))
      (lblcnt     (cons 0 0))
      (macenv     (cons 0 0))
      (cmpenv     (cons 0 0))
      (symtab     (cons 0 0))
      (globtab    (cons 0 0))
      (strtab     (cons 0 0))
      (lams       (cons 0 0))

      ;; --- 1. I/O & Compiler Loop ---
      (nxtchar (lambda ()
        (let ((c (peek lookahd)))
          (cond (c (poke lookahd 0) c) (1 (getchar))))))
      (ungtchar (lambda (c) (poke lookahd c)))

      (skpcom (lambda ()
        (let ((c (nxtchar)))
          (cond ((or (eql c 10) (eql c 0)) (skpws)) (1 (skpcom))))))
      (skpws  (lambda ()
        (let ((c (nxtchar)))
          (cond ((isspace c) (skpws)) ((eql c 59)  (skpcom)) (1 (ungtchar c))))))

      (getid (lambda () (let ((id (peek lblcnt))) (poke lblcnt (add id 1)) id)))

      ;; --- 2. Lexer/Parser ---
      (prsint (lambda (acc)
        (let ((c (nxtchar)))
          (cond ((isdigit c) (prsint (add (mul acc 10) (sub c 48))))
                (1 (ungtchar c) acc)))))

      (prsnumc (lambda (acc shift)
        (let ((c (nxtchar)))
          (cond
            ((isdelim c) (ungtchar c) acc)
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
               (cond
                 ((eql esc 110) (pokebyte (add ptr count) 10))
                 ((eql esc 116) (pokebyte (add ptr count) 9))
                 ((eql esc 114) (pokebyte (add ptr count) 13))
                 ((eql esc 92)  (pokebyte (add ptr count) 92))
                 ((eql esc 34)  (pokebyte (add ptr count) 34))
                 (1 (pokebyte (add ptr count) esc)))
               (prsstrl ptr (add count 1))))
            (1
             (pokebyte (add ptr count) c)
             (prsstrl ptr (add count 1)))))))

      (prsstr (lambda () (prsstrl (getheap) 0)))

      (prslist (lambda () (skpws)
        (let ((c (nxtchar)))
          (cond ((or (eql c 41) (eql c 0)) 0)
                (1 (ungtchar c) (cons (prsexpr) (prslist)))))))

      (prsexpr (lambda () (skpws)
        (let ((c (nxtchar)))
          (cond ((eql c 40)  (cons 2 (prslist)))
                ((eql c 34)  (cons 3 (prsstr)))
                ;; --- NEW: Syntactic Sugar for 'expr ---
                ((eql c 39)  (cons 2 (cons (cons 1 435745158513) (cons (prsexpr) 0))))
                ;; --------------------------------------
                ((isdigit c) (ungtchar c) (cons 0 (prsint 0)))
                ((eql c 45)  (cons 0 (sub 0 (prsint 0))))
                (1 (ungtchar c) (cons 1 (prssym)))))))

      ;; --- Dynamic Primitive Loading ---
      (readfile (lambda (filename)
        (let ((ptr (nulterm filename)))
          (let ((fd (syscall 2 ptr 0 0)))
            (cond
              ((lt fd 0) 0)
              (1
               (let ((buf (getheap)))
                 (let ((byteread (syscall 0 fd buf 65536)))
                   (syscall 3 fd)
                   (pokebyte (add buf byteread) 10)
                   (let ((safelen (add byteread 1)))
                     (alloc (add (div safelen 8) 1))
                     (cons buf safelen))))))))))

      (findchar (lambda (ptr end c)
        (cond ((ge ptr end) 0)
              ((eql (logand (peek ptr) 255) c) ptr)
              (1 (findchar (add ptr 1) end c)))))

      ;; --- 2.5 Cmp-Time Environment ---
      (lksym (lambda (str table)
        (cond ((eql table 0) 0)
              (1 (let ((entry (sfcar table)))
                   (cond ((streq str (car entry)) (cdr entry))
                         (1 (lksym str (cdr table)))))))))

      (lksymi (lambda (symint table)
        (cond ((eql table 0) 0)
              (1 (let ((entry (sfcar table)))
                   (cond ((eql symint (car entry)) (cdr entry))
                         (1 (lksymi symint (cdr table)))))))))

      (lkmac (lambda (symint env)
        (cond ((eql env 0) 0)
              (1 (let ((binding (sfcar env)))
                   (cond ((eql symint (car binding)) (cdr binding))
                         (1 (lkmac symint (cdr env)))))))))

      (bndmac (lambda (params args env)
        (cond (params (cons (cons (cdr (car params)) (car args))
                            (bndmac (cdr params) (cdr args) env)))
              (1 env))))

      (lkenv (lambda (symint env)
        (cond
          ((eql env 0) 0)
          (1 (let ((binding (sfcar env)))
               (cond
                 ((eql symint (car binding)) (cdr binding))
                 (1 (lkenv symint (cdr env)))))))))

      (evlet (lambda (bnds env)
        (cond ((eql bnds 0) env)
              (1 (let ((bndlst (cdr (car bnds))))
                   (evlet (cdr bnds)
                          (cons (cons (cdr (car bndlst))
                                      (evalast (car (cdr bndlst)) env))
                                env)))))))

      (evalast (lambda (ast env)
        (cond
          ((eql ast 0) 0)
          (1 (let ((tag (car ast)) (val (cdr ast)))
               (cond
                 ((eql tag 0) val)
                 ((eql tag 3) val)
                 ((eql tag 1) (lkmac val env))
                 ((eql tag 2)
                  (let ((funcname (cdr (car val))) (args (cdr val)))
                    (cond
                      ((symeq funcname "quote") (car args))
                      ((symeq funcname "car")   (car (evalast (car args) env)))
                      ((symeq funcname "cdr")   (cdr (evalast (car args) env)))
                      ((symeq funcname "cons")  (cons (evalast (car args) env) (evalast (car (cdr args)) env)))
                      ((symeq funcname "eql")   (cond ((eql (evalast (car args) env) (evalast (car (cdr args)) env)) 1) (1 0)))
                      ((symeq funcname "if")    (cond ((evalast (car args) env) (evalast (car (cdr args)) env)) (1 (evalast (car (cdr (cdr args))) env))))
                      ;; --- Math & Let Bindings for Macros ---
                      ((symeq funcname "add") (add (evalast (car args) env) (evalast (car (cdr args)) env)))
                      ((symeq funcname "sub") (sub (evalast (car args) env) (evalast (car (cdr args)) env)))
                      ((symeq funcname "mul") (mul (evalast (car args) env) (evalast (car (cdr args)) env)))
                      ((symeq funcname "div") (div (evalast (car args) env) (evalast (car (cdr args)) env)))
                      ((symeq funcname "ash") (ash (evalast (car args) env) (evalast (car (cdr args)) env)))
                      ((symeq funcname "let") (evalast (car (cdr args)) (evlet (cdr (car args)) env)))
                      ;; -------------------------------------------
                      (1 0))))
                 (1 0)))))))

      (append (lambda (l1 l2)
                (cond ((eql l1 0) l2)
                      (1 (cons (car l1) (append (cdr l1) l2))))))

      (emitir (lambda (ir)
        (cond
          ((eql ir 0) 0)
          (1 (let ((node (car ir)) (op (car node)) (arg (cdr node)))
               (cond
                 ((eql op (OP_LDIMM)) (putstr "  ir_load_imm ") (putint arg) (putchar 10))
                 ((eql op (OP_LDMEM)) (putstr "  ir_load_mem ") (putint arg) (putchar 10))
                 ((eql op (OP_LDGLB)) (putstr "  ir_load_glob global_") (putsym arg) (putchar 10))

                 ;; Stack & Math Opcodes
                 ((eql op (OP_PUSH))  (putline "  ir_push"))
                 ((eql op (OP_SET2))  (putline "  ir_set_arg2"))
                 ((eql op (OP_POPR8)) (putline "  ir_pop_r8"))
                 ((eql op (OP_POPRA)) (putline "  ir_pop_rax"))
                 ((eql op (OP_ADD))   (putline "  ir_add"))
                 ((eql op (OP_SUB))   (putline "  ir_sub"))
                 ((eql op (OP_MUL))   (putline "  ir_mul"))
                 ((eql op (OP_DIV))   (putline "  ir_div"))
                 ((eql op (OP_EQL))   (putline "  ir_eql"))
                 ((eql op (OP_LT))    (putline "  ir_lt"))
                 ((eql op (OP_GT))    (putline "  ir_gt"))
                 ((eql op (OP_LE))    (putline "  ir_le"))
                 ((eql op (OP_GE))    (putline "  ir_ge"))
                 ((eql op (OP_AND))   (putline "  ir_and"))
                 ((eql op (OP_OR))    (putline "  ir_or"))
                 ((eql op (OP_ASH))   (putline "  ir_ash"))
                 ((eql op (OP_CAR))   (putline "  ir_car"))
                 ((eql op (OP_CDR))   (putline "  ir_cdr"))
                 ((eql op (OP_CONS))  (putline "  ir_cons"))
                 ((eql op (OP_PEEK))  (putline "  ir_peek"))
                 ((eql op (OP_POKE))  (putline "  ir_poke"))
                 ((eql op (OP_POKEB)) (putline "  ir_pokebyte"))
                 ((eql op (OP_PEEKI)) (putline "  ir_peekidx"))
                 ((eql op (OP_POKEI)) (putline "  ir_pokeidx"))
                 ((eql op (OP_NOT))   (putline "  ir_not"))
                 ((eql op (OP_DICT))  (putline "  ir_dict"))
                 ((eql op (OP_GETHP)) (putline "  ir_getheap"))
                 ((eql op (OP_STHEP)) (putline "  ir_setheap"))
                 ((eql op (OP_ALLOC)) (putline "  ir_alloc"))
                 ((eql op (OP_GTCHR)) (putline "  ir_getchar"))
                 ((eql op (OP_PTCHR)) (putline "  ir_putchar"))
                 ((eql op (OP_PTINT)) (putline "  ir_putint"))
                 ((eql op (OP_PTHEX)) (putline "  ir_puthex"))
                 ((eql op (OP_PTCHK)) (putline "  ir_putchunk"))

                 ((eql op (OP_LABEL)) (putstr  "  ir_label ") (putint arg) (putchar 10))
                 ((eql op (OP_JZ))    (putstr  "  ir_jz ") (putint arg) (putchar 10))
                 ((eql op (OP_JMP))   (putstr  "  ir_jmp ") (putint arg) (putchar 10))
                 ((eql op (OP_CALL))  (putline "  ir_call"))
                 ((eql op (OP_RET))   (putline "  ir_ret"))
                 
                 ((eql op (OP_ADDSP)) (putstr  "  ir_add_rsp ") (putint arg) (putchar 10))
                 ((eql op (OP_TCALL)) (putline "  ir_tcall"))
                 ((eql op (OP_POPAR)) (putstr  "  ir_pop_arg ") (putint arg) (putchar 10))
                 ((eql op (OP_PSHAR)) (putstr  "  ir_push_arg ") (putint arg) (putchar 10))
                 ((eql op (OP_ENTER)) (putline "  ir_enter"))
                 ((eql op (OP_LDFNC)) (putstr  "  ir_load_func ") (putint arg) (putchar 10))
                 ((eql op (OP_LDSTR)) (putstr  "  ir_load_str ") (putint arg) (putchar 10))
                 ((eql op (OP_STGLB)) (putstr  "  ir_store_glob global_") (putsym arg) (putchar 10))
                 ((eql op (OP_SYS))   (putstr  "  ir_syscall ") (putint arg) (putchar 10))

                 (1 (putstr "  ;; UNKNOWN IR OPCODE: ") (putint op) (putchar 10)))
               (emitir (cdr ir)))))))

      ;; --- 2.7 AST Rewriter Pass (Macro Expander) ---

      ;; Automatically tags raw cons-lists generated by macros with '2'
      (normlst (lambda (lst)
        (cond ((eql lst 0) 0)
              (1 (cons (normast (car lst)) (normlst (cdr lst)))))))

      (normast (lambda (ast)
        (cond ((eql ast 0) 0)
              ((gt (car ast) 3)  (cons 2 (normlst ast)))      ; Auto-tag raw lists!
              ((eql (car ast) 2) (cons 2 (normlst (cdr ast)))) ; Deep-copy/validate parsed lists
              (1 ast))))

      (ismacro (lambda (ast)
        (cond ((eql (car ast) 2)
               (let ((func (car (cdr ast))))
                 (cond ((eql (car func) 1) (symeq (cdr func) "macro")) (1 0))))
              (1 0))))

      (mapxnd (lambda (lst)
        (cond ((eql lst 0) 0)
              (1 (let ((expd (expand (car lst))))
                   (cond ((eql expd 0) (mapxnd (cdr lst)))
                         (1 (cons expd (mapxnd (cdr lst))))))))))

      (xndletb (lambda (bnds)
        (cond ((eql bnds 0) 0)
              (1 (let ((bndlst (cdr (car bnds))))
                   (let ((sym (car bndlst))
                         (val (car (cdr bndlst))))
                     (cond
                       ((ismacro val)
                        (let ((valist (cdr val)))
                          (let ((macargs (cdr (car (cdr valist))))
                                (macbody (car (cdr (cdr valist)))))
                            (poke macenv (cons (cons (cdr sym) (cons macargs macbody)) (peek macenv)))
                            (xndletb (cdr bnds)))))

                       (1
                        (cons (cons 2 (cons sym (cons (expand val) 0)))
                              (xndletb (cdr bnds)))))))))))

      (xndcnd (lambda (clauses)
        (cond ((eql clauses 0) 0)
              (1 (let ((clauselst (cdr (car clauses))))
                   (cons (cons 2 (cons (expand (car clauselst)) (mapxnd (cdr clauselst))))
                         (xndcnd (cdr clauses))))))))

      (expand (lambda (ast)
        (cond
          ((eql ast 0) 0)
          (1 (let ((tag (car ast)) (val (cdr ast)))
               (cond
                 ((eql tag 2)
                  (let ((func (car val)) (args (cdr val)))
                    (cond
                      ((eql (car func) 1)
                       (let ((fname (cdr func)))
                         (cond
                           ;; 1. Evaluate the macro, NORMALIZE it, then expand!
                           ((lkmac fname (peek macenv))
                            (let ((mdef (lkmac fname (peek macenv))))
                              (expand (normast (evalast (cdr mdef) (bndmac (car mdef) args 0))))))

                           ;; 2. Special Form: defmacro
                           ((symeq fname "defmacro")
                            (let ((msig (cdr (car args))))
                              (poke macenv (cons (cons (cdr (car msig)) (cons (cdr msig) (car (cdr args)))) (peek macenv)))
                              0))

                           ;; 3. Special Form: let
                           ((symeq fname "let")
                            (cons 2 (cons func (cons (cons 2 (xndletb (cdr (car args)))) (mapxnd (cdr args))))))

                           ;; 4. Special Form: cond
                           ((symeq fname "cond")
                            (cons 2 (cons func (xndcnd args))))

                           ;; 5. Special Form: lambda
                           ((symeq fname "lambda")
                            (cons 2 (cons func (cons (car args) (mapxnd (cdr args))))))

                           ;; 6. Special Form: quote
                           ((symeq fname "quote") ast)

                           ;; 7. Standard calls
                           (1 (cons 2 (cons func (mapxnd args)))))))

                      (1 (cons 2 (mapxnd val))))))
                 (1 ast)))))))

      ;; --- 3. The Code Generator ---
      (cmpnum (lambda (val)
        (cons (cons (OP_LDIMM) val) 0)))

      (emitclp (lambda (ptr len state)
        (cond
          ((gt len 0)
           (let ((b (logand (peek ptr) 255))
                 (isprint (and (ge b 32) (and (le b 126) (not (eql b 34))))))
             (cond
               (isprint
                (cond
                  ((eql state 0) (putchar 34) (putchar b) (emitclp (add ptr 1) (sub len 1) 1))
                  ((eql state 1) (putchar b) (emitclp (add ptr 1) (sub len 1) 1))
                  ((eql state 2) (putstr ", ") (putchar 34) (putchar b) (emitclp (add ptr 1) (sub len 1) 1))
                  (1 0)))
               (1
                (cond
                  ((eql state 0) (putint b) (emitclp (add ptr 1) (sub len 1) 2))
                  ((eql state 1) (putchar 34) (putstr ", ") (putint b) (emitclp (add ptr 1) (sub len 1) 2))
                  ((eql state 2) (putstr ", ") (putint b) (emitclp (add ptr 1) (sub len 1) 2))
                  (1 0))))))
           (1 (cond ((eql state 1) (putchar 34)) (1 0))))))

      (emitclpi (lambda (symint state count)
        (cond
          ((or (eql symint 0) (ge count 8))
           (cond ((eql state 1) (putchar 34)) (1 0)))
          (1
           (let ((b (logand symint 255))
                 (isprint (and (ge b 32) (and (le b 126) (not (eql b 34))))))
             (cond
               (isprint
                (cond
                  ((eql state 0) (putchar 34) (putchar b) (emitclpi (ash symint -8) 1 (add count 1)))
                  ((eql state 1) (putchar b) (emitclpi (ash symint -8) 1 (add count 1)))
                  ((eql state 2) (putstr ", ") (putchar 34) (putchar b) (emitclpi (ash symint -8) 1 (add count 1)))
                  (1 0)))
               (1
                (cond
                  ((eql state 0) (putint b) (emitclpi (ash symint -8) 2 (add count 1)))
                  ((eql state 1) (putchar 34) (putstr ", ") (putint b) (emitclpi (ash symint -8) 2 (add count 1)))
                  ((eql state 2) (putstr ", ") (putint b) (emitclpi (ash symint -8) 2 (add count 1)))
                  (1 0)))))))))

      (emitdata (lambda (prefix id val)
        (putline "align 8")
        (putstr prefix) (putint id) (putstr ": dq ")
        (putstr prefix) (putint id) (putstr "_data, ")
        (putint (sfcdr val)) (putchar 10)
        (putstr prefix) (putint id) (putline "_data:")
        (cond ((gt (sfcdr val) 0) (putstr "  db ") (emitclp (sfcar val) (sfcdr val) 0) (putchar 10)) (1 0))))

      (emsymdat (lambda (symint)
        (putline "align 8")
        (putstr "sym_") (putsym symint) (putstr ": dq sym_") (putsym symint) (putstr "_data, ")
        (putint (symlen symint)) (putchar 10)
        (putstr "sym_") (putsym symint) (putline "_data:")
        (cond ((gt symint 0) (putstr "  db ") (emitclpi symint 0 0) (putchar 10)) (1 0))))

      (allsyms (lambda (table)
        (cond (table (let ((entry (sfcar table))) (emsymdat (car entry)) (allsyms (cdr table)))) (1 0))))

      (allglobs (lambda (table)
        (cond (table (let ((entry (sfcar table))) (putline "align 8") (putstr "global_") (putsym (car entry)) (putline ": dq 0") (allglobs (cdr table)))) (1 0))))

      (allstrs (lambda (table)
        (cond (table (let ((entry (sfcar table)) (id (car entry)) (val (cdr entry))) (emitdata "STR_" id val) (allstrs (cdr table)))) (1 0))))

      (cntlocs (lambda (env)
        (cond ((eql env 0) 0)
              (1 (let ((val (cdr (car env)))) (cond ((lt val 0) (add 1 (cntlocs (cdr env)))) (1 (cntlocs (cdr env)))))))))

      ;; --- Control Flow ---
      (cmpif (lambda (args compenv istail pararity) 
        (let ((cnd (car args)) (thn (car (cdr args))) (els (car (cdr (cdr args)))) 
              (elsid (getid)) (endid (getid))) 
          (cmpexpr cnd compenv 0 pararity) 
          (emitir (cons (cons (OP_JZ) elsid) 0))
          (cmpexpr thn compenv istail pararity) 
          (emitir (cons (cons (OP_JMP) endid) 0))
          (emitir (cons (cons (OP_LABEL) elsid) 0))
          (cmpexpr els compenv istail pararity) 
          (emitir (cons (cons (OP_LABEL) endid) 0)))))

      (cmpcndc (lambda (clauses endid compenv istail pararity)
        (cond
          ((eql clauses 0) 
           (emitir (cons (cons (OP_LDIMM) 0) 0))
           (emitir (cons (cons (OP_LABEL) endid) 0)))
          (1 (let ((clause (car clauses)) (nextid (getid)) (clauslst (cdr clause))
                   (cnd (car clauslst)) (body (cdr clauslst)))
               (cmpexpr cnd compenv 0 pararity) 
               (emitir (cons (cons (OP_JZ) nextid) 0))
               (cmpseq body compenv istail pararity)
               (emitir (cons (cons (OP_JMP) endid) 0))
               (emitir (cons (cons (OP_LABEL) nextid) 0))
               (cmpcndc (cdr clauses) endid compenv istail pararity))))))
      
      ;; --- Bindings ---
      (cmpbnd (lambda (binding offset crrenv pararity)
        (let ((bnditems (cdr binding)) (varnode (car bnditems)) (valnode (car (cdr bnditems))))
          (cmpexpr valnode crrenv 0 pararity)
          (emitir (cons (cons (OP_PUSH) 0) 0))
          (cons (cons (cdr varnode) offset) crrenv))))

      (cmplet (lambda (args compenv istail pararity)
        (let ((bndslst (cdr (car args))) (bodystmt (cdr args)) (numbnds (cntbnds bndslst))
              (offset (mul -8 (add 1 (cntlocs compenv))))
              (newenv (cmpbnds bndslst offset compenv pararity)))
          (cmpseq bodystmt newenv istail pararity)
          (emitir (cons (cons (OP_ADDSP) (mul 8 numbnds)) 0)))))

      (cmpgbnds (lambda (bindings compenv)
        (cond
          (bindings
           (let ((bnditems (cdr (car bindings))) 
                 (varnode (car bnditems)) 
                 (valnode (car (cdr bnditems)))
                 (symint (cdr varnode)))
             
             (let ((existing (lksymi symint (peek globtab))))
               (cond (existing 0)
                     (1 (poke globtab (cons (cons symint 1) (peek globtab))))))
             
             (cmpexpr valnode compenv 0 0)
             (emitir (cons (cons (OP_PUSH) 0) 0))
             (emitir (cons (cons (OP_STGLB) symint) 0))
             (cmpgbnds (cdr bindings) compenv)))
          (1 0))))
      
      ;; --- Functions & Apply ---
      (cmplam (lambda (args compenv) 
        (let ((id (getid))) 
          (poke lams (cons (cons id args) (peek lams)))
          (emitir (cons (cons (OP_LDFNC) id) 0)))))

      (alllams (lambda ()
        (let ((lambdas (peek lams)))
          (cond
            (lambdas (poke lams (cdr lambdas))
             (let ((entry (car lambdas)) (id (car entry)) (args (cdr entry))
                   (params (cdr (car args))) (bodystmt (cdr args)) 
                   (numparam (cntbnds params)) 
                   (lamenv (bldpenv params -8 0)))
               (emitir (cons (cons (OP_LABEL) id) 0))
               (emitir (cons (cons (OP_ENTER) 0) 0))
               (pushargs numparam 0)
               (cmpseq bodystmt lamenv 1 numparam) 
               (emitir (cons (cons (OP_RET) 0) 0))
               (alllams)))
            (1 0)))))

      (cmpapp (lambda (funcnode args compenv istail pararity) 
        (let ((numargs (cntbnds args)))
          (cmpargs args compenv pararity) 
          (cmpexpr funcnode compenv 0 pararity)
          (popargs numargs 0) 
          (cond
            (istail (emitir (cons (cons (OP_TCALL) 0) 0)))
            (1 (emitir (cons (cons (OP_CALL) 0) 0)))))))

      (cmpargs (lambda (args compenv pararity) 
        (cond 
          (args (cmpargs (cdr args) compenv pararity) 
                (cmpexpr (car args) compenv 0 pararity) 
                (emitir (cons (cons (OP_PUSH) 0) 0)))
          (1 0))))

      (popargs (lambda (n i)
        (cond ((eql i n) 0)
              (1 (emitir (cons (cons (OP_POPAR) i) 0))
                 (popargs n (add i 1))))))

      (pushargs (lambda (n i)
        (cond ((eql i n) 0)
              (1 (emitir (cons (cons (OP_PSHAR) i) 0))
                 (pushargs n (add i 1))))))

      (cmpsys (lambda (args compenv pararity)
        (let ((numargs (cntbnds args)))
          (cmpargs args compenv pararity)
          (emitir (cons (cons (OP_POPRA) 0) 0))
          (popargs (sub numargs 1) 0)
          (emitir (cons (cons (OP_SYS) numargs) 0)))))

      (cmpbnds (lambda (bindings offset crrenv pararity)
        (cond (bindings (let ((newenv (cmpbnd (car bindings) offset crrenv pararity))) (cmpbnds (cdr bindings) (sub offset 8) newenv pararity))) (1 crrenv))))

      (cmpglet (lambda (args compenv)
        (putline "  ;; global let block")
        (cmpgbnds (cdr (car args)) compenv)
        (cmpseq (cdr args) compenv 0 0)))

      (cmpcond (lambda (args compenv istail pararity)
        (let ((endid (getid)))
          (putline "  ;; cond block") (cmpcndc args endid compenv istail pararity))))

      (cmpseq (lambda (stmts compenv istail pararity)
        (cond
          ((eql stmts 0) 0)
          (1 (let ((islast (eql (cdr stmts) 0)) (stmttail (cond (istail islast) (1 0))))
               (cmpexpr (car stmts) compenv stmttail pararity)
               (cmpseq (cdr stmts) compenv istail pararity))))))

      ;; --- 5. Function Definitions (Lambda & Apply) ---
      (cntbnds (lambda (bindings) (cond (bindings (add 1 (cntbnds (cdr bindings)))) (1 0))))

      (bldpenv (lambda (params offset baseenv)
        (cond (params (cons (cons (cdr (car params)) offset)
                            (bldpenv (cdr params) (sub offset 8) baseenv)))
              (1 baseenv))))

      ;; --- 6. Hardcoded Hardware Primitives ---
      (isnul (lambda (s) (cond ((symeq s "getchar") 1) ((symeq s "getheap") 1) ((symeq s "dict") 1) (1 0))))
      (isun (lambda (s)
        (cond ((symeq s "putint") 1) ((symeq s "puthex") 1) ((symeq s "putchar") 1) ((symeq s "putchunk") 1)
              ((symeq s "peek") 1) ((symeq s "alloc") 1) ((symeq s "setheap") 1) ((symeq s "car") 1)
              ((symeq s "cdr") 1) ((symeq s "not") 1) (1 0))))
      (isvarm (lambda (s)
        (cond ((symeq s "add") 1) ((symeq s "sub") 1) ((symeq s "mul") 1) ((symeq s "div") 1)
              ((symeq s "logior") 1) ((symeq s "or") 1) ((symeq s "logand") 1) ((symeq s "and") 1) (1 0))))
      (isbin (lambda (s)
        (cond ((symeq s "eql") 1) ((symeq s "lt") 1) ((symeq s "gt") 1) ((symeq s "le") 1) ((symeq s "ge") 1)
              ((symeq s "poke") 1) ((symeq s "pokebyte") 1) ((symeq s "peekidx") 1) ((symeq s "cons") 1)
              ((symeq s "ash") 1) (1 0))))

      (cmpvar (lambda (symint args compenv pararity)
        (cond
          (args (cmpexpr (car args) compenv 0 pararity) (cmpvarl symint (cdr args) compenv pararity))
          (1 (cond ((symeq symint "mul") (putline "  mov rax, 1")) (1 (putline "  mov rax, 0")))))))

      (cmpnul (lambda (symint)
        (let ((opir
               (cond
                 ((symeq symint "getchar") (OP_GTCHR))
                 ((symeq symint "getheap") (OP_GETHP))
                 ((symeq symint "dict")    (OP_DICT))
                 (1 0))))
          (emitir (cons (cons opir 0) 0)))))

      (cmpun (lambda (symint arg compenv pararity)
        (cmpexpr arg compenv 0 pararity)
        (let ((opir
               (cond
                 ((symeq symint "putint")   (OP_PTINT))
                 ((symeq symint "puthex")   (OP_PTHEX))
                 ((symeq symint "putchar")  (OP_PTCHR))
                 ((symeq symint "putchunk") (OP_PTCHK))
                 ((symeq symint "peek")     (OP_PEEK))
                 ((symeq symint "alloc")    (OP_ALLOC))
                 ((symeq symint "setheap")  (OP_STHEP))
                 ((symeq symint "car")      (OP_CAR))
                 ((symeq symint "cdr")      (OP_CDR))
                 ((symeq symint "not")      (OP_NOT))
                 (1 0))))
          (emitir (cons (cons opir 0) 0)))))

      (cmpbin (lambda (symint arg2 compenv pararity)
        (emitir (cons (cons (OP_PUSH) 0) 0))
        (cmpexpr arg2 compenv 0 pararity)
        (let ((opir
               (cond
                 ((symeq symint "add") (OP_ADD))
                 ((symeq symint "sub") (OP_SUB))
                 ((symeq symint "mul") (OP_MUL))
                 ((symeq symint "div") (OP_DIV))
                 ((symeq symint "eql") (OP_EQL))
                 ((symeq symint "lt")  (OP_LT))
                 ((symeq symint "gt")  (OP_GT))
                 ((symeq symint "le")  (OP_LE))
                 ((symeq symint "ge")  (OP_GE))
                 ((symeq symint "and") (OP_AND))
                 ((symeq symint "logand") (OP_AND))
                 ((symeq symint "or")  (OP_OR))
                 ((symeq symint "logior") (OP_OR))
                 ((symeq symint "ash") (OP_ASH))
                 ((symeq symint "poke") (OP_POKE))
                 ((symeq symint "pokebyte") (OP_POKEB))
                 ((symeq symint "peekidx") (OP_PEEKI))
                 ((symeq symint "cons") (OP_CONS))
                 (1 0))))
          (emitir (cons (cons (OP_SET2) 0)
                  (cons (cons opir 0) 0))))))

      (cmptern (lambda (symint arg1 arg2 arg3 compenv pararity)
        (cmpexpr arg1 compenv 0 pararity)
        (emitir (cons (cons (OP_PUSH) 0) 0))
        (cmpexpr arg2 compenv 0 pararity)
        (emitir (cons (cons (OP_PUSH) 0) 0))
        (cmpexpr arg3 compenv 0 pararity)
        (emitir (cons (cons (OP_SET2) 0) 0))
        (emitir (cons (cons (OP_POPR8) 0) 0))
        (let ((opir (cond ((symeq symint "pokeidx") (OP_POKEI)) (1 0))))
          (emitir (cons (cons opir 0) 0)))))

      ;; Make c*r resolution 100% IR compliant!
      (emitcadr (lambda (symint)
        (let ((c (logand symint 255)))
          (cond
            ((eql c 114) 1)
            (1 (emitcadr (ash symint -8))
               (cond ((eql c 97)  (emitir (cons (cons (OP_CAR) 0) 0)))
                     ((eql c 100) (emitir (cons (cons (OP_CDR) 0) 0)))
                     (1 0)))))))

      ;; Simplified cmpvarl - AST Optimization is gone, everything uses IR!
      (cmpvarl (lambda (symint args compenv pararity)
        (cond
          (args
           (cmpbin symint (car args) compenv pararity)
           (cmpvarl symint (cdr args) compenv pararity))
          (1 0))))

      ;; --- c[ad]+r Dynamic Bitwise Traversal ---
      (iscadrc (lambda (symint)
        (let ((c (logand symint 255)))
          (cond
            ((eql c 0) 0)
            ((eql c 114) (eql (ash symint -8) 0))
            ((or (eql c 97) (eql c 100)) (iscadrc (ash symint -8)))
            (1 0)))))

      (iscadr (lambda (symint)
        (let ((c (logand symint 255)))
          (cond
            ((eql c 99)
             (let ((c1 (logand (ash symint -8) 255)))
               (cond ((or (eql c1 97) (eql c1 100)) (iscadrc (ash symint -8))) (1 0))))
            (1 0)))))

      (cmplist (lambda (astlist compenv istail pararity)
        (let ((funcnode (car astlist)) (args (cdr astlist)))
          (cond
            ((eql (car funcnode) 1)
             (let ((funcname (cdr funcnode)))
               (cond
                 ;; All macro logic has been deleted here!
                 ((symeq funcname "let") (cmplet args compenv istail pararity))
                 ((symeq funcname "lambda") (cmplam args compenv))
                 ((symeq funcname "cond") (cmpcond args compenv istail pararity))
                 ((symeq funcname "if") (cmpif args compenv istail pararity))
                 ((eql (iscadr funcname) 1) (cmpexpr (car args) compenv 0 pararity) (putline "  ;; inline c*r") (emitcadr (ash funcname -8)))
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
                 ;; --- 1. Numbers now generate IR lists! ---
                 ((eql tag 0)
                  (emitir (cmpnum val)))

                 ;; --- 2. Variables now generate IR lists! ---
                 ((eql tag 1)
                  (let ((offset (lkenv val compenv)))
                    (cond
                      ((eql offset 0)
                       (cond ((lksymi val (peek globtab))
                              (emitir (cons (cons (OP_LDGLB) val) 0)))
                             (1 (putstr "ERROR: UNBOUND VARIABLE ") (putsym val) (putchar 10) (exit 1))))
                      (1 (emitir (cons (cons (OP_LDMEM) offset) 0))))))

                 ((eql tag 2) (cmplist val compenv istail pararity))
                 ((eql tag 3) (let ((id (getid))) (poke strtab (cons (cons id val) (peek strtab))) (emitir (cons (cons (OP_LDSTR) id) 0))))
                 (1 0)))))))

      (cmpprog (lambda (ast)
        (cond
          ((eql (car ast) 2)
           (let ((val (cdr ast)) (funcnode (car val)))
             (cond
               ((eql (car funcnode) 1) (cond ((symeq (cdr funcnode) "let") (cmpglet (cdr val) 0)) (1 (cmpexpr ast 0 0 0))))
               (1 (cmpexpr ast 0 0 0)))))
          (1 (cmpexpr ast 0 0 0)))))

      (cmploop (lambda ()
        (let ((rawast (prsexpr)))
          (cond
            ((and (eql (car rawast) 1) (eql (cdr rawast) 0)) 0)
            (1
             (let ((expast (expand rawast)))
               ;; Only pass to the code generator if it wasn't stripped out!
               (cond ((eql expast 0) 0)
                     (1 (cmpprog expast)))
               (cmploop))))))))

  ;; --- 7. THE MAIN ENTRY POINT ---
  (putline "format ELF64 executable 3")
  (putline "segment readable executable")
  (putline "include 'ir_macros.asm'")
  (putline "entry _start")
  (putline "_start:")
  (putline "  push rbp")
  (putline "  mov rbp, rsp")
  (putline "  lea r15, [heap_start]")

  (cmploop) ; We now evaluate top-level code blocks continuously!

  (putline "  mov rdi, rax")
  (putline "  mov rax, 60")
  (putline "  syscall")

  (putline "  ;; --- COMPILED FUNCTIONS ---")
  (alllams)

  (putline "segment readable writeable")
  (allsyms (peek symtab))
  (allglobs (peek globtab))
  (allstrs (peek strtab))
  (putline "align 8")
  (putline "global_dict: file 'dictionary.bin'")
  (putline "heap_start: rb 1024 * 1024 * 8"))
