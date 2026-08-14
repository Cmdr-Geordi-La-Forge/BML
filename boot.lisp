(let ((lookahd    (cons 0 0))
      (lblcnt     (cons 0 0))
      (macenv     (cons 0 0))
      (cmpenv     (cons 0 0))
      (symtab     (cons 0 0))
      (globtab    (cons 0 0))
      (strtab     (cons 0 0))
      (lams       (cons 0 0))
      (globprim   (cons 0 0))

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

      (emitopt (lambda (symint suffix arg)
        (let ((fullname (catstr (sym2str symint) suffix)))
          (let ((asm (lksym fullname (peek globprim))))
            (cond
              (asm 
               (putstr "  ;; inline optimized ") (putline fullname)
               (puttmpl (sfcar asm) (sfcdr asm) arg) 
               1) 
              (1 0))))))
      
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

      (fndbody (lambda (ptr end)
        (cond
          ((ge ptr end) end)
          ((eql (logand (peek ptr) 255) 10)
           (let ((nextptr (add ptr 1))
                 (nlpos   (findchar nextptr end 10))
                 (colpos  (findchar nextptr end 58))
                 (semipos (findchar nextptr end 59)))
             (cond
               ((and (gt colpos 0) (or (eql nlpos 0) (lt colpos nlpos)))
                (cond
                  ((and (gt semipos 0) (lt semipos colpos))
                   (fndbody nextptr end))
                  (1 nextptr))) 
               (1 (fndbody nextptr end)))))
          (1 (fndbody (add ptr 1) end)))))

      (prspriml (lambda (ptr end prims)
        (cond
          ((ge ptr end) prims)
          (1
           (let ((c (logand (peek ptr) 255)))
             (cond
               ((or (eql c 32) (eql c 9) (eql c 10) (eql c 13))
                (prspriml (add ptr 1) end prims))
               ((eql c 59)
                (let ((nlpos (findchar ptr end 10)))
                  (cond ((eql nlpos 0) prims)
                        (1 (prspriml (add nlpos 1) end prims)))))
               (1
                (let ((colpos (findchar ptr end 58)))
                  (cond
                    ((eql colpos 0) prims)
                    (1
                     (let ((labellen (sub colpos ptr))
                           (labelstr (cons ptr labellen))
                           (bodystrt (add colpos 1))
                           (bodyend  (fndbody bodystrt end))
                           (bodystr  (cons bodystrt (sub bodyend bodystrt)))
                           (newprims (cons (cons labelstr bodystr) prims)))
                       (prspriml bodyend end newprims))))))))))))

      (loadprim (lambda (filename)
        (let ((filedata (readfile filename)))
          (cond
            (filedata
             (poke globprim (prspriml (car filedata) (add (car filedata) (cdr filedata)) 0)))
            (1 (putline "Error: Could not load primitives.asm"))))))

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
                      ((symeq funcname "car") (car (evalast (car args) env)))
                      ((symeq funcname "cdr") (cdr (evalast (car args) env)))
                      ((symeq funcname "cons") (cons (evalast (car args) env) (evalast (car (cdr args)) env)))
                      ((symeq funcname "eql") (cond ((eql (evalast (car args) env) (evalast (car (cdr args)) env)) 1) (1 0)))
                      ((symeq funcname "if") (cond ((evalast (car args) env) (evalast (car (cdr args)) env)) (1 (evalast (car (cdr (cdr args))) env))))
                      (1 0))))
                 (1 0)))))))

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

      (mapxnd (lambda (lst)
        (cond ((eql lst 0) 0)
              (1 (let ((expd (expand (car lst))))
                   (cond ((eql expd 0) (mapxnd (cdr lst))) 
                         (1 (cons expd (mapxnd (cdr lst))))))))))

      (xndletb (lambda (bnds)
        (cond ((eql bnds 0) 0)
              (1 (let ((bndlst (cdr (car bnds))))
                   (cons (cons 2 (cons (car bndlst) (cons (expand (car (cdr bndlst))) 0)))
                         (xndletb (cdr bnds))))))))

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
      (cmpnum (lambda (val) (putstr "  mov rax, ") (putint val) (putchar 10)))

      (emitsym (lambda (symint)
        (let ((existing (lksymi symint (peek symtab))))
          (cond (existing 0)
                (1 (poke symtab (cons (cons symint 1) (peek symtab))))))
        (putstr "  lea rdi, [sym_") (putsym symint) (putline "]")))

      (emitglob (lambda (symint)
        (let ((existing (lksymi symint (peek globtab))))
          (cond (existing 0)
                (1 (poke globtab (cons (cons symint 1) (peek globtab))))))
        (putstr "  lea rdi, [global_") (putsym symint) (putline "]")))

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

      (cmpbnd (lambda (binding offset crrenv pararity)
        (let ((bnditems (cdr binding)) (varnode (car bnditems)) (valnode (car (cdr bnditems))))
          (cmpexpr valnode crrenv 0 pararity)
          (putline "  push rax")
          (cons (cons (cdr varnode) offset) crrenv))))

      (cmpbnds (lambda (bindings offset crrenv pararity)
        (cond (bindings (let ((newenv (cmpbnd (car bindings) offset crrenv pararity))) (cmpbnds (cdr bindings) (sub offset 8) newenv pararity))) (1 crrenv))))

      (cmplet (lambda (args compenv istail pararity)
        (let ((bndslst (cdr (car args))) (bodystmt (cdr args)) (numbnds (cntbnds bndslst))
              (offset (mul -8 (add 1 (cntlocs compenv))))
              (newenv (cmpbnds bndslst offset compenv pararity)))
          (putline "  ;; let block")
          (cmpseq bodystmt newenv istail pararity)
          (putstr "  add rsp, ") (putint (mul 8 numbnds)) (putline " ; Pop let bindings"))))

      (cmpgbnds (lambda (bindings compenv)
        (cond
          (bindings
           (let ((bnditems (cdr (car bindings))) (varnode (car bnditems)) (valnode (car (cdr bnditems))))
             (cmpexpr valnode compenv 0 0) (putline "  push rax")
             (emitglob (cdr varnode)) (putline "  pop rax") (putline "  mov [rdi], rax")
             (cmpgbnds (cdr bindings) compenv)))
          (1 0))))

      (cmpglet (lambda (args compenv)
        (putline "  ;; global let block")
        (cmpgbnds (cdr (car args)) compenv)
        (cmpseq (cdr args) compenv 0 0)))

      ;; --- 4. Special Forms & Native COND ---
      (cmpif (lambda (args compenv istail pararity) 
        (let ((cnd (car args)) (thn (car (cdr args))) (els (car (cdr (cdr args)))) (id (getid))) 
          (putline "  ;; if block") 
          (cmpexpr cnd compenv 0 pararity) (putline "  test rax, rax") 
          (putstr "  jz IF_ELS_") (putint id) (putchar 10) 
          (cmpexpr thn compenv istail pararity) (putstr "  jmp IF_END_") (putint id) (putchar 10) 
          (putstr "IF_ELS_") (putint id) (putline ":") 
          (cmpexpr els compenv istail pararity) (putstr "IF_END_") (putint id) (putline ":"))))

      (cmpcndc (lambda (clauses endid compenv istail pararity)
        (cond
          ((eql clauses 0) (putline "  mov rax, 0") (putstr "COND_END_") (putint endid) (putline ":"))
          (1 (let ((clause (car clauses)) (nextid (getid)) (clauslst (cdr clause))
                   (cnd (car clauslst)) (body (cdr clauslst)))
               (cmpexpr cnd compenv 0 pararity) (putline "  test rax, rax")
               (putstr "  jz COND_NEXT_") (putint nextid) (putchar 10)
               (cmpseq body compenv istail pararity)
               (putstr "  jmp COND_END_") (putint endid) (putchar 10)
               (putstr "COND_NEXT_") (putint nextid) (putline ":")
               (cmpcndc (cdr clauses) endid compenv istail pararity))))))

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

      (alllams (lambda ()
        (let ((lambdas (peek lams)))
          (cond
            (lambdas (poke lams (cdr lambdas))
             (let ((entry (car lambdas)) (id (car entry)) (args (cdr entry))
                   (params (cdr (car args))) (bodystmt (cdr args)) 
                   (numparam (cntbnds params)) 
                   (lamenv (bldpenv params -8 0))) ; <-- Local frame mapping
               (putstr "L_START_") (putint id) (putline ":") 
               (putline "  push rbp") 
               (putline "  mov rbp, rsp") 
               (pushargs numparam 0) ; <-- Push C ABI registers to locals
               (cmpseq bodystmt lamenv 1 numparam) 
               (putline "  mov rsp, rbp") 
               (putline "  pop rbp") 
               (putline "  ret") ; <-- Standard return!
               (alllams)))
            (1 0)))))

      (cmpapp (lambda (funcnode args compenv istail pararity) 
        (let ((numargs (cntbnds args)))
          (cmpargs args compenv pararity) 
          (cmpexpr funcnode compenv 0 pararity)
          
          (putline "  ;; load C ABI registers")
          (popargs numargs 0) 
          
          (cond
            (istail
             (putline "  ;; tail-call")
             (putline "  mov rsp, rbp")
             (putline "  pop rbp")
             (putline "  jmp rax")) ; <-- Hardware TCE
            (1 (putline "  call rax"))))))
      
      (cmplam (lambda (args compenv) 
        (let ((id (getid))) 
          (poke lams (cons (cons id args) (peek lams)))
          (putstr "  lea rax, [L_START_") (putint id) (putline "]"))))

      (cmpargs (lambda (args compenv pararity) 
        (cond 
          (args (cmpargs (cdr args) compenv pararity) 
                (cmpexpr (car args) compenv 0 pararity) 
                (putline "  push rax")) 
          (1 0))))

      (popargs (lambda (n i)
        (cond ((eql i n) 0)
              (1 (putstr "  pop ")
                 (cond ((eql i 0) (putline "rdi"))
                       ((eql i 1) (putline "rsi"))
                       ((eql i 2) (putline "rdx"))
                       ((eql i 3) (putline "rcx"))
                       ((eql i 4) (putline "r8"))
                       ((eql i 5) (putline "r9")))
                 (popargs n (add i 1))))))

      (pushargs (lambda (n i)
        (cond ((eql i n) 0)
              (1 (putstr "  push ")
                 (cond ((eql i 0) (putline "rdi"))
                       ((eql i 1) (putline "rsi"))
                       ((eql i 2) (putline "rdx"))
                       ((eql i 3) (putline "rcx"))
                       ((eql i 4) (putline "r8"))
                       ((eql i 5) (putline "r9")))
                 (pushargs n (add i 1))))))

      (cmpsys (lambda (args compenv pararity)
        (let ((numargs (cntbnds args)))
          (cmpargs args compenv pararity)
          (putline "  ;; syscall")
          (putline "  pop rax")
          (popargs (sub numargs 1) 0)
          
          ;; CFFI Bridge: The kernel expects the 4th arg in r10, not rcx.
          ;; If we have 4+ args (numargs >= 5 including syscall number), move it!
          (cond ((ge numargs 5) (putline "  mov r10, rcx")) (1 0))
          
          (putline "  syscall"))))

      ;; --- 6. Hardcoded Hardware Primitives ---
      (isnul (lambda (s) (cond ((symeq s "getchar") 1) ((symeq s "getheap") 1) (1 0))))
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

      (cmpvarl (lambda (symint args compenv pararity)
        (cond
          (args
           (let ((nextarg (car args)) (tag2 (car nextarg)) (val2 (cdr nextarg)))
             (cond
               ((eql tag2 0)
                (cond
                  ((symeq symint "ash")
                   (cond ((ge val2 0) (emitopt symint "_left_imm" val2)) (1 (emitopt symint "_right_imm" (sub 0 val2)))))
                  ((emitopt symint "_imm" val2) 1) 
                  (1 (cmpbin symint nextarg compenv pararity))))
               ((eql tag2 1)
                (let ((offset (lkenv val2 compenv)))
                  (cond
                    ((lt offset 0) (cond ((emitopt symint "_local" (sub 0 offset)) 1) (1 (cmpbin symint nextarg compenv pararity))))
                    ((gt offset 0) (cond ((emitopt symint "_arg" offset) 1) (1 (cmpbin symint nextarg compenv pararity))))
                    (1 (cmpbin symint nextarg compenv pararity)))))
               (1 (cmpbin symint nextarg compenv pararity))))
           (cmpvarl symint (cdr args) compenv pararity))
          (1 0))))
      
      (cmpvar (lambda (symint args compenv pararity)
        (cond
          (args (cmpexpr (car args) compenv 0 pararity) (cmpvarl symint (cdr args) compenv pararity))
          (1 (cond ((symeq symint "mul") (putline "  mov rax, 1")) (1 (putline "  mov rax, 0")))))))

      (cmpnul (lambda (symint) 
        (putstr "  ;; inline ") (putsym symint) (putchar 10)
        (let ((asm (lksym (sym2str symint) (peek globprim)))) (cond (asm (putstr asm)) (1 0)))))

      (cmpun (lambda (symint arg compenv pararity) 
        (cmpexpr arg compenv 0 pararity) (putstr "  ;; inline ") (putsym symint) (putchar 10)
        (let ((asm (lksym (sym2str symint) (peek globprim)))) (cond (asm (putstr asm)) (1 0)))))

      (cmptern (lambda (symint arg1 arg2 arg3 compenv pararity)
        (cmpexpr arg1 compenv 0 pararity) (putline "  push rax") (cmpexpr arg2 compenv 0 pararity)
        (putline "  push rax") (cmpexpr arg3 compenv 0 pararity) (putline "  pop rcx")
        (putline "  pop r8") (putstr "  ;; inline ") (putsym symint) (putchar 10)
        (let ((asm (lksym (sym2str symint) (peek globprim)))) (cond (asm (putstr asm)) (1 0)))))

      (cmpbin (lambda (symint arg2 compenv pararity)
        (putline "  push rax") (cmpexpr arg2 compenv 0 pararity) (putline "  mov rcx, rax")
        (putline "  pop rax") (putstr "  ;; inline ") (putsym symint) (putchar 10)
        (cond 
          ((symeq symint "ash") 
           (let ((id (getid)))
             (putline "  test rcx, rcx") (putstr "  jns ASH_LEFT_") (putint id) (putchar 10)
             (putline "  neg rcx") (putline "  sar rax, cl") (putstr "  jmp ASH_DONE_") (putint id) (putchar 10)
             (putstr "ASH_LEFT_") (putint id) (putline ":") (putline "  shl rax, cl")
             (putstr "ASH_DONE_") (putint id) (putline ":")))
          (1 (let ((asm (lksym (sym2str symint) (peek globprim)))) (cond (asm (putstr asm)) (1 0)))))))

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

      (emitcadr (lambda (symint)
        (let ((c (logand symint 255)))
          (cond
            ((eql c 114) 1)
            (1 (emitcadr (ash symint -8))
               (cond ((eql c 97) (putline "  mov rax, [rax]"))
                     ((eql c 100) (putline "  mov rax, [rax+8]"))
                     (1 0)))))))

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
                 ((eql tag 0) (cmpnum val))
                 ((eql tag 1) 
                  (let ((offset (lkenv val compenv)))
                    (cond
                      ((eql offset 0) (putstr "  ;; global lookup") (putchar 10) (emitglob val) (putline "  mov rax, [rdi]"))
                      ((gt offset 0) (putstr "  ;; arg lookup") (putchar 10) (putstr "  mov rax, [rbp + ") (putint offset) (putline "]"))
                      (1 (putstr "  ;; local lookup") (putchar 10) (putstr "  mov rax, [rbp - ") (putint (sub 0 offset)) (putline "]")))))
                 ((eql tag 2) (cmplist val compenv istail pararity))
                 ((eql tag 3) (let ((id (getid))) (poke strtab (cons (cons id val) (peek strtab))) (putline "  ;; string literal") (putstr "  lea rax, [STR_") (putint id) (putline "]")))
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
  (putline "include 'runtime.asm'")
  (putline "entry _start")
  (putline "_start:")
  (putline "  push rbp")
  (putline "  mov rbp, rsp")
  (putline "  lea r15, [heap_start]")
  
  (loadprim "primitives.asm")
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
  (putline "heap_start: rb 1024 * 1024 * 8"))
