(let ((lookahd         (cons 0 0))
      (lblcnt     (cons 0 0))
      (cmpenv       (cons 0 0))
      (symtab      (cons 0 0))
      (globtab      (cons 0 0))
      (strtab      (cons 0 0))
      (lams   (cons 0 0))
      (globprim (cons 0 0))

      ;; --- 1. I/O & Memory Safety ---
      (nxtchar (lambda ()
        (let ((c (peek lookahd)))
          (cond (c (poke lookahd 0) c) (1 (getchar))))))
      (ungtchar (lambda (c) (poke lookahd c)))

      (sfcar (lambda (l) (if (eql l 0) 0 (car l))))
      (sfcdr (lambda (l) (if (eql l 0) 0 (cdr l))))

      (isspace (lambda (c) (or (eql c 32) (eql c 10) (eql c 13) (eql c 9))))
      (isdigit (lambda (c) (and (ge c 48) (le c 57))))
      (isdelim (lambda (c)
                 (or (isspace c) (or (eql c 0) (eql c 40) (eql c 41)))))

      (skpcom (lambda ()
        (let ((c (nxtchar)))
          (cond ((or (eql c 10) (eql c 0))
                 (skpws))
                (1 (skpcom))))))
      (skpws  (lambda ()
        (let ((c (nxtchar)))
          (cond ((isspace c) (skpws))
                ((eql c 59)  (skpcom))
                (1 (ungtchar c))))))

      (putstr  (lambda (s) (syscall 1 1 (sfcar s) (sfcdr s))))
      (putline (lambda (s) (putstr s) (putchar 10)))

      (catstr (lambda (s1 s2)
        (let ((len1     (sfcdr s1))
              (len2     (sfcdr s2))
              (totallen (add len1 len2))
              (aligned  (mul (div (add totallen 8) 8) 8))
              (ptr      (getheap)))
          (alloc (div aligned 8))
          (cpybytes (sfcar s1) ptr len1)
          (cpybytes (sfcar s2) (add ptr len1) len2)
          (cons ptr totallen))))

      (puttmpl (lambda (ptr len arg)
        (cond
          ((le len 0) 0)
          (1 (let ((c (logand (peek ptr) 255)))
               (cond
                 ((and (eql c 37) (gt len 1)) ; Check for '%'
                  (let ((nextc (logand (peek (add ptr 1)) 255)))
                    (cond
                      ((eql nextc 49) ; Check for '1'
                       (putint arg) ; Substitute!
                       (puttmpl (add ptr 2) (sub len 2) arg))
                      (1 (putchar c) (puttmpl (add ptr 1) (sub len 1) arg)))))
                 (1 (putchar c) (puttmpl (add ptr 1) (sub len 1) arg))))))))

      ;; --- Integer Symbol Utilities ---
      
      (symlenc (lambda (symint count)
        (cond ((or (eql symint 0) (ge count 8)) count)
              (1 (symlenc (ash symint -8) (add count 1))))))
      (symlen (lambda (symint) (symlenc symint 0)))

      (sym2str (lambda (symint)
        (let ((ptr (getheap)))
          (poke ptr symint)
          (alloc 1) ; Allocate exactly 1 block (8 bytes)
          (cons ptr (symlen symint)))))

      ;; Safely packs an arbitrary string literal into our 64-bit integer representation
      (str2symc (lambda (ptr len shift acc)
        (cond ((or (le len 0) (ge shift 64)) acc)
              (1 (str2symc (add ptr 1) (sub len 1) (add shift 8)
                           (logior acc (ash (logand (peek ptr) 255) shift)))))))

      (symeq (lambda (symint str)
        ;; Dynamic packing avoids ANY reliance on assembler zero/NOP padding behavior!
        (eql symint (str2symc (sfcar str) (sfcdr str) 0 0))))

      (putsymc (lambda (symint count)
        (cond ((or (eql symint 0) (ge count 8)) 0)
              (1 (putchar (logand symint 255))
                 (putsymc (ash symint -8) (add count 1))))))
      (putsym (lambda (symint) (putsymc symint 0)))

      (emitopt (lambda (symint suffix arg)
        (let ((fullname (catstr (sym2str symint) suffix)))
          (let ((asm (lksym fullname (peek globprim))))
            (cond
              (asm 
               (putstr "  ;; inline optimized ") (putline fullname)
               (puttmpl (sfcar asm) (sfcdr asm) arg) 
               1) ; Return 1 on success
              (1 0)))))) ; Return 0 on failure
      
      (getbyte (lambda (chunk charidx)
        (logand (ash chunk (mul charidx -8)) 255)))

      (streqbl (lambda (ptr1 ptr2 len idx)
        (cond 
          ((ge idx len) 1)
          (1 (let ((chunkidx (div idx 8))
                   (charidx (sub idx (mul chunkidx 8)))
                   (chunk1 (peek (add ptr1 (mul chunkidx 8))))
                   (chunk2 (peek (add ptr2 (mul chunkidx 8))))
                   (c1 (getbyte chunk1 charidx))
                   (c2 (getbyte chunk2 charidx)))
               (cond 
                 ((eql c1 c2) (streqbl ptr1 ptr2 len (add idx 1)))
                 (1 0)))))))

      (streq (lambda (s1 s2)
        (cond ((and (eql s1 0) (eql s2 0)) 1)
              ((or (eql s1 0) (eql s2 0)) 0)
              ((eql (sfcdr s1) (sfcdr s2)) 
               (streqbl (sfcar s1) (sfcar s2) (sfcdr s1) 0))
              (1 0))))
      
      (getid (lambda () (let ((id (peek lblcnt))) (poke lblcnt (add id 1)) id)))

      ;; --- 2. The Lexer/Parser ---

      (prsint (lambda (acc)
        (let ((c (nxtchar)))
          (cond ((isdigit c) (prsint (add (mul acc 10) (sub c 48))))
                (1 (ungtchar c) acc)))))

      (prsnumc (lambda (acc shift)
        (let ((c (nxtchar)))
          (cond
            ((isdelim c) (ungtchar c) acc)
            ((ge shift 64) (prsnumc acc shift)) ; Drop chars beyond 8 to protect memory
            (1 (prsnumc (logior acc (ash c shift)) (add shift 8)))))))

      (prssym (lambda () 
        ;; Symbol logic is now O(1) packing. No heap tracking needed.
        (prsnumc 0 0)))
      
      (padzeros (lambda (ptr count target)
        (cond ((lt count target)
               (pokebyte (add ptr count) 0)
               (padzeros ptr (add count 1) target))
              (1 0))))

      (prsstrl (lambda (ptr count)
        (let ((c (nxtchar)))
          (cond
            ((or (eql c 34) (eql c 0))
             ;; Find the 8-byte boundary
             (let ((aligncnt (mul (div (add count 8) 8) 8)))
               (padzeros ptr count aligncnt)
               (alloc (div aligncnt 8))
               (cons ptr count)))
            
            ;; Intercept backslash (ASCII 92) for escape sequences
            ((eql c 92)
             (let ((esc (nxtchar)))
               (cond
                 ((eql esc 110) (pokebyte (add ptr count) 10)) ; \n
                 ((eql esc 116) (pokebyte (add ptr count) 9))  ; \t
                 ((eql esc 114) (pokebyte (add ptr count) 13)) ; \r
                 ((eql esc 92)  (pokebyte (add ptr count) 92)) ; \\
                 ((eql esc 34)  (pokebyte (add ptr count) 34)) ; \"
                 (1 (pokebyte (add ptr count) esc)))           ; Fallback
               (prsstrl ptr (add count 1))))

            (1
             (pokebyte (add ptr count) c)
             (prsstrl ptr (add count 1)))))))

      (prsstr (lambda () 
        ;; Get the current heap pointer and start writing at offset 0
        (prsstrl (getheap) 0)))
      
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

      (cpybytes (lambda (src dst len)
        (cond ((gt len 0)
               (pokebyte dst (logand (peek src) 255))
               (cpybytes (add src 1) (add dst 1) (sub len 1)))
              (1 0))))

      (nulterm (lambda (s)
        (let ((len (sfcdr s))
              (ptr (getheap)))
          (alloc (add (div len 8) 1))
          (cpybytes (sfcar s) ptr len)
          (pokebyte (add ptr len) 0)
          ptr)))

      (readfile (lambda (filename)
        (let ((ptr (nulterm filename)))
          (let ((fd (syscall 2 ptr 0 0))) ; sys_open (fd = 2)
            (cond
              ((lt fd 0) 0)
              (1
               (let ((buf (getheap)))
                 ;; sys_read (fd = 0), load up to 64KB
                 (let ((byteread (syscall 0 fd buf 65536)))
                   (syscall 3 fd) ; sys_close (fd = 3)
                   
                   ;; --- EOF SANITIZATION ---
                   ;; Unconditionally append a newline byte at the EOF memory address
                   (pokebyte (add buf byteread) 10)
                   
                   (let ((safelen (add byteread 1)))
                     ;; Commit the allocation including our extra byte
                     (alloc (add (div safelen 8) 1))
                     (cons buf safelen))))))))))

      (findchar (lambda (ptr end c)
        (cond ((ge ptr end) 0)
              ((eql (logand (peek ptr) 255) c) ptr)
              (1 (findchar (add ptr 1) end c)))))

      (fndbody (lambda (ptr end)
        (cond
          ((ge ptr end) end)
          ((eql (logand (peek ptr) 255) 10) ; Reached a newline
           (let ((nextptr (add ptr 1))
                 (nlpos   (findchar nextptr end 10))
                 (colpos  (findchar nextptr end 58))
                 (semipos (findchar nextptr end 59)))
             (cond
               ;; If a colon exists on the next line...
               ((and (gt colpos 0) (or (eql nlpos 0) (lt colpos nlpos)))
                ;; And it is NOT hidden inside a comment...
                (cond
                  ((and (gt semipos 0) (lt semipos colpos))
                   (fndbody nextptr end))
                  (1 nextptr))) ; It is a real label! The body ends here!
               
               ;; Otherwise, the body continues
               (1 (fndbody nextptr end)))))
          (1 (fndbody (add ptr 1) end)))))

      (prspriml (lambda (ptr end prims)
        (cond
          ((ge ptr end) prims)
          (1
           (let ((c (logand (peek ptr) 255)))
             (cond
               ;; 1. Skip whitespace (Space=32, Tab=9, NL=10, CR=13)
               ((or (eql c 32) (or (eql c 9) (or (eql c 10) (eql c 13))))
                (prspriml (add ptr 1) end prims))
               
               ;; 2. Skip comment lines (';' = 59)
               ((eql c 59)
                (let ((nlpos (findchar ptr end 10)))
                  (cond ((eql nlpos 0) prims)
                        (1 (prspriml (add nlpos 1) end prims)))))
               
               ;; 3. Parse actual label and body
               (1
                (let ((colpos (findchar ptr end 58))) ; Find ':'
                  (cond
                    ((eql colpos 0) prims)
                    (1
                     ;; Flattened sequential let bindings!
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
             (poke globprim 
                   (prspriml (sfcar filedata) 
                             (add (sfcar filedata) (sfcdr filedata)) 
                             0)))
            (1 (putline "Error: Could not load primitives.asm"))))))

      ;; --- 2.5 Cmp-Time Environment ---
      
      (lksym (lambda (str table)
        (cond ((eql table 0) 0)
              (1 (let ((entry (sfcar table)))
                   (cond ((streq str (sfcar entry)) (sfcdr entry))
                         (1 (lksym str (sfcdr table)))))))))
                         
      (lksymi (lambda (symint table)
        (cond ((eql table 0) 0)
              (1 (let ((entry (sfcar table)))
                   (cond ((eql symint (sfcar entry)) (sfcdr entry))
                         (1 (lksymi symint (sfcdr table)))))))))
                         
      (lkmac (lambda (symint env)
        (cond ((eql env 0) 0)
              (1 (let ((binding (sfcar env)))
                   (cond ((eql symint (sfcar binding))
                          (sfcdr binding))
                         (1 (lkmac symint (sfcdr env)))))))))
                         
      (bndmac (lambda (params args env)
                (cond (params (cons (cons (sfcdr (sfcar params)) (sfcar args))
                                    (bndmac (sfcdr params) (sfcdr args) env)))
                      (1 env))))
      
      (lkenv (lambda (symint env)
        (cond
          ((eql env 0) 0) 
          (1 (let ((binding (sfcar env)))
               (cond
                 ((eql symint (sfcar binding)) (sfcdr binding)) 
                 (1 (lkenv symint (sfcdr env)))))))))

      (evalast (lambda (ast env)
        (cond
          ((eql ast 0) 0)
          (1 (let ((tag (sfcar ast)) (val (sfcdr ast)))
               (cond
                 ((eql tag 0) val)
                 ((eql tag 3) val)
                 ((eql tag 1) (lkmac val env))
                 ((eql tag 2)
                  ;; Extract the pure integer symbol payload natively
                  (let ((funcname (sfcdr (sfcar val))) (args (sfcdr val)))
                    (cond
                      ((symeq funcname "quote")
                       (sfcar args))
                      ((symeq funcname "car")
                       (sfcar (evalast (sfcar args) env)))
                      ((symeq funcname "cdr")
                       (sfcdr (evalast (sfcar args) env)))
                      ((symeq funcname "cons")
                       (cons (evalast (sfcar args) env)
                             (evalast (sfcar (sfcdr args)) env)))
                      ((symeq funcname "eql")
                       (cond ((eql (evalast (sfcar args) env)
                                   (evalast (sfcar (sfcdr args)) env))
                              1)
                             (1 0)))
                      ((symeq funcname "if")
                       (cond ((evalast (sfcar args) env)
                              (evalast (sfcar (sfcdr args)) env))
                             (1 (evalast (sfcar (sfcdr (sfcdr args))) env))))
                      (1 0))))
                 (1 0)))))))

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

      ;; --- DATA SECTION EMITTERS ---
      
      (emitclp (lambda (ptr len state)
        (cond
          ((gt len 0)
           (let ((b (logand (peek ptr) 255))
                 (isprint (and (ge b 32) (and (le b 126) (not (eql b 34))))))
             (cond
               (isprint
                (cond
                  ((eql state 0)
                   (putchar 34) (putchar b) (emitclp (add ptr 1) (sub len 1) 1))
                  ((eql state 1)
                   (putchar b) (emitclp (add ptr 1) (sub len 1) 1))
                  ((eql state 2)
                   (putstr ", ") (putchar 34) (putchar b)
                   (emitclp (add ptr 1) (sub len 1) 1))
                  (1 0)))
               (1
                (cond
                  ((eql state 0)
                   (putint b) (emitclp (add ptr 1) (sub len 1) 2))
                  ((eql state 1)
                   (putchar 34) (putstr ", ") (putint b)
                   (emitclp (add ptr 1) (sub len 1) 2))
                  ((eql state 2)
                   (putstr ", ") (putint b)
                   (emitclp (add ptr 1) (sub len 1) 2))
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
                  ((eql state 0)
                   (putchar 34) (putchar b) (emitclpi (ash symint -8) 1 (add count 1)))
                  ((eql state 1)
                   (putchar b) (emitclpi (ash symint -8) 1 (add count 1)))
                  ((eql state 2)
                   (putstr ", ") (putchar 34) (putchar b)
                   (emitclpi (ash symint -8) 1 (add count 1)))
                  (1 0)))
               (1
                (cond
                  ((eql state 0)
                   (putint b) (emitclpi (ash symint -8) 2 (add count 1)))
                  ((eql state 1)
                   (putchar 34) (putstr ", ") (putint b)
                   (emitclpi (ash symint -8) 2 (add count 1)))
                  ((eql state 2)
                   (putstr ", ") (putint b)
                   (emitclpi (ash symint -8) 2 (add count 1)))
                  (1 0)))))))))

      (emitdata (lambda (prefix id val)
        (putline "align 8")
        (putstr prefix) (putint id) (putstr ": dq ")
        (putstr prefix) (putint id) (putstr "_data, ")
        (putint (sfcdr val)) (putchar 10)
        
        (putstr prefix) (putint id) (putline "_data:")
        (cond ((gt (sfcdr val) 0)
               (putstr "  db ")
               (emitclp (sfcar val) (sfcdr val) 0)
               (putchar 10))
              (1 0))))

      (emsymdat (lambda (symint)
        (putline "align 8")
        (putstr "sym_") (putsym symint) (putstr ": dq sym_") (putsym symint) (putstr "_data, ")
        (putint (symlen symint)) (putchar 10)
        
        (putstr "sym_") (putsym symint) (putline "_data:")
        (cond ((gt symint 0)
               (putstr "  db ")
               (emitclpi symint 0 0)
               (putchar 10))
              (1 0))))

      (allsyms (lambda (table)
        (cond
          (table
           (let ((entry (sfcar table)))
             (emsymdat (sfcar entry))
             (allsyms (sfcdr table))))
          (1 0))))

      (allglobs (lambda (table)
        (cond
          (table
           (let ((entry (sfcar table)))
             (putline "align 8")
             (putstr "global_") (putsym (sfcar entry)) (putline ": dq 0")
             (allglobs (sfcdr table))))
          (1 0))))

      (allstrs (lambda (table)
        (cond
          (table
           (let ((entry (sfcar table))
                 (id    (sfcar entry))
                 (val   (sfcdr entry)))
             (emitdata "STR_" id val)
             (allstrs (sfcdr table))))
          (1 0))))

      (cntlocs (lambda (env)
        (cond
          ((eql env 0) 0)
          (1 (let ((val (sfcdr (sfcar env))))
               (cond
                 ((lt val 0) (add 1 (cntlocs (sfcdr env))))
                 (1 (cntlocs (sfcdr env)))))))))

      (cmpbnd (lambda (binding offset crrenv pararity)
        (let ((bnditems (sfcdr binding))
              (varnode  (sfcar bnditems))
              (valnode  (sfcar (sfcdr bnditems))))
          (cmpexpr valnode crrenv 0 pararity)
          (putline "  push rax")
          (cons (cons (sfcdr varnode) offset) crrenv))))

      (cmpbnds (lambda (bindings offset crrenv pararity)
        (cond
          (bindings
           (let ((newenv (cmpbnd (sfcar bindings) offset crrenv pararity)))
             (cmpbnds (sfcdr bindings) (sub offset 8) newenv pararity)))
          (1 crrenv))))

      (cmplet (lambda (args compenv istail pararity)
        (let ((bndslst (sfcdr (sfcar args)))
              (bodystmt (sfcdr args))
              (numbnds (cntbnds bndslst))
              (offset (mul -8 (add 1 (cntlocs compenv))))
              (newenv (cmpbnds bndslst offset compenv pararity)))
          (putline "  ;; let block")
          (cmpseq bodystmt newenv istail pararity)
          (putstr "  add rsp, ") (putint (mul 8 numbnds)) (putline " ; Pop let bindings"))))

      (cmpgbnds (lambda (bindings compenv)
        (cond
          (bindings
           (let ((bnditems (sfcdr (sfcar bindings)))
                 (varnode (sfcar bnditems))
                 (valnode (sfcar (sfcdr bnditems))))
             (cmpexpr valnode compenv 0 0)
             (putline "  push rax")
             (emitglob (sfcdr varnode))
             (putline "  pop rax")
             (putline "  mov [rdi], rax")
             (cmpgbnds (sfcdr bindings) compenv)))
          (1 0))))

      (cmpglet (lambda (args compenv)
        (let ((bndslst (sfcdr (sfcar args)))
              (bodystmt (sfcdr args)))
          (putline "  ;; global let block")
          (cmpgbnds bndslst compenv)
          (cmpseq bodystmt compenv 0 0))))

      ;; --- 4. Special Forms & Native COND ---
      (cmpif (lambda (args compenv istail pararity) 
        (let ((cnd (sfcar args))
              (thn (sfcar (sfcdr args)))
              (els (sfcar (sfcdr (sfcdr args))))
              (id  (getid))) 
          (putline "  ;; if block") 
          (cmpexpr cnd compenv 0 pararity)
          (putline "  test rax, rax") 
          (putstr "  jz IF_ELS_") (putint id) (putchar 10) 
          (cmpexpr thn compenv istail pararity) 
          (putstr "  jmp IF_END_") (putint id) (putchar 10) 
          (putstr "IF_ELS_") (putint id) (putline ":") 
          (cmpexpr els compenv istail pararity) 
          (putstr "IF_END_") (putint id) (putline ":"))))

      (cmpcndc (lambda (clauses endid compenv istail pararity)
        (cond
          ((eql clauses 0)
           (putline "  mov rax, 0") (putstr "COND_END_")
           (putint endid) (putline ":"))
          (1 (let ((clause      (sfcar clauses))
                   (nextid     (getid))
                   (clauslst (sfcdr clause))
                   (cnd         (sfcar clauslst))
                   (body        (sfcdr clauslst)))
                   
               (cmpexpr cnd compenv 0 pararity)
               (putline "  test rax, rax")
               (putstr "  jz COND_NEXT_") (putint nextid) (putchar 10)
               
               (cmpseq body compenv istail pararity)
               
               (putstr "  jmp COND_END_") (putint endid) (putchar 10)
               (putstr "COND_NEXT_") (putint nextid) (putline ":")
               (cmpcndc (sfcdr clauses) endid compenv istail pararity))))))

      (cmpcond (lambda (args compenv istail pararity) 
        (let ((endid (getid))) 
          (putline "  ;; cond block") 
          (cmpcndc args endid compenv istail pararity))))
          
      (cmpseq (lambda (stmts compenv istail pararity) 
        (cond 
          ((eql stmts 0) 0) 
          (1 (let ((islast (eql (sfcdr stmts) 0))
                   (stmttail (cond (istail islast) (1 0))))
               (cmpexpr (sfcar stmts) compenv stmttail pararity) 
               (cmpseq (sfcdr stmts) compenv istail pararity))))))

      ;; --- 5. Function Definitions (Lambda & Apply) ---
      (cntbnds (lambda (bindings)
                 (cond (bindings (add 1 (cntbnds (sfcdr bindings)))) (1 0))))
      
      (bldpenv (lambda (params offset baseenv)
        (cond
          (params (cons (cons (sfcdr (sfcar params)) offset)
                        (bldpenv (sfcdr params) (add offset 8) baseenv)))
          (1 baseenv))))

      (cmplam (lambda (args compenv) 
        (let ((id (getid))) 
          (poke lams (cons (cons id args) (peek lams)))
          (putstr "  lea rax, [L_START_") (putint id) (putline "]"))))

      (alllams (lambda ()
        (let ((lambdas (peek lams)))
          (cond
            (lambdas (poke lams (sfcdr lambdas))
             (let ((entry (sfcar lambdas))
                   (id (sfcar entry))
                   (args (sfcdr entry))
                   (params (sfcdr (sfcar args)))
                   (bodystmt (sfcdr args)) 
                   (numparam (cntbnds params))
                   (lamenv (bldpenv params 16 0))) 
               (putstr "L_START_") (putint id) (putline ":") 
               (putline "  push rbp") 
               (putline "  mov rbp, rsp") 
               
               (cmpseq bodystmt lamenv 1 numparam) 
               
               (putline "  mov rsp, rbp") 
               (putline "  pop rbp") 
               (putstr "  ret ") (putint (mul 8 numparam)) (putchar 10) 
               
               (alllams)))
            (1 0)))))

      (cmpargs (lambda (args compenv pararity) 
        (cond 
          (args (cmpargs (sfcdr args) compenv pararity) 
                (cmpexpr (sfcar args) compenv 0 pararity) 
                (putline "  push rax")) 
          (1 0))))

      (popsys (lambda (n i)
        (cond ((eql i n) 0)
              (1 (putstr "  pop ")
                 (cond ((eql i 0) (putline "rax"))
                       ((eql i 1) (putline "rdi"))
                       ((eql i 2) (putline "rsi"))
                       ((eql i 3) (putline "rdx"))
                       ((eql i 4) (putline "r10"))
                       ((eql i 5) (putline "r8"))
                       ((eql i 6) (putline "r9")))
                 (popsys n (add i 1))))))

      (cmpsys (lambda (args compenv pararity)
        (let ((numargs (cntbnds args)))
          (cmpargs args compenv pararity)
          (putline "  ;; syscall")
          (popsys numargs 0)
          (putline "  syscall"))))

      ;; TCE Register Shuffling Helpers
      (poptce (lambda (n i)
        (cond ((gt i n) 0)
              (1 (putstr "  pop ") 
                 (cond ((eql i 1) (putline "r8"))
                       ((eql i 2) (putline "r9"))
                       ((eql i 3) (putline "r10"))
                       ((eql i 4) (putline "r11"))
                       ((eql i 5) (putline "r12"))
                       ((eql i 6) (putline "r13")))
                 (poptce n (add i 1))))))

      (pushtce (lambda (i)
        (cond ((eql i 0) 0)
              (1 (putstr "  push ")
                 (cond ((eql i 1) (putline "r8"))
                       ((eql i 2) (putline "r9"))
                       ((eql i 3) (putline "r10"))
                       ((eql i 4) (putline "r11"))
                       ((eql i 5) (putline "r12"))
                       ((eql i 6) (putline "r13")))
                 (pushtce (sub i 1))))))

      (cmpapp (lambda (funcnode args compenv istail pararity) 
        (let ((numargs (cntbnds args)))
          (cmpargs args compenv pararity) 
          (cmpexpr funcnode compenv 0 pararity)
          
          (cond
            (istail
             (putline "  ;; tail-call")
             (poptce numargs 1)
             (putline "  mov rsp, rbp")
             (putline "  pop rbp")
             (putline "  pop rcx")
             (putstr "  add rsp, ") (putint (mul 8 pararity)) (putline " ; Clean parent args")
             (pushtce numargs)
             (putline "  push rcx")
             (putline "  jmp rax"))
            (1
             (putline "  call rax"))))))

      ;; --- 6. Hardcoded Hardware Primitives ---
      
      ;; --- Primitive Category Checkers ---
      (isnul (lambda (s)
        (cond ((symeq s "getchar") 1)
              ((symeq s "getheap") 1)
              (1 0))))

      (isun (lambda (s)
        (cond ((symeq s "putint") 1)
              ((symeq s "puthex") 1)
              ((symeq s "putchar") 1)
              ((symeq s "putchunk") 1)
              ((symeq s "peek") 1)
              ((symeq s "alloc") 1)
              ((symeq s "setheap") 1)
              ((symeq s "car") 1)
              ((symeq s "cdr") 1)
              ((symeq s "not") 1)
              (1 0))))

      (isvarm (lambda (s)
        (cond ((symeq s "add") 1)
              ((symeq s "sub") 1)
              ((symeq s "mul") 1)
              ((symeq s "div") 1)
              ((symeq s "logior") 1)
              ((symeq s "or") 1)
              ((symeq s "logand") 1)
              ((symeq s "and") 1)
              (1 0))))

      (isbin (lambda (s)
        (cond ((symeq s "eql") 1)
              ((symeq s "lt") 1)
              ((symeq s "gt") 1)
              ((symeq s "le") 1)
              ((symeq s "ge") 1)
              ((symeq s "poke") 1)
              ((symeq s "pokebyte") 1)
              ((symeq s "peekidx") 1)
              ((symeq s "cons") 1)
              ((symeq s "ash") 1)
              (1 0))))

      (cmpvarl (lambda (symint args compenv pararity)
        (cond
          (args
           (let ((nextarg (sfcar args))
                 (tag2    (sfcar nextarg))
                 (val2    (sfcdr nextarg)))
             (cond
               ((eql tag2 0)
                (cond
                  ((symeq symint "ash")
                   (cond ((ge val2 0) (emitopt symint "_left_imm" val2))
                         (1 (emitopt symint "_right_imm" (sub 0 val2)))))
                  ((emitopt symint "_imm" val2) 1) 
                  (1 (cmpbin symint nextarg compenv pararity))))

               ((eql tag2 1)
                (let ((offset (lkenv val2 compenv)))
                  (cond
                    ((lt offset 0)
                     (cond ((emitopt symint "_local" (sub 0 offset)) 1)
                           (1 (cmpbin symint nextarg compenv pararity))))
                    ((gt offset 0)
                     (cond ((emitopt symint "_arg" offset) 1)
                           (1 (cmpbin symint nextarg compenv pararity))))
                    (1 (cmpbin symint nextarg compenv pararity)))))
               
               (1 (cmpbin symint nextarg compenv pararity))))
           (cmpvarl symint (sfcdr args) compenv pararity))
          (1 0))))
      
      (cmpvar (lambda (symint args compenv pararity)
        (cond
          (args
           (cmpexpr (sfcar args) compenv 0 pararity)
           (cmpvarl symint (sfcdr args) compenv pararity))
          (1 
           (cond
             ((symeq symint "mul") (putline "  mov rax, 1"))
             (1 (putline "  mov rax, 0")))))))

      (cmpnul (lambda (symint) 
        (putstr "  ;; inline ") (putsym symint) (putchar 10)
        (let ((asm (lksym (sym2str symint) (peek globprim))))
          (cond (asm (putstr asm)) (1 0)))))

      (cmpun (lambda (symint arg compenv pararity) 
        (cmpexpr arg compenv 0 pararity) 
        (putstr "  ;; inline ") (putsym symint) (putchar 10)
        (let ((asm (lksym (sym2str symint) (peek globprim))))
          (cond (asm (putstr asm)) (1 0)))))

      (cmptern (lambda (symint arg1 arg2 arg3 compenv pararity)
        (cmpexpr arg1 compenv 0 pararity)
        (putline "  push rax")
        (cmpexpr arg2 compenv 0 pararity)
        (putline "  push rax")
        (cmpexpr arg3 compenv 0 pararity)
        (putline "  pop rcx")
        (putline "  pop r8")
        (putstr "  ;; inline ") (putsym symint) (putchar 10)
        (let ((asm (lksym (sym2str symint) (peek globprim))))
          (cond (asm (putstr asm)) (1 0)))))

      (cmpbin (lambda (symint arg2 compenv pararity)
        (putline "  push rax")
        (cmpexpr arg2 compenv 0 pararity)
        (putline "  mov rcx, rax")
        (putline "  pop rax")
        (putstr "  ;; inline ") (putsym symint) (putchar 10)
        (cond 
          ((symeq symint "ash") 
           (let ((id (getid)))
             (putline "  test rcx, rcx")
             (putstr "  jns ASH_LEFT_") (putint id) (putchar 10)
             (putline "  neg rcx")
             (putline "  sar rax, cl")
             (putstr "  jmp ASH_DONE_") (putint id) (putchar 10)
             (putstr "ASH_LEFT_") (putint id) (putline ":")
             (putline "  shl rax, cl")
             (putstr "ASH_DONE_") (putint id) (putline ":")))
          (1 (let ((asm (lksym (sym2str symint) (peek globprim))))
               (cond (asm (putstr asm)) (1 0)))))))

      ;; --- c[ad]+r Dynamic Bitwise Traversal ---
      
      (iscadrc (lambda (symint)
        (let ((c (logand symint 255)))
          (cond
            ((eql c 0) 0)
            ((eql c 114) (eql (ash symint -8) 0)) ; 'r' must be the last char
            ((or (eql c 97) (eql c 100)) (iscadrc (ash symint -8)))
            (1 0)))))

      (iscadr (lambda (symint)
        (let ((c (logand symint 255)))
          (cond
            ((eql c 99) ; 'c'
             (let ((c1 (logand (ash symint -8) 255)))
               (cond ((or (eql c1 97) (eql c1 100))
                      (iscadrc (ash symint -8)))
                     (1 0))))
            (1 0)))))

      (emitcadr (lambda (symint)
        (let ((c (logand symint 255)))
          (cond
            ((eql c 114) 1) ; 'r' ends the traversal
            (1 
             ;; Recursive call pushes execution down the stack
             (emitcadr (ash symint -8))
             ;; Pop left-to-right maintaining execution order
             (cond ((eql c 97) (putline "  mov rax, [rax]"))
                   ((eql c 100) (putline "  mov rax, [rax+8]"))
                   (1 0)))))))

      (cmplist (lambda (astlist compenv istail pararity)
        (let ((funcnode (sfcar astlist)) (args (sfcdr astlist)))
          (cond
            ((eql (sfcar funcnode) 1)
             ;; Here, funcname is directly the 64-bit integer
             (let ((funcname (sfcdr funcnode))
                   (macrodef (lkmac funcname (peek cmpenv))))
               (cond
                 (macrodef
                  (let ((macparam (sfcar macrodef)) (macbody (sfcdr macrodef)))
                    (cmpexpr (evalast macbody (bndmac macparam args 0))
                             compenv istail pararity)))
                 (1
                  (cond
                    ((symeq funcname "defmacro")
                     (let ((macrosig (sfcdr (sfcar args)))
                           (macbody (sfcar (sfcdr args))))
                       (poke cmpenv (cons (cons (sfcdr (sfcar macrosig))
                                                (cons (sfcdr macrosig) macbody))
                                          (peek cmpenv)))))
                    ((symeq funcname "let") 
                     (cmplet args compenv istail pararity))
                    ((symeq funcname "lambda")
                     (cmplam args compenv))
                    ((symeq funcname "cond")
                     (cmpcond args compenv istail pararity))
                    ((symeq funcname "if")
                     (cmpif args compenv istail pararity))
                    ((eql (iscadr funcname) 1)
                     (cmpexpr (sfcar args) compenv 0 pararity)
                     (putline "  ;; inline c*r")
                     (emitcadr (ash funcname -8))) ; Shift off the initial 'c'
                    
                    ;; --- THE CLEAN DISPATCH ---
                    ((isnul funcname)
                     (cmpnul funcname))
                    ((isun funcname)
                     (cmpun funcname (sfcar args) compenv pararity))
                    ((isvarm funcname)
                     (cmpvar funcname args compenv pararity))
                    ((isbin funcname)
                     (cmpvar funcname (cons (sfcar args)
                                            (cons (sfcar (sfcdr args)) 0))
                             compenv pararity))
                    ((symeq funcname "pokeidx")
                     (cmptern funcname (sfcar args) (sfcar (sfcdr args))
                              (sfcar (sfcdr (sfcdr args))) compenv pararity))
                    ((symeq funcname "syscall") (cmpsys args compenv pararity))
                    (1 (cmpapp funcnode args compenv istail pararity)))))))
            (1 (cmpapp funcnode args compenv istail pararity))))))

      (cmpexpr (lambda (ast compenv istail pararity)
        (cond
          ((eql ast 0) 0)
          (1 (let ((tag (sfcar ast)) (val (sfcdr ast)))
               (cond
                 ((eql tag 0) (cmpnum val))
                 ((eql tag 1) 
                  (let ((offset (lkenv val compenv)))
                    (cond
                      ((eql offset 0) 
                       (putstr "  ;; global lookup") (putchar 10)
                       (emitglob val)
                       (putline "  mov rax, [rdi]"))
                      ((gt offset 0)
                       (putstr "  ;; arg lookup") (putchar 10)
                       (putstr "  mov rax, [rbp + ") (putint offset) (putline "]"))
                      (1
                       (putstr "  ;; local lookup") (putchar 10)
                       (putstr "  mov rax, [rbp - ") (putint (sub 0 offset)) (putline "]")))))
                 ((eql tag 2) (cmplist val compenv istail pararity))
                 ((eql tag 3) 
                  (let ((id (getid)))
                    (poke strtab (cons (cons id val) (peek strtab)))
                    (putline "  ;; string literal")
                    (putstr "  lea rax, [STR_") (putint id) (putline "]")))
                 (1 0)))))))

      (cmpprog (lambda (ast)
        (cond
          ((eql (sfcar ast) 2)
           (let ((val (sfcdr ast))
                 (funcnode (sfcar val)))
             (cond
               ((eql (sfcar funcnode) 1)
                (cond
                  ((symeq (sfcdr funcnode) "let")
                   (cmpglet (sfcdr val) 0))
                  (1 (cmpexpr ast 0 0 0))))
               (1 (cmpexpr ast 0 0 0)))))
          (1 (cmpexpr ast 0 0 0))))))

  ;; --- 7. THE MAIN ENTRY POINT ---
  (putline "format ELF64 executable 3")
  (putline "segment readable executable")
  (putline "include 'runtime.asm'")
  (putline "entry _start")
  (putline "_start:")
  (putline "  push rbp")
  (putline "  mov rbp, rsp")
  (putline "  lea r15, [heap_start]")
  

  ;; Execute parser safely within a protected stack frame
  (loadprim "primitives.asm")
  (cmpprog (prsexpr))
  
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
