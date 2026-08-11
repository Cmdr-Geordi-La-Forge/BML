(let ((lookahead (cons 0 0))
      (label-counter (cons 0 0))
      (compile-env (cons 0 0))
      (symbol-table (cons 0 0))
      (global-table (cons 0 0))
      (hoisted-lambdas (cons 0 0))

      ;; --- 1. I/O & Memory Safety ---
      (next-char (lambda () (let ((c (peek lookahead))) (cond (c (poke lookahead 0) c) (1 (read-char))))))
      (unget-char (lambda (c) (poke lookahead c)))

      (safe-car (lambda (l) (if (eql l 0) 0 (car l))))
      (safe-cdr (lambda (l) (if (eql l 0) 0 (cdr l))))

      (is-space (lambda (c) (or (eql c 32) (or (eql c 10) (or (eql c 13) (eql c 9))))))
      (is-digit (lambda (c) (and (ge c 48) (le c 57))))
      (is-delim (lambda (c) (or (is-space c) (or (eql c 0) (or (eql c 40) (eql c 41))))))

      (skip-comment (lambda () (let ((c (next-char))) (cond ((or (eql c 10) (eql c 0)) (skip-whitespace)) (1 (skip-comment))))))
      (skip-whitespace (lambda () (let ((c (next-char))) (cond ((is-space c) (skip-whitespace)) ((eql c 59) (skip-comment)) (1 (unget-char c))))))

      (print-string (lambda (s) (cond (s (print-chunk (safe-car s)) (print-string (safe-cdr s))) (1 0))))
      (print-line (lambda (s) (print-string s) (print-char 10)))
      (string-eq (lambda (s1 s2) (cond ((and (eql s1 0) (eql s2 0)) 1) ((or (eql s1 0) (eql s2 0)) 0) ((eql (safe-car s1) (safe-car s2)) (string-eq (safe-cdr s1) (safe-cdr s2))) (1 0))))
      (get-id (lambda () (let ((id (peek label-counter))) (poke label-counter (add id 1)) id)))

      ;; --- 2. The Lexer/Parser ---
      (parse-int (lambda (acc) (let ((c (next-char))) (cond ((is-digit c) (parse-int (add (mul acc 10) (sub c 48)))) (1 (unget-char c) acc)))))
      (parse-symbol-chunk (lambda (acc count) (let ((c (next-char))) (cond ((is-delim c) (unget-char c) acc) ((eql count 8) (unget-char c) acc) (1 (parse-symbol-chunk (logior acc (ash c (mul count 8))) (add count 1)))))))
      (parse-symbol (lambda () (let ((chunk (parse-symbol-chunk 0 0)) (next (peek lookahead))) (cond ((is-delim next) (cons chunk 0)) (1 (cons chunk (parse-symbol)))))))

      (parse-string-chunk (lambda (acc count) (let ((c (next-char))) (cond ((or (eql c 34) (eql c 0)) (unget-char c) acc) ((eql count 8) (unget-char c) acc) (1 (parse-string-chunk (logior acc (ash c (mul count 8))) (add count 1)))))))
      (parse-string (lambda () (let ((chunk (parse-string-chunk 0 0)) (next (peek lookahead))) (cond ((or (eql next 34) (eql next 0)) (cond ((eql next 34) (next-char)) (1 0)) (cons chunk 0)) (1 (cons chunk (parse-string)))))))

      (parse-list (lambda () (skip-whitespace) (let ((c (next-char))) (cond ((or (eql c 41) (eql c 0)) 0) (1 (unget-char c) (cons (parse-expr) (parse-list)))))))
      (parse-expr (lambda () (skip-whitespace) (let ((c (next-char))) (cond ((eql c 40) (cons 2 (parse-list))) ((eql c 34) (cons 3 (parse-string))) ((is-digit c) (unget-char c) (cons 0 (parse-int 0))) ((eql c 45) (cons 0 (sub 0 (parse-int 0)))) (1 (unget-char c) (cons 1 (parse-symbol)))))))

      ;; --- 2.5 Compile-Time Environment ---
      (lookup-symbol (lambda (str table) (cond ((eql table 0) 0) (1 (let ((entry (safe-car table))) (cond ((string-eq str (safe-car entry)) (safe-cdr entry)) (1 (lookup-symbol str (safe-cdr table)))))))))
      (lookup-macro (lambda (name env) (cond ((eql env 0) 0) (1 (let ((binding (safe-car env))) (cond ((string-eq name (safe-car binding)) (safe-cdr binding)) (1 (lookup-macro name (safe-cdr env)))))))))
      (bind-macro-args (lambda (params args env) (cond (params (cons (cons (safe-cdr (safe-car params)) (safe-car args)) (bind-macro-args (safe-cdr params) (safe-cdr args) env))) (1 env))))
      
      (lookup-env (lambda (name env)
        (cond
          ((eql env 0) 0) 
          (1 (let ((binding (safe-car env)))
               (cond
                 ((string-eq name (safe-car binding)) (safe-cdr binding)) 
                 (1 (lookup-env name (safe-cdr env)))))))))

      (eval-ast (lambda (ast env)
        (cond
          ((eql ast 0) 0)
          (1 (let ((tag (safe-car ast)) (val (safe-cdr ast)))
               (cond
                 ((eql tag 0) val)
                 ((eql tag 3) val)
                 ((eql tag 1) (lookup-macro val env))
                 ((eql tag 2)
                  (let ((func-name (safe-cdr (safe-car val))) (args (safe-cdr val)))
                    (cond
                      ((string-eq func-name "quote") (safe-car args))
                      ((string-eq func-name "car") (safe-car (eval-ast (safe-car args) env)))
                      ((string-eq func-name "cdr") (safe-cdr (eval-ast (safe-car args) env)))
                      ((string-eq func-name "cons") (cons (eval-ast (safe-car args) env) (eval-ast (safe-car (safe-cdr args)) env)))
                      ((string-eq func-name "eql") (cond ((eql (eval-ast (safe-car args) env) (eval-ast (safe-car (safe-cdr args)) env)) 1) (1 0)))
                      ((string-eq func-name "if") (cond ((eval-ast (safe-car args) env) (eval-ast (safe-car (safe-cdr args)) env)) (1 (eval-ast (safe-car (safe-cdr (safe-cdr args))) env))))
                      (1 0))))
                 (1 0)))))))

      ;; --- 3. The Code Generator ---
      (compile-number (lambda (val) (print-string "  mov rax, ") (print-int val) (print-char 10)))
      
      (emit-sym-chunks (lambda (s id chunk-id) (cond (s (print-string "SYM_") (print-int id) (print-string "_") (print-int chunk-id) (print-line ":") (print-string "  dq ") (print-int (safe-car s)) (print-char 10) (cond ((safe-cdr s) (print-string "  dq SYM_") (print-int id) (print-string "_") (print-int (add chunk-id 1)) (print-char 10) (emit-sym-chunks (safe-cdr s) id (add chunk-id 1))) (1 (print-line "  dq 0")))) (1 0))))
      
      (emit-inline-symbol (lambda (str-chunks)
        (let ((existing (lookup-symbol str-chunks (peek symbol-table))))
          (cond
            (existing (print-string "  lea rdi, [SYM_") (print-int (safe-car existing)) (print-line "_0]"))
            (1 (let ((id (get-id)))
                 (poke symbol-table (cons (cons str-chunks (cons id 0)) (peek symbol-table)))
                 (print-string "  lea rdi, [SYM_") (print-int id) (print-line "_0]")))))))

      (emit-global-var (lambda (str-chunks)
        (let ((existing (lookup-symbol str-chunks (peek global-table))))
          (cond
            (existing (print-string "  lea rdi, [VAL_") (print-int (safe-car existing)) (print-line "]"))
            (1 (let ((id (get-id)))
                 (poke global-table (cons (cons str-chunks (cons id 0)) (peek global-table)))
                 (print-string "  lea rdi, [VAL_") (print-int id) (print-line "]")))))))

      ;; --- DATA SECTION EMITTERS ---
      (emit-all-symbols (lambda (table)
        (cond
          (table
           (let ((entry (safe-car table)))
             (emit-sym-chunks (safe-car entry) (safe-car (safe-cdr entry)) 0)
             (emit-all-symbols (safe-cdr table))))
          (1 0))))

      (emit-all-globals (lambda (table)
        (cond
          (table
           (let ((entry (safe-car table)))
             (print-string "VAL_") (print-int (safe-car (safe-cdr entry))) (print-line ": dq 0")
             (emit-all-globals (safe-cdr table))))
          (1 0))))

      (count-locals (lambda (env)
        (cond
          ((eql env 0) 0)
          (1 (let ((val (safe-cdr (safe-car env))))
               (cond
                 ((lt val 0) (add 1 (count-locals (safe-cdr env))))
                 (1 (count-locals (safe-cdr env)))))))))

      (compile-binding (lambda (binding current-offset current-env parent-arity)
        (let ((binding-items (safe-cdr binding)))
          (let ((var-node (safe-car binding-items))
                (val-node (safe-car (safe-cdr binding-items))))
            ;; A let-binding value is NEVER in tail position
            (compile-expr val-node current-env 0 parent-arity)
            (print-line "  push rax")
            (cons (cons (safe-cdr var-node) current-offset) current-env)))))

      (compile-bindings (lambda (bindings current-offset current-env parent-arity)
        (cond
          (bindings
           (let ((new-env (compile-binding (safe-car bindings) current-offset current-env parent-arity)))
             (compile-bindings (safe-cdr bindings) (sub current-offset 8) new-env parent-arity)))
          (1 current-env))))

      (compile-let (lambda (args comp-env is-tail parent-arity)
        (let ((bindings-list (safe-cdr (safe-car args)))
              (body-stmts (safe-cdr args))
              (num-bindings (count-bindings bindings-list))
              (start-offset (mul -8 (add 1 (count-locals comp-env)))))
          (print-line "  ;; let block")
          (let ((new-env (compile-bindings bindings-list start-offset comp-env parent-arity)))
            ;; The let-body sequence inherits the tail position!
            (compile-sequence body-stmts new-env is-tail parent-arity)
            (print-string "  add rsp, ") (print-int (mul 8 num-bindings)) (print-line " ; Pop let bindings")))))

      ;; THE GLOBAL LET
      (compile-global-bindings (lambda (bindings comp-env)
        (cond
          (bindings
           (let ((binding-items (safe-cdr (safe-car bindings))))
             (let ((var-node (safe-car binding-items))
                   (val-node (safe-car (safe-cdr binding-items))))
               (compile-expr val-node comp-env 0 0)
               (print-line "  push rax")
               (emit-global-var (safe-cdr var-node))
               (print-line "  pop rax")
               (print-line "  mov [rdi], rax")
               (compile-global-bindings (safe-cdr bindings) comp-env))))
          (1 0))))

      (compile-global-let (lambda (args comp-env)
        (let ((bindings-list (safe-cdr (safe-car args)))
              (body-stmts (safe-cdr args)))
          (print-line "  ;; global let block")
          (compile-global-bindings bindings-list comp-env)
          (compile-sequence body-stmts comp-env 0 0))))

      ;; --- 4. Special Forms & Native COND ---
      (compile-if (lambda (args comp-env is-tail parent-arity) 
        (let ((cnd (safe-car args)) (thn (safe-car (safe-cdr args))) (els (safe-car (safe-cdr (safe-cdr args)))) (id (get-id))) 
          (print-line "  ;; if block") 
          (compile-expr cnd comp-env 0 parent-arity) ; Condition is NEVER in tail position
          (print-line "  test rax, rax") 
          (print-string "  jz IF_ELS_") (print-int id) (print-char 10) 
          (compile-expr thn comp-env is-tail parent-arity) 
          (print-string "  jmp IF_END_") (print-int id) (print-char 10) 
          (print-string "IF_ELS_") (print-int id) (print-line ":") 
          (compile-expr els comp-env is-tail parent-arity) 
          (print-string "IF_END_") (print-int id) (print-line ":"))))

      (compile-cond-clauses (lambda (clauses end-id comp-env is-tail parent-arity)
        (cond
          ((eql clauses 0) (print-line "  mov rax, 0") (print-string "COND_END_") (print-int end-id) (print-line ":"))
          (1 (let ((clause (safe-car clauses)) (next-id (get-id)))
               (let ((clause-items (safe-cdr clause)))
                 (compile-expr (safe-car clause-items) comp-env 0 parent-arity) ; Condition is NEVER tail
                 (print-line "  test rax, rax")
                 (print-string "  jz COND_NEXT_") (print-int next-id) (print-char 10)
                 (compile-sequence (safe-cdr clause-items) comp-env is-tail parent-arity)
                 (print-string "  jmp COND_END_") (print-int end-id) (print-char 10)
                 (print-string "COND_NEXT_") (print-int next-id) (print-line ":")
                 (compile-cond-clauses (safe-cdr clauses) end-id comp-env is-tail parent-arity)))))))

      (compile-cond (lambda (args comp-env is-tail parent-arity) 
        (let ((end-id (get-id))) 
          (print-line "  ;; cond block") 
          (compile-cond-clauses args end-id comp-env is-tail parent-arity))))
          
      (compile-sequence (lambda (stmts comp-env is-tail parent-arity) 
        (cond 
          ((eql stmts 0) 0) 
          (1 (let ((is-last (eql (safe-cdr stmts) 0)))
               (let ((stmt-tail (cond (is-tail is-last) (1 0))))
                 (compile-expr (safe-car stmts) comp-env stmt-tail parent-arity) 
                 (compile-sequence (safe-cdr stmts) comp-env is-tail parent-arity)))))))

      ;; --- 5. Function Definitions (Lambda & Apply) ---
      (count-bindings (lambda (bindings) (cond (bindings (add 1 (count-bindings (safe-cdr bindings)))) (1 0))))
      
      (build-param-env (lambda (params offset base-env)
        (cond
          (params (cons (cons (safe-cdr (safe-car params)) offset)
                        (build-param-env (safe-cdr params) (add offset 8) base-env)))
          (1 base-env))))

      (compile-lambda (lambda (args comp-env) 
        (let ((id (get-id))) 
          ;; Save (id . args) to our compile-time queue!
          (poke hoisted-lambdas (cons (cons id args) (peek hoisted-lambdas)))
          (print-string "  lea rax, [L_START_") (print-int id) (print-line "]"))))

      (emit-all-lambdas (lambda ()
        (let ((lambdas (peek hoisted-lambdas)))
          (cond
            (lambdas
             ;; Pop the current lambda off the queue
             (poke hoisted-lambdas (safe-cdr lambdas))
             (let ((entry (safe-car lambdas)))
               (let ((id (safe-car entry))
                     (args (safe-cdr entry)))
                 (let ((params (safe-cdr (safe-car args))) 
                       (body-stmts (safe-cdr args)) 
                       (num-params (count-bindings params))) 
                   (let ((lambda-env (build-param-env params 16 0))) 
                     (print-string "L_START_") (print-int id) (print-line ":") 
                     (print-line "  push rbp") 
                     (print-line "  mov rbp, rsp") 
                     
                     ;; Compile the body! (Tail = 1, Parent Arity = num-params)
                     (compile-sequence body-stmts lambda-env 1 num-params) 
                     
                     (print-line "  mov rsp, rbp") 
                     (print-line "  pop rbp") 
                     (print-string "  ret ") (print-int (mul 8 num-params)) (print-char 10) 
                     
                     (emit-all-lambdas))))))
            (1 0)))))

      (compile-args-reverse (lambda (args comp-env parent-arity) 
        (cond 
          (args (compile-args-reverse (safe-cdr args) comp-env parent-arity) 
                ;; Evaluated args are NEVER in tail position
                (compile-expr (safe-car args) comp-env 0 parent-arity) 
                (print-line "  push rax")) 
          (1 0))))

      ;; TCE Register Shuffling Helpers
      (pop-tce-regs (lambda (n i)
        (cond ((gt i n) 0)
              (1 (print-string "  pop ") 
                 (cond ((eql i 1) (print-line "r8"))
                       ((eql i 2) (print-line "r9"))
                       ((eql i 3) (print-line "r10"))
                       ((eql i 4) (print-line "r11"))
                       ((eql i 5) (print-line "r12"))
                       ((eql i 6) (print-line "r13")))
                 (pop-tce-regs n (add i 1))))))

      (push-tce-regs (lambda (i)
        (cond ((eql i 0) 0)
              (1 (print-string "  push ")
                 (cond ((eql i 1) (print-line "r8"))
                       ((eql i 2) (print-line "r9"))
                       ((eql i 3) (print-line "r10"))
                       ((eql i 4) (print-line "r11"))
                       ((eql i 5) (print-line "r12"))
                       ((eql i 6) (print-line "r13")))
                 (push-tce-regs (sub i 1))))))

      (compile-apply (lambda (func-node args comp-env is-tail parent-arity) 
        (let ((num-args (count-bindings args)))
          (compile-args-reverse args comp-env parent-arity) 
          (compile-expr func-node comp-env 0 parent-arity)
          
          (cond
            (is-tail
             (print-line "  ;; tail-call")
             (pop-tce-regs num-args 1)
             (print-line "  mov rsp, rbp")
             (print-line "  pop rbp")
             (print-line "  pop rcx")
             (print-string "  add rsp, ") (print-int (mul 8 parent-arity)) (print-line " ; Clean parent args")
             (push-tce-regs num-args)
             (print-line "  push rcx")
             (print-line "  jmp rax"))
            (1
             (print-line "  call rax"))))))

      ;; --- 6. Hardcoded Hardware Primitives ---
      (compile-primitive-nullary (lambda (op-name) 
        (print-string "  ;; inline ") (print-line op-name) 
        (cond ((string-eq op-name "read-char") (print-line "  call read_char")) (1 0))))
        
      (compile-primitive-unary (lambda (op-name arg comp-env parent-arity) 
        (compile-expr arg comp-env 0 parent-arity) 
        (print-string "  ;; inline ") (print-line op-name) 
        (cond ((string-eq op-name "print-int") (print-line "  call print_int")) 
              ((string-eq op-name "print-char") (print-line "  call print_char")) 
              ((string-eq op-name "print-chunk") (print-line "  call print_chunk")) 
              ((or (string-eq op-name "car") (string-eq op-name "peek")) (print-line "  mov rax, [rax]")) 
              ((string-eq op-name "cdr") (print-line "  mov rax, [rax+8]"))
              ((string-eq op-name "alloc") (print-line "  imul rax, 8") (print-line "  mov rcx, r15") (print-line "  add r15, rax") (print-line "  mov rax, rcx")) 
              (1 0))))
              
      (compile-primitive-binary (lambda (op-name arg1 arg2 comp-env parent-arity) 
        (compile-expr arg1 comp-env 0 parent-arity) 
        (print-line "  push rax") 
        (compile-expr arg2 comp-env 0 parent-arity) 
        (print-line "  pop rcx") 
        (print-string "  ;; inline ") (print-line op-name) 
        (cond ((string-eq op-name "add") (print-line "  add rax, rcx")) 
              ((string-eq op-name "sub") (print-line "  sub rcx, rax") (print-line "  mov rax, rcx")) 
              ((string-eq op-name "mul") (print-line "  imul rax, rcx")) 
              ((string-eq op-name "div") (print-line "  mov r8, rax") (print-line "  mov rax, rcx") (print-line "  cqo") (print-line "  idiv r8")) 
              ((string-eq op-name "eql") (print-line "  cmp rcx, rax") (print-line "  mov rax, 0") (print-line "  sete al")) 
              ((string-eq op-name "lt") (print-line "  cmp rcx, rax") (print-line "  mov rax, 0") (print-line "  setl al")) 
              ((string-eq op-name "gt") (print-line "  cmp rcx, rax") (print-line "  mov rax, 0") (print-line "  setg al")) 
              ((string-eq op-name "le") (print-line "  cmp rcx, rax") (print-line "  mov rax, 0") (print-line "  setle al")) 
              ((string-eq op-name "ge") (print-line "  cmp rcx, rax") (print-line "  mov rax, 0") (print-line "  setge al")) 
              ((string-eq op-name "ash") (print-line "  xchg rax, rcx") (print-line "  shl rax, cl")) 
              ((or (string-eq op-name "logior") (string-eq op-name "or")) (print-line "  or rax, rcx")) 
              ((string-eq op-name "and") (print-line "  and rax, rcx")) 
              ((string-eq op-name "poke") (print-line "  mov [rcx], rax")) 
              ((string-eq op-name "cons") (print-line "  mov [r15], rcx") (print-line "  mov [r15+8], rax") (print-line "  mov rax, r15") (print-line "  add r15, 16"))
              ((string-eq op-name "peek-idx") (print-line "  mov rax, [rcx + rax*8]")) 
              (1 0))))

      (compile-primitive-ternary (lambda (op-name arg1 arg2 arg3 comp-env parent-arity)
        (compile-expr arg1 comp-env 0 parent-arity)
        (print-line "  push rax")
        (compile-expr arg2 comp-env 0 parent-arity)
        (print-line "  push rax")
        (compile-expr arg3 comp-env 0 parent-arity)
        (print-line "  pop rcx")
        (print-line "  pop r8")
        (print-string "  ;; inline ") (print-line op-name)
        (cond ((string-eq op-name "poke-idx") (print-line "  mov [r8 + rcx*8], rax"))
              (1 0))))

      (compile-list (lambda (ast-list comp-env is-tail parent-arity)
        (let ((func-node (safe-car ast-list)) (args (safe-cdr ast-list)))
          (cond
            ((eql (safe-car func-node) 1)
             (let ((func-name (safe-cdr func-node)))
               (let ((macro-def (lookup-macro func-name (peek compile-env))))
                 (cond
                   (macro-def
                     (let ((macro-params (safe-car macro-def)) (macro-body (safe-cdr macro-def)))
                       (compile-expr (eval-ast macro-body (bind-macro-args macro-params args 0)) comp-env is-tail parent-arity)))
                   (1
                     (cond
                       ((string-eq func-name "defmacro")
                        (let ((macro-sig (safe-cdr (safe-car args))) (macro-body (safe-car (safe-cdr args))))
                          (poke compile-env (cons (cons (safe-cdr (safe-car macro-sig)) (cons (safe-cdr macro-sig) macro-body)) (peek compile-env)))))
                       ((string-eq func-name "let") 
                        (compile-let args comp-env is-tail parent-arity))
                       ((string-eq func-name "lambda") (compile-lambda args comp-env))
                       ((string-eq func-name "cond") (compile-cond args comp-env is-tail parent-arity))
                       ((string-eq func-name "if") (compile-if args comp-env is-tail parent-arity))
                       ((string-eq func-name "read-char") (compile-primitive-nullary func-name))
                       ((or (string-eq func-name "print-int") (or (string-eq func-name "print-char") (or (string-eq func-name "print-chunk") (or (string-eq func-name "car") (or (string-eq func-name "cdr") (or (string-eq func-name "peek") (string-eq func-name "alloc")))))))
                        (compile-primitive-unary func-name (safe-car args) comp-env parent-arity))
                       ((or (string-eq func-name "add") (or (string-eq func-name "sub") (or (string-eq func-name "mul") (or (string-eq func-name "div") (or (string-eq func-name "eql") (or (string-eq func-name "lt") (or (string-eq func-name "gt") (or (string-eq func-name "le") (or (string-eq func-name "ge") (or (string-eq func-name "ash") (or (string-eq func-name "logior") (or (string-eq func-name "or") (or (string-eq func-name "and") (or (string-eq func-name "poke") (or (string-eq func-name "peek-idx") (string-eq func-name "cons"))))))))))))))))
                        (compile-primitive-binary func-name (safe-car args) (safe-car (safe-cdr args)) comp-env parent-arity))
                       ((string-eq func-name "poke-idx")
                        (compile-primitive-ternary func-name (safe-car args) (safe-car (safe-cdr args)) (safe-car (safe-cdr (safe-cdr args))) comp-env parent-arity))
                       (1 (compile-apply func-node args comp-env is-tail parent-arity))))))))
            (1 (compile-apply func-node args comp-env is-tail parent-arity))))))

      (compile-expr (lambda (ast comp-env is-tail parent-arity)
        (cond
          ((eql ast 0) 0)
          (1 (let ((tag (safe-car ast)) (val (safe-cdr ast)))
               (cond
                 ((eql tag 0) (compile-number val))
                 ((eql tag 1) 
                  (let ((offset (lookup-env val comp-env)))
                    (cond
                      ((eql offset 0) 
                       (print-string "  ;; global lookup") (print-char 10)
                       (emit-global-var val)
                       (print-line "  mov rax, [rdi]"))
                      ((gt offset 0)
                       (print-string "  ;; arg lookup") (print-char 10)
                       (print-string "  mov rax, [rbp + ") (print-int offset) (print-line "]"))
                      (1
                       (print-string "  ;; local lookup") (print-char 10)
                       (print-string "  mov rax, [rbp - ") (print-int (sub 0 offset)) (print-line "]")))))
                 ((eql tag 2) (compile-list val comp-env is-tail parent-arity))
                 ((eql tag 3) (print-line "  ;; string") (emit-inline-symbol val) (print-line "  mov rax, rdi"))
                 (1 0)))))))

      ;; NEW FIX: Encapsulate evaluation so rbp is initialized safely!
      (compile-program (lambda (ast)
        (cond
          ((eql (safe-car ast) 2)
           (let ((val (safe-cdr ast)))
             (let ((func-node (safe-car val)))
               (cond
                 ((eql (safe-car func-node) 1)
                  (cond
                    ((string-eq (safe-cdr func-node) "let")
                     (compile-global-let (safe-cdr val) 0))
                    (1 (compile-expr ast 0 0 0))))
                 (1 (compile-expr ast 0 0 0))))))
          (1 (compile-expr ast 0 0 0))))))

  ;; --- 7. THE MAIN ENTRY POINT ---
  (print-line "format ELF64 executable 3")
  (print-line "segment readable executable")
  (print-line "include 'runtime.asm'")
  (print-line "entry _start")
  (print-line "_start:")
  (print-line "  lea r15, [heap_start]")
  
  ;; Execute parser safely within a protected stack frame
  (compile-program (parse-expr)) 
  
  (print-line "  mov rdi, rax")
  (print-line "  mov rax, 60")
  (print-line "  syscall")

  (print-line "  ;; --- COMPILED FUNCTIONS ---")
  (emit-all-lambdas)

  (print-line "segment readable writeable")
  (emit-all-symbols (peek symbol-table))
  (emit-all-globals (peek global-table))
  (print-line "heap_start: rb 1024 * 1024 * 8"))
