(let ((lookahead (cons 0 0))
      (label-counter (cons 0 0))
      (compile-env (cons 0 0))
      (symbol-table (cons 0 0))
      (global-table (cons 0 0))
      (string-table (cons 0 0))
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

      (print-string (lambda (s) (syscall 1 1 (safe-car s) (safe-cdr s))))
      (print-line (lambda (s) (print-string s) (print-char 10)))
      ;; --- Replaced String Comparison ---
      (string-eq-byte-loop (lambda (ptr1 ptr2 len idx)
        (cond 
          ;; If we reached the length without failing, they match!
          ((ge idx len) 1)
          (1 (let ((chunk-idx (div idx 8))
                   (char-idx (sub idx (mul chunk-idx 8))))
               ;; Fetch the 8-byte chunk for both strings
               (let ((chunk1 (peek (add ptr1 (mul chunk-idx 8))))
                     (chunk2 (peek (add ptr2 (mul chunk-idx 8)))))
                 ;; Extract the exact byte we care about
                 (let ((c1 (get-char-byte chunk1 char-idx))
                       (c2 (get-char-byte chunk2 char-idx)))
                   (cond 
                     ((eql c1 c2) (string-eq-byte-loop ptr1 ptr2 len (add idx 1)))
                     (1 0)))))))))

      (string-eq (lambda (s1 s2)
        (cond ((and (eql s1 0) (eql s2 0)) 1)
              ((or (eql s1 0) (eql s2 0)) 0)
              ((eql (safe-cdr s1) (safe-cdr s2)) 
               ;; Start the byte-by-byte comparison at index 0
               (string-eq-byte-loop (safe-car s1) (safe-car s2) (safe-cdr s1) 0))
              (1 0))))
      (get-id (lambda () (let ((id (peek label-counter))) (poke label-counter (add id 1)) id)))

      ;; --- 2. The Lexer/Parser ---

      (parse-int (lambda (acc) (let ((c (next-char))) (cond ((is-digit c) (parse-int (add (mul acc 10) (sub c 48)))) (1 (unget-char c) acc)))))

      (parse-symbol-loop (lambda (ptr count)
        (let ((c (next-char)))
          (cond
            ((is-delim c)
             (unget-char c)
             (let ((aligned-count (mul (div (add count 8) 8) 8)))
               (pad-zeros ptr count aligned-count)
               (alloc (div aligned-count 8))
               (cons ptr count)))
            (1
             (poke-byte (add ptr count) c)
             (parse-symbol-loop ptr (add count 1)))))))

      (parse-symbol (lambda () 
        (parse-symbol-loop (get-heap) 0)))
      
      (pad-zeros (lambda (ptr count target)
        (cond ((lt count target)
               (poke-byte (add ptr count) 0)
               (pad-zeros ptr (add count 1) target))
              (1 0))))

      (parse-string-loop (lambda (ptr count)
        (let ((c (next-char)))
          (cond
            ((or (eql c 34) (eql c 0))
             ;; Find the 8-byte boundary
             (let ((aligned-count (mul (div (add count 8) 8) 8)))
               (pad-zeros ptr count aligned-count)
               (alloc (div aligned-count 8))
               (cons ptr count)))
            (1
             (poke-byte (add ptr count) c)
             (parse-string-loop ptr (add count 1)))))))

      (parse-string (lambda () 
        ;; Get the current heap pointer and start writing at offset 0
        (parse-string-loop (get-heap) 0)))
      
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
                  ;; Extract the raw string from the symbol node
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
      
      (emit-contiguous-loop (lambda (ptr len)
        (cond
          ((gt len 0)
           (print-int (logand (peek ptr) 255))
           (cond ((gt len 1) (print-string ", ")) (1 0))
           (emit-contiguous-loop (add ptr 1) (sub len 1)))
          (1 0))))

      (emit-contiguous-data (lambda (prefix id val)
        (print-line "align 8")
        (print-string prefix) (print-int id) (print-line ":")
        (print-string "  dq ") (print-string prefix) (print-int id) (print-line "_data")
        (print-string "  dq ") (print-int (safe-cdr val)) (print-char 10)
        (print-string prefix) (print-int id) (print-line "_data:")
        (cond ((gt (safe-cdr val) 0)
               (print-string "  db ")
               (emit-contiguous-loop (safe-car val) (safe-cdr val))
               (print-char 10))
              (1 0))
        (print-line "  align 8")))

      (emit-all-symbols (lambda (table)
        (cond
          (table
           (let ((entry (safe-car table)))
             (let ((sym (safe-car entry))
                   (id (safe-car (safe-cdr entry))))
               (emit-contiguous-data "SYM_" id sym)
               (emit-all-symbols (safe-cdr table)))))
          (1 0))))

      (emit-all-strings (lambda (table)
        (cond
          (table
           (let ((entry (safe-car table)))
             (let ((id (safe-car entry))
                   (val (safe-cdr entry)))
               (emit-contiguous-data "STR_" id val)
               (emit-all-strings (safe-cdr table)))))
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
          (1 (let ((clause      (safe-car clauses))
                   (next-id     (get-id))
                   (clause-list (safe-cdr clause))       ; 1. Peel off the AST tag 2
                   (cnd         (safe-car clause-list))  ; 2. Extract the condition
                   (body        (safe-cdr clause-list))) ; 3. Extract the body statements
                   
               (compile-expr cnd comp-env 0 parent-arity)
               (print-line "  test rax, rax")
               (print-string "  jz COND_NEXT_") (print-int next-id) (print-char 10)
               
               (compile-sequence body comp-env is-tail parent-arity)
               
               (print-string "  jmp COND_END_") (print-int end-id) (print-char 10)
               (print-string "COND_NEXT_") (print-int next-id) (print-line ":")
               (compile-cond-clauses (safe-cdr clauses) end-id comp-env is-tail parent-arity))))))

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

      (pop-syscall-regs (lambda (n i)
        (cond ((eql i n) 0)
              (1 (print-string "  pop ")
                 (cond ((eql i 0) (print-line "rax"))
                       ((eql i 1) (print-line "rdi"))
                       ((eql i 2) (print-line "rsi"))
                       ((eql i 3) (print-line "rdx"))
                       ((eql i 4) (print-line "r10"))
                       ((eql i 5) (print-line "r8"))
                       ((eql i 6) (print-line "r9")))
                 (pop-syscall-regs n (add i 1))))))

      (compile-syscall (lambda (args comp-env parent-arity)
        (let ((num-args (count-bindings args)))
          (compile-args-reverse args comp-env parent-arity)
          (print-line "  ;; syscall")
          (pop-syscall-regs num-args 0)
          (print-line "  syscall"))))

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
      ;; --- Primitive Category Checkers ---
      (is-nullary (lambda (s)
        (cond ((string-eq s "read-char") 1)
              ((string-eq s "get-heap") 1)
              (1 0))))

      (is-unary (lambda (s)
        (cond ((string-eq s "print-int") 1)
              ((string-eq s "print-char") 1)
              ((string-eq s "print-chunk") 1)
              ((string-eq s "peek") 1)
              ((string-eq s "alloc") 1)
              ((string-eq s "set-heap") 1)
              ((string-eq s "car") 1)
              ((string-eq s "cdr") 1)
              (1 0))))

      (is-variadic-math (lambda (s)
        (cond ((string-eq s "add") 1)
              ((string-eq s "sub") 1)
              ((string-eq s "mul") 1)
              ((string-eq s "div") 1)
              ((string-eq s "logior") 1)
              ((string-eq s "or") 1)
              ((string-eq s "logand") 1)
              ((string-eq s "and") 1)
              (1 0))))

      (is-strict-binary (lambda (s)
        (cond ((string-eq s "eql") 1)
              ((string-eq s "lt") 1)
              ((string-eq s "gt") 1)
              ((string-eq s "le") 1)
              ((string-eq s "ge") 1)
              ((string-eq s "poke") 1)
              ((string-eq s "poke-byte") 1)
              ((string-eq s "peek-idx") 1)
              ((string-eq s "cons") 1)
              ((string-eq s "ash") 1) ; ash is now handled generically here!
              (1 0))))
      
      (compile-primitive-nullary (lambda (op-name) 
        (print-string "  ;; inline ") (print-string op-name) (print-char 10)
        (cond ((string-eq op-name "read-char") (print-line "  call read_char"))
              ((string-eq op-name "get-heap")  (print-line "  mov rax, r15"))
              (1 0))))

      (compile-primitive-unary (lambda (op-name arg comp-env parent-arity) 
        (compile-expr arg comp-env 0 parent-arity) 
        (print-string "  ;; inline ") (print-string op-name) (print-char 10)
        (cond ((string-eq op-name "print-int") (print-line "  call print_int")) 
              ((string-eq op-name "print-char") (print-line "  call print_char")) 
              ((string-eq op-name "print-chunk") (print-line "  call print_chunk")) 
              ((or (string-eq op-name "car") (string-eq op-name "peek")) (print-line "  mov rax, [rax]")) 
              ((string-eq op-name "cdr") (print-line "  mov rax, [rax+8]"))
              ((string-eq op-name "alloc") (print-line "  imul rax, 8") (print-line "  mov rcx, r15") (print-line "  add r15, rax") (print-line "  mov rax, rcx"))
              ((string-eq op-name "set-heap") (print-line "  mov r15, rax"))
              (1 0))))

      (compile-primitive-ternary (lambda (op-name arg1 arg2 arg3 comp-env parent-arity)
        (compile-expr arg1 comp-env 0 parent-arity)
        (print-line "  push rax")
        (compile-expr arg2 comp-env 0 parent-arity)
        (print-line "  push rax")
        (compile-expr arg3 comp-env 0 parent-arity)
        (print-line "  pop rcx")
        (print-line "  pop r8")
        (print-string "  ;; inline ") (print-string op-name) (print-char 10)
        (cond ((string-eq op-name "poke-idx") (print-line "  mov [r8 + rcx*8], rax"))
              (1 0))))

      (compile-binary-fallback (lambda (op-name arg2 comp-env parent-arity)
        (print-line "  push rax")
        (compile-expr arg2 comp-env 0 parent-arity)
        (print-line "  mov rcx, rax")
        (print-line "  pop rax")
        (print-string "  ;; inline ") (print-string op-name) (print-char 10)
        (cond 
          ((string-eq op-name "add") (print-line "  add rax, rcx"))
          ((string-eq op-name "sub") (print-line "  sub rax, rcx"))
          ((string-eq op-name "mul") (print-line "  imul rax, rcx"))
          ((string-eq op-name "div") (print-line "  mov r8, rcx") (print-line "  cqo") (print-line "  idiv r8"))
          ((string-eq op-name "eql") (print-line "  cmp rax, rcx") (print-line "  mov rax, 0") (print-line "  sete al"))
          ((string-eq op-name "lt")  (print-line "  cmp rax, rcx") (print-line "  mov rax, 0") (print-line "  setl al"))
          ((string-eq op-name "gt")  (print-line "  cmp rax, rcx") (print-line "  mov rax, 0") (print-line "  setg al"))
          ((string-eq op-name "le")  (print-line "  cmp rax, rcx") (print-line "  mov rax, 0") (print-line "  setle al"))
          ((string-eq op-name "ge")  (print-line "  cmp rax, rcx") (print-line "  mov rax, 0") (print-line "  setge al"))
          ((string-eq op-name "ash") 
           (let ((id (get-id)))
             (print-line "  test rcx, rcx")
             (print-string "  jns ASH_LEFT_") (print-int id) (print-char 10)
             (print-line "  neg rcx")
             (print-line "  sar rax, cl")
             (print-string "  jmp ASH_DONE_") (print-int id) (print-char 10)
             (print-string "ASH_LEFT_") (print-int id) (print-line ":")
             (print-line "  shl rax, cl")
             (print-string "ASH_DONE_") (print-int id) (print-line ":")))
          ((or (string-eq op-name "logior") (string-eq op-name "or")) (print-line "  or rax, rcx"))
          ((or (string-eq op-name "logand") (string-eq op-name "and")) (print-line "  and rax, rcx"))
          ((string-eq op-name "poke") (print-line "  mov [rax], rcx"))
          ((string-eq op-name "poke-byte") (print-line "  mov [rax], cl"))
          ((string-eq op-name "cons") (print-line "  mov [r15], rax") (print-line "  mov [r15+8], rcx") (print-line "  mov rax, r15") (print-line "  add r15, 16"))
          ((string-eq op-name "peek-idx") (print-line "  mov rax, [rax + rcx*8]"))
          (1 0))))

      (compile-variadic-loop (lambda (op-name args comp-env parent-arity)
        (cond
          (args
           (let ((next-arg (safe-car args)))
             (let ((tag2 (safe-car next-arg))
                   (val2 (safe-cdr next-arg)))
               
               (cond
                 ;; --- OPTIMIZATION 1: Arg is an Integer (Tag 0) ---
                 ((eql tag2 0)
                  (cond
                    ((string-eq op-name "add") (print-string "  add rax, ") (print-int val2) (print-char 10))
                    ((string-eq op-name "sub") (print-string "  sub rax, ") (print-int val2) (print-char 10))
                    ((string-eq op-name "mul") (print-string "  imul rax, ") (print-int val2) (print-char 10))
                    ((or (string-eq op-name "logior") (string-eq op-name "or")) (print-string "  or rax, ") (print-int val2) (print-char 10))
                    ((or (string-eq op-name "logand") (string-eq op-name "and")) (print-string "  and rax, ") (print-int val2) (print-char 10))
                    ((string-eq op-name "ash")
                     (cond
                       ((ge val2 0) (print-string "  shl rax, ") (print-int val2) (print-char 10))
                       (1 (print-string "  sar rax, ") (print-int (sub 0 val2)) (print-char 10))))
                    (1 (compile-binary-fallback op-name next-arg comp-env parent-arity))))

                 ;; --- OPTIMIZATION 2: Arg is a Symbol (Tag 1) ---
                 ((eql tag2 1)
                  (let ((offset (lookup-env val2 comp-env)))
                    (cond
                      ((lt offset 0) ; Local variable
                       (let ((abs-offset (sub 0 offset)))
                         (cond
                           ((string-eq op-name "add") (print-string "  add rax, [rbp - ") (print-int abs-offset) (print-line "]"))
                           ((string-eq op-name "sub") (print-string "  sub rax, [rbp - ") (print-int abs-offset) (print-line "]"))
                           ((string-eq op-name "mul") (print-string "  imul rax, [rbp - ") (print-int abs-offset) (print-line "]"))
                           ((or (string-eq op-name "logior") (string-eq op-name "or")) (print-string "  or rax, [rbp - ") (print-int abs-offset) (print-line "]"))
                           ((or (string-eq op-name "logand") (string-eq op-name "and")) (print-string "  and rax, [rbp - ") (print-int abs-offset) (print-line "]"))
                           (1 (compile-binary-fallback op-name next-arg comp-env parent-arity)))))
                      ((gt offset 0) ; Function argument
                       (cond
                         ((string-eq op-name "add") (print-string "  add rax, [rbp + ") (print-int offset) (print-line "]"))
                         ((string-eq op-name "sub") (print-string "  sub rax, [rbp + ") (print-int offset) (print-line "]"))
                         ((string-eq op-name "mul") (print-string "  imul rax, [rbp + ") (print-int offset) (print-line "]"))
                         ((or (string-eq op-name "logior") (string-eq op-name "or")) (print-string "  or rax, [rbp + ") (print-int offset) (print-line "]"))
                         ((or (string-eq op-name "logand") (string-eq op-name "and")) (print-string "  and rax, [rbp + ") (print-int offset) (print-line "]"))
                         (1 (compile-binary-fallback op-name next-arg comp-env parent-arity))))
                      (1 (compile-binary-fallback op-name next-arg comp-env parent-arity))))) ; Global variable

                 ;; --- FALLBACK: Complex Expressions ---
                 (1 (compile-binary-fallback op-name next-arg comp-env parent-arity)))))
             
           (compile-variadic-loop op-name (safe-cdr args) comp-env parent-arity))
          (1 0))))

      (compile-primitive-variadic (lambda (op-name args comp-env parent-arity)
        (cond
          (args
           (compile-expr (safe-car args) comp-env 0 parent-arity)
           (compile-variadic-loop op-name (safe-cdr args) comp-env parent-arity))
          (1 
           (cond
             ((string-eq op-name "mul") (print-line "  mov rax, 1"))
             (1 (print-line "  mov rax, 0")))))))

      ;; --- c[ad]+r Dynamic Chaining Helpers ---
      (get-chunk (lambda (lst idx)
        (cond ((eql lst 0) 0)
              ((eql idx 0) (safe-car lst))
              (1 (get-chunk (safe-cdr lst) (sub idx 1))))))

      (get-char-byte (lambda (chunk char-idx)
        (logand (ash chunk (mul char-idx -8)) 255)))

      (is-cadr-chars (lambda (str byte-idx)
        (cond
          ;; Out of bounds check
          ((ge byte-idx (safe-cdr str)) 0)
          (1 (let ((chunk-idx (div byte-idx 8))
                   (char-idx (sub byte-idx (mul chunk-idx 8))))
               ;; Dereference the specific 8-byte chunk from the heap
               (let ((chunk (peek (add (safe-car str) (mul chunk-idx 8)))))
                 (let ((c (get-char-byte chunk char-idx)))
                   (cond
                     ((eql c 114) ; 'r'
                      ;; Must be the last character in the string
                      (cond ((eql byte-idx (sub (safe-cdr str) 1)) 1) (1 0)))
                     ((or (eql c 97) (eql c 100)) ; 'a' or 'd'
                      (is-cadr-chars str (add byte-idx 1)))
                     (1 0)))))))))

      (is-cadr-form (lambda (str)
        (cond
          ((le (safe-cdr str) 0) 0)
          (1 (let ((chunk (peek (safe-car str)))) ; Dereference the first 8 bytes
               (let ((c0 (get-char-byte chunk 0))
                     (c1 (get-char-byte chunk 1)))
                 (cond
                   ;; Must start with 'c', followed by at least one 'a' or 'd'
                   ((and (eql c0 99) (or (eql c1 97) (eql c1 100)))
                    (is-cadr-chars str 1))
                   (1 0))))))))

      (emit-cadr-chars (lambda (str byte-idx)
        (let ((chunk-idx (div byte-idx 8))
              (char-idx (sub byte-idx (mul chunk-idx 8))))
          ;; Dereference the specific 8-byte chunk from the heap
          (let ((chunk (peek (add (safe-car str) (mul chunk-idx 8)))))
            (let ((c (get-char-byte chunk char-idx)))
              (cond
                ((eql c 114) 1)
                (1 
                 ;; Recursive call pushes down the stack
                 (emit-cadr-chars str (add byte-idx 1))
                 ;; Pop left-to-right to maintain execution order
                 (cond ((eql c 97) (print-line "  mov rax, [rax]"))
                       (1 (print-line "  mov rax, [rax+8]"))))))))))

      (compile-list (lambda (ast-list comp-env is-tail parent-arity)
        (let ((func-node (safe-car ast-list)) (args (safe-cdr ast-list)))
          (cond
            ;; Check if the function node is a SYMBOL (tag 1)
            ((eql (safe-car func-node) 1)
             ;; Peel off the tag 1 to get the actual (ptr . len) string
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
                       ((eql (is-cadr-form func-name) 1)
                        (compile-expr (safe-car args) comp-env 0 parent-arity)
                        (print-line "  ;; inline c*r")
                        (emit-cadr-chars func-name 1))
                       
                       ;; --- THE CLEAN DISPATCH ---
                       ((is-nullary func-name)
                        (compile-primitive-nullary func-name))
                       ((is-unary func-name)
                        (compile-primitive-unary func-name (safe-car args) comp-env parent-arity))
                       ((is-variadic-math func-name)
                        (compile-primitive-variadic func-name args comp-env parent-arity))
                       ((is-strict-binary func-name)
                        ;; Ensure strict binary ops only ever get 2 arguments fed to the variadic compiler
                        (compile-primitive-variadic func-name (cons (safe-car args) (cons (safe-car (safe-cdr args)) 0)) comp-env parent-arity))
                       ((string-eq func-name "poke-idx")
                        (compile-primitive-ternary func-name (safe-car args) (safe-car (safe-cdr args)) (safe-car (safe-cdr (safe-cdr args))) comp-env parent-arity))
                       ((string-eq func-name "syscall") (compile-syscall args comp-env parent-arity))
                       ;; If no primitives/macros match, compile it as a standard user function call!
                       (1 (compile-apply func-node args comp-env is-tail parent-arity))))))))
            ;; If func-node is NOT a symbol (e.g. ((lambda (...) ...)) ), compile as standard call
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
                 ((eql tag 3) 
                  (let ((id (get-id)))
                    (poke string-table (cons (cons id val) (peek string-table)))
                    (print-line "  ;; string literal")
                    (print-string "  lea rax, [STR_") (print-int id) (print-line "]")))
                 (1 0)))))))

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
  (print-line "  push rbp")
  (print-line "  mov rbp, rsp")
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
  (emit-all-strings (peek string-table))
  (print-line "heap_start: rb 1024 * 1024 * 8"))
