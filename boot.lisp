(let ((lookahead (cons 0 0))
      (label-counter (cons 0 0))
      (compile-env (cons 0 0))
      (symbol-table (cons 0 0)))
  (let (
        ;; --- 1. I/O & Memory Safety ---
        (next-char (lambda () (let ((c (peek lookahead))) (if c (begin (poke lookahead 0) c) (read-char)))))
        (unget-char (lambda (c) (poke lookahead c)))

        (safe-car (lambda (l) (if (eql l 0) 0 (car l))))
        (safe-cdr (lambda (l) (if (eql l 0) 0 (cdr l))))

        (is-space (lambda (c) (or (eql c 32) (or (eql c 10) (or (eql c 13) (eql c 9))))))
        (is-digit (lambda (c) (and (ge c 48) (le c 57))))
        (is-delim (lambda (c) (or (is-space c) (or (eql c 0) (or (eql c 40) (eql c 41))))))

        (skip-comment (lambda () (let ((c (next-char))) (if (or (eql c 10) (eql c 0)) (skip-whitespace) (skip-comment)))))
        (skip-whitespace (lambda () (let ((c (next-char))) (cond ((is-space c) (skip-whitespace)) ((eql c 59) (skip-comment)) (1 (unget-char c))))))

        (print-string (lambda (s) (if s (begin (print-chunk (safe-car s)) (print-string (safe-cdr s))) 0)))
        (print-line (lambda (s) (begin (print-string s) (print-char 10))))
        (string-eq (lambda (s1 s2) (cond ((and (eql s1 0) (eql s2 0)) 1) ((or (eql s1 0) (eql s2 0)) 0) ((eql (safe-car s1) (safe-car s2)) (string-eq (safe-cdr s1) (safe-cdr s2))) (1 0))))
        (get-id (lambda () (let ((id (peek label-counter))) (begin (poke label-counter (add id 1)) id))))

        ;; --- 2. The Lexer/Parser ---
        (parse-int (lambda (acc) (let ((c (next-char))) (if (is-digit c) (parse-int (add (mul acc 10) (sub c 48))) (begin (unget-char c) acc)))))
        (parse-symbol-chunk (lambda (acc count) (let ((c (next-char))) (cond ((is-delim c) (begin (unget-char c) acc)) ((eql count 8) (begin (unget-char c) acc)) (1 (parse-symbol-chunk (logior acc (ash c (mul count 8))) (add count 1)))))))
        (parse-symbol (lambda () (let ((chunk (parse-symbol-chunk 0 0))) (let ((next (peek lookahead))) (if (is-delim next) (cons chunk 0) (cons chunk (parse-symbol)))))))

        (parse-string-chunk (lambda (acc count) (let ((c (next-char))) (cond ((or (eql c 34) (eql c 0)) (begin (unget-char c) acc)) ((eql count 8) (begin (unget-char c) acc)) (1 (parse-string-chunk (logior acc (ash c (mul count 8))) (add count 1)))))))
        (parse-string (lambda () (let ((chunk (parse-string-chunk 0 0))) (let ((next (peek lookahead))) (if (or (eql next 34) (eql next 0)) (begin (if (eql next 34) (next-char) 0) (cons chunk 0)) (cons chunk (parse-string)))))))

        (parse-list (lambda () (begin (skip-whitespace) (let ((c (next-char))) (if (or (eql c 41) (eql c 0)) 0 (begin (unget-char c) (cons (parse-expr) (parse-list))))))))
        (parse-expr (lambda () (begin (skip-whitespace) (let ((c (next-char))) (cond ((eql c 40) (cons 2 (parse-list))) ((eql c 34) (cons 3 (parse-string))) ((is-digit c) (begin (unget-char c) (cons 0 (parse-int 0)))) (1 (begin (unget-char c) (cons 1 (parse-symbol)))))))))

        ;; --- 2.5 Compile-Time Macro Evaluator & Interning ---
        (lookup-symbol (lambda (str table) (if (eql table 0) 0 (let ((entry (safe-car table))) (if (string-eq str (safe-car entry)) (safe-cdr entry) (lookup-symbol str (safe-cdr table)))))))
        (lookup-macro (lambda (name env) (if (eql env 0) 0 (let ((binding (safe-car env))) (if (string-eq name (safe-car binding)) (safe-cdr binding) (lookup-macro name (safe-cdr env)))))))
        (bind-macro-args (lambda (params args env) (if params (cons (cons (safe-cdr (safe-car params)) (safe-car args)) (bind-macro-args (safe-cdr params) (safe-cdr args) env)) env)))

        (eval-ast (lambda (ast env)
          (if (eql ast 0) 0
              (let ((tag (safe-car ast)) (val (safe-cdr ast)))
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
                       ((string-eq func-name "eql") (if (eql (eval-ast (safe-car args) env) (eval-ast (safe-car (safe-cdr args)) env)) 1 0))
                       ((string-eq func-name "if") (if (eval-ast (safe-car args) env) (eval-ast (safe-car (safe-cdr args)) env) (eval-ast (safe-car (safe-cdr (safe-cdr args))) env)))
                       (1 0))))
                  (1 0))))))

        ;; --- 3. The Code Generator ---
        (compile-number (lambda (val) (begin (print-string "  mov rax, ") (print-int val) (print-char 10))))
        (emit-sym-chunks (lambda (s id chunk-id) (if s (begin (print-string "SYM_") (print-int id) (print-string "_") (print-int chunk-id) (print-line ":") (print-string "  dq ") (print-int (safe-car s)) (print-char 10) (if (safe-cdr s) (begin (print-string "  dq SYM_") (print-int id) (print-string "_") (print-int (add chunk-id 1)) (print-char 10) (emit-sym-chunks (safe-cdr s) id (add chunk-id 1))) (print-line "  dq 0"))) 0)))
        (emit-inline-symbol (lambda (str-chunks)
          (let ((existing (lookup-symbol str-chunks (peek symbol-table))))
            (if existing
                (begin (print-string "  lea rdi, [SYM_") (print-int (safe-car existing)) (print-line "_0]"))
                (let ((id (get-id)))
                  (begin
                    (poke symbol-table (cons (cons str-chunks (cons id 0)) (peek symbol-table)))
                    (print-string "  jmp SYM_AFTER_") (print-int id) (print-char 10)
                    (emit-sym-chunks str-chunks id 0)
                    (print-string "SYM_AFTER_") (print-int id) (print-line ":")
                    (print-string "  lea rdi, [SYM_") (print-int id) (print-line "_0]")))))))

        (count-bindings (lambda (bindings) (if bindings (add 1 (count-bindings (safe-cdr bindings))) 0)))
        (compile-binding (lambda (binding) (let ((binding-items (safe-cdr binding))) (let ((var-node (safe-car binding-items)) (val-node (safe-car (safe-cdr binding-items)))) (begin (compile-expr val-node) (emit-inline-symbol (safe-cdr var-node)) (print-line "  sub rsp, 32") (print-line "  mov [rsp], rdi") (print-line "  mov [rsp+8], rax") (print-line "  mov [rsp+16], rsp") (print-line "  mov [rsp+24], r14") (print-line "  lea r14, [rsp+16]"))))))
        (compile-bindings (lambda (bindings) (if bindings (begin (compile-binding (safe-car bindings)) (compile-bindings (safe-cdr bindings))) 0)))
        (compile-let (lambda (args) (let ((bindings-list (safe-cdr (safe-car args))) (body-stmts (safe-cdr args)) (num-bindings (count-bindings bindings-list))) (begin (print-line "  ;; let block") (print-line "  push r14 ; Save env") (compile-bindings bindings-list) (compile-sequence body-stmts) (print-string "  add rsp, ") (print-int (mul 32 num-bindings)) (print-char 10) (print-line "  pop r14 ; Restore env")))))

        ;; --- 4. Special Forms & Native COND ---
        (compile-if (lambda (args) (let ((cnd (safe-car args)) (thn (safe-car (safe-cdr args))) (els (safe-car (safe-cdr (safe-cdr args)))) (id (get-id))) (begin (print-line "  ;; if block") (compile-expr cnd) (print-line "  test rax, rax") (print-string "  jz IF_ELS_") (print-int id) (print-char 10) (compile-expr thn) (print-string "  jmp IF_END_") (print-int id) (print-char 10) (print-string "IF_ELS_") (print-int id) (print-line ":") (compile-expr els) (print-string "IF_END_") (print-int id) (print-line ":")))))

        (compile-cond-clauses (lambda (clauses end-id)
          (if (eql clauses 0)
              (begin (print-line "  mov rax, 0") (print-string "COND_END_") (print-int end-id) (print-line ":"))
              (let ((clause (safe-car clauses)) (next-id (get-id)))
                (let ((clause-items (safe-cdr clause)))
                  (begin
                    (compile-expr (safe-car clause-items))
                    (print-line "  test rax, rax")
                    (print-string "  jz COND_NEXT_") (print-int next-id) (print-char 10)
                    (compile-sequence (safe-cdr clause-items))
                    (print-string "  jmp COND_END_") (print-int end-id) (print-char 10)
                    (print-string "COND_NEXT_") (print-int next-id) (print-line ":")
                    (compile-cond-clauses (safe-cdr clauses) end-id)))))))

        (compile-cond (lambda (args) (let ((end-id (get-id))) (begin (print-line "  ;; cond block") (compile-cond-clauses args end-id)))))
        (compile-sequence (lambda (stmts) (if (eql stmts 0) 0 (begin (compile-expr (safe-car stmts)) (compile-sequence (safe-cdr stmts))))))

        ;; --- 5. Function Definitions (Lambda & Apply) ---
        (bind-params (lambda (params offset) (if params (begin (emit-inline-symbol (safe-cdr (safe-car params))) (print-string "  mov rsi, [rbp + ") (print-int offset) (print-line "]") (print-line "  sub rsp, 32") (print-line "  mov [rsp], rdi") (print-line "  mov [rsp+8], rsi") (print-line "  mov [rsp+16], rsp") (print-line "  mov [rsp+24], r14") (print-line "  lea r14, [rsp+16]") (bind-params (safe-cdr params) (add offset 8))) 0)))
        (compile-lambda (lambda (args) (let ((params (safe-cdr (safe-car args))) (body-stmts (safe-cdr args)) (id (get-id)) (num-params (count-bindings params))) (begin (print-string "  jmp L_END_") (print-int id) (print-char 10) (print-string "L_START_") (print-int id) (print-line ":") (print-line "  push rbp") (print-line "  mov rbp, rsp") (print-line "  push r14") (bind-params params 16) (compile-sequence body-stmts) (print-line "  mov r14, [rbp - 8]") (print-line "  mov rsp, rbp") (print-line "  pop rbp") (print-string "  ret ") (print-int (mul 8 num-params)) (print-char 10) (print-string "L_END_") (print-int id) (print-line ":") (print-string "  lea rax, [L_START_") (print-int id) (print-line "]")))))
        (compile-args-reverse (lambda (args) (if args (begin (compile-args-reverse (safe-cdr args)) (compile-expr (safe-car args)) (print-line "  push rax")) 0)))
        (compile-apply (lambda (func-node args) (begin (compile-args-reverse args) (compile-expr func-node) (print-line "  call rax"))))

        ;; --- 6. Hardcoded Hardware Primitives ---
        (compile-primitive-nullary (lambda (op-name) (begin (print-string "  ;; inline ") (print-line op-name) (cond ((string-eq op-name "read-char") (print-line "  call read_char")) (1 0)))))
        (compile-primitive-unary (lambda (op-name arg) (begin (compile-expr arg) (print-string "  ;; inline ") (print-line op-name) (cond ((string-eq op-name "print-int") (print-line "  call print_int")) ((string-eq op-name "print-char") (print-line "  call print_char")) ((string-eq op-name "print-chunk") (print-line "  call print_chunk")) ((or (string-eq op-name "car") (string-eq op-name "peek")) (print-line "  mov rax, [rax]")) ((string-eq op-name "cdr") (print-line "  mov rax, [rax+8]")) (1 0)))))
        (compile-primitive-binary (lambda (op-name arg1 arg2) (begin (compile-expr arg1) (print-line "  push rax") (compile-expr arg2) (print-line "  pop rcx") (print-string "  ;; inline ") (print-line op-name) (cond ((string-eq op-name "add") (print-line "  add rax, rcx")) ((string-eq op-name "sub") (begin (print-line "  sub rcx, rax") (print-line "  mov rax, rcx"))) ((string-eq op-name "mul") (print-line "  imul rax, rcx")) ((string-eq op-name "div") (begin (print-line "  mov r8, rax") (print-line "  mov rax, rcx") (print-line "  cqo") (print-line "  idiv r8"))) ((string-eq op-name "eql") (begin (print-line "  cmp rcx, rax") (print-line "  mov rax, 0") (print-line "  sete al"))) ((string-eq op-name "lt") (begin (print-line "  cmp rcx, rax") (print-line "  mov rax, 0") (print-line "  setl al"))) ((string-eq op-name "gt") (begin (print-line "  cmp rcx, rax") (print-line "  mov rax, 0") (print-line "  setg al"))) ((string-eq op-name "le") (begin (print-line "  cmp rcx, rax") (print-line "  mov rax, 0") (print-line "  setle al"))) ((string-eq op-name "ge") (begin (print-line "  cmp rcx, rax") (print-line "  mov rax, 0") (print-line "  setge al"))) ((string-eq op-name "ash") (begin (print-line "  xchg rax, rcx") (print-line "  shl rax, cl"))) ((or (string-eq op-name "logior") (string-eq op-name "or")) (print-line "  or rax, rcx")) ((string-eq op-name "and") (print-line "  and rax, rcx")) ((string-eq op-name "poke") (print-line "  mov [rcx], rax")) ((string-eq op-name "cons") (begin (print-line "  mov [r15], rcx") (print-line "  mov [r15+8], rax") (print-line "  mov rax, r15") (print-line "  add r15, 16"))) (1 0)))))

        (compile-list (lambda (ast-list)
          (let ((func-node (safe-car ast-list)) (args (safe-cdr ast-list)))
            (cond
              ((eql (safe-car func-node) 1)
               (let ((func-name (safe-cdr func-node)))
                 ;; 1. Check if it's a Macro Call
                 (let ((macro-def (lookup-macro func-name (peek compile-env))))
                   (if macro-def
                       (let ((macro-params (safe-car macro-def)) (macro-body (safe-cdr macro-def)))
                         (compile-expr (eval-ast macro-body (bind-macro-args macro-params args 0))))
                       ;; 2. Otherwise, check standard primitives
                       (cond
                         ;; FIX 2: Safely extract macro-sig using safe-cdr to bypass tag 2!
                         ((string-eq func-name "defmacro")
                          (let ((macro-sig (safe-cdr (safe-car args))) (macro-body (safe-car (safe-cdr args))))
                            (poke compile-env (cons (cons (safe-cdr (safe-car macro-sig)) (cons (safe-cdr macro-sig) macro-body)) (peek compile-env)))))
                         ((string-eq func-name "let") (compile-let args))
                         ((string-eq func-name "lambda") (compile-lambda args))
                         ((string-eq func-name "cond") (compile-cond args))
                         ((string-eq func-name "if") (compile-if args))
                         ((string-eq func-name "begin") (compile-sequence args))
                         ((string-eq func-name "read-char") (compile-primitive-nullary func-name))
                         ((or (string-eq func-name "print-int") (or (string-eq func-name "print-char") (or (string-eq func-name "print-chunk") (or (string-eq func-name "car") (or (string-eq func-name "cdr") (string-eq func-name "peek"))))))
                          (compile-primitive-unary func-name (safe-car args)))
                         ((or (string-eq func-name "add") (or (string-eq func-name "sub") (or (string-eq func-name "mul") (or (string-eq func-name "div") (or (string-eq func-name "eql") (or (string-eq func-name "lt") (or (string-eq func-name "gt") (or (string-eq func-name "le") (or (string-eq func-name "ge") (or (string-eq func-name "ash") (or (string-eq func-name "logior") (or (string-eq func-name "or") (or (string-eq func-name "and") (or (string-eq func-name "poke") (string-eq func-name "cons")))))))))))))))
                          (compile-primitive-binary func-name (safe-car args) (safe-car (safe-cdr args))))
                         (1 (compile-apply func-node args)))))))
              (1 (compile-apply func-node args))))))

        (compile-expr (lambda (ast)
          (if (eql ast 0) 0
              (let ((tag (safe-car ast)) (val (safe-cdr ast)))
                (cond
                  ((eql tag 0) (compile-number val))
                  ((eql tag 1) (begin (print-string "  ;; lookup ") (print-string val) (print-char 10) (emit-inline-symbol val) (print-line "  call lookup_env")))
                  ((eql tag 2) (compile-list val))
                  ((eql tag 3) (begin (print-line "  ;; string") (emit-inline-symbol val) (print-line "  mov rax, rdi")))
                  (1 0))))))
       )

    ;; --- 7. THE MAIN ENTRY POINT ---
    (begin
      (print-line "format ELF64 executable 3")
      (print-line "segment readable executable")
      (print-line "include 'runtime.asm'")
      (print-line "entry _start")
      (print-line "_start:")
      (print-line "  mov r14, 0")
      (print-line "  lea r15, [heap_start]")
      (compile-expr (parse-expr))
      (print-line "  mov rdi, rax")
      (print-line "  mov rax, 60")
      (print-line "  syscall")
      (print-line "segment readable writeable")
      (print-line "heap_start: rb 1024 * 1024 * 8"))))
