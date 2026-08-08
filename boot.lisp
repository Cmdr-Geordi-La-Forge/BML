(let ((lookahead (cons 0 0))
      (label-counter (cons 0 0))) 
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
        (skip-whitespace (lambda () (let ((c (next-char))) (if (is-space c) (skip-whitespace) (if (eql c 59) (skip-comment) (unget-char c))))))
        
        (print-string (lambda (s) (if s (begin (print-chunk (safe-car s)) (print-string (safe-cdr s))) 0)))
        (print-line (lambda (s) (begin (print-string s) (print-char 10))))
        (string-eq (lambda (s1 s2) (if (and (eql s1 0) (eql s2 0)) 1 (if (or (eql s1 0) (eql s2 0)) 0 (if (eql (safe-car s1) (safe-car s2)) (string-eq (safe-cdr s1) (safe-cdr s2)) 0)))))
        (get-id (lambda () (let ((id (peek label-counter))) (begin (poke label-counter (add id 1)) id))))
        
        ;; --- 2. The Lexer/Parser ---
        (parse-int (lambda (acc) (let ((c (next-char))) (if (is-digit c) (parse-int (add (mul acc 10) (sub c 48))) (begin (unget-char c) acc)))))
        (parse-symbol-chunk (lambda (acc count) (let ((c (next-char))) (if (is-delim c) (begin (unget-char c) acc) (if (eql count 8) (begin (unget-char c) acc) (parse-symbol-chunk (logior acc (ash c (mul count 8))) (add count 1)))))))
        (parse-symbol (lambda () (let ((chunk (parse-symbol-chunk 0 0))) (let ((next (peek lookahead))) (if (is-delim next) (cons chunk 0) (cons chunk (parse-symbol)))))))
        
        (parse-string-chunk (lambda (acc count) (let ((c (next-char))) (if (or (eql c 34) (eql c 0)) (begin (unget-char c) acc) (if (eql count 8) (begin (unget-char c) acc) (parse-string-chunk (logior acc (ash c (mul count 8))) (add count 1)))))))
        (parse-string (lambda () (let ((chunk (parse-string-chunk 0 0))) (let ((next (peek lookahead))) (if (or (eql next 34) (eql next 0)) (begin (if (eql next 34) (next-char) 0) (cons chunk 0)) (cons chunk (parse-string)))))))
        
        (parse-list (lambda () (begin (skip-whitespace) (let ((c (next-char))) (if (or (eql c 41) (eql c 0)) 0 (begin (unget-char c) (cons (parse-expr) (parse-list))))))))
        (parse-expr (lambda () (begin (skip-whitespace) (let ((c (next-char))) (if (eql c 40) (cons 2 (parse-list)) (if (eql c 34) (cons 3 (parse-string)) (if (is-digit c) (begin (unget-char c) (cons 0 (parse-int 0))) (begin (unget-char c) (cons 1 (parse-symbol))))))))))
        
        ;; --- 3. The Code Generator ---
        (compile-number (lambda (val) (begin (print-string "  mov rax, ") (print-int val) (print-char 10))))
        (emit-sym-chunks (lambda (s id chunk-id) (if s (begin (print-string "SYM_") (print-int id) (print-string "_") (print-int chunk-id) (print-line ":") (print-string "  dq ") (print-int (safe-car s)) (print-char 10) (if (safe-cdr s) (begin (print-string "  dq SYM_") (print-int id) (print-string "_") (print-int (add chunk-id 1)) (print-char 10) (emit-sym-chunks (safe-cdr s) id (add chunk-id 1))) (print-line "  dq 0"))) 0)))
        (emit-inline-symbol (lambda (str-chunks) (let ((id (get-id))) (begin (print-string "  jmp SYM_AFTER_") (print-int id) (print-char 10) (emit-sym-chunks str-chunks id 0) (print-string "SYM_AFTER_") (print-int id) (print-line ":") (print-string "  lea rdi, [SYM_") (print-int id) (print-line "_0]")))))
        
        (count-bindings (lambda (bindings) (if bindings (add 1 (count-bindings (safe-cdr bindings))) 0)))
        (compile-binding (lambda (binding) (let ((binding-items (safe-cdr binding))) (let ((var-node (safe-car binding-items)) (val-node (safe-car (safe-cdr binding-items)))) (begin (compile-expr val-node) (emit-inline-symbol (safe-cdr var-node)) (print-line "  sub rsp, 32") (print-line "  mov [rsp], rdi") (print-line "  mov [rsp+8], rax") (print-line "  mov [rsp+16], rsp") (print-line "  mov [rsp+24], r14") (print-line "  lea r14, [rsp+16]"))))))
        (compile-bindings (lambda (bindings) (if bindings (begin (compile-binding (safe-car bindings)) (compile-bindings (safe-cdr bindings))) 0)))
        (compile-let (lambda (args) (let ((bindings-list (safe-cdr (safe-car args))) (body-stmts (safe-cdr args)) (num-bindings (count-bindings bindings-list))) (begin (print-line "  ;; let block") (print-line "  push r14 ; Save env") (compile-bindings bindings-list) (compile-sequence body-stmts) (print-string "  add rsp, ") (print-int (mul 32 num-bindings)) (print-char 10) (print-line "  pop r14 ; Restore env")))))

        ;; --- 4. Function Definitions (Lambda & Apply) ---
        (bind-params (lambda (params offset) (if params (begin (emit-inline-symbol (safe-cdr (safe-car params))) (print-string "  mov rsi, [rbp + ") (print-int offset) (print-line "]") (print-line "  sub rsp, 32") (print-line "  mov [rsp], rdi") (print-line "  mov [rsp+8], rsi") (print-line "  mov [rsp+16], rsp") (print-line "  mov [rsp+24], r14") (print-line "  lea r14, [rsp+16]") (bind-params (safe-cdr params) (add offset 8))) 0)))
        (compile-lambda (lambda (args) (let ((params (safe-cdr (safe-car args))) (body-stmts (safe-cdr args)) (id (get-id)) (num-params (count-bindings params))) (begin (print-string "  jmp L_END_") (print-int id) (print-char 10) (print-string "L_START_") (print-int id) (print-line ":") (print-line "  push rbp") (print-line "  mov rbp, rsp") (print-line "  push r14") (bind-params params 16) (compile-sequence body-stmts) (print-line "  mov r14, [rbp - 8]") (print-line "  mov rsp, rbp") (print-line "  pop rbp") (print-string "  ret ") (print-int (mul 8 num-params)) (print-char 10) (print-string "L_END_") (print-int id) (print-line ":") (print-string "  lea rax, [L_START_") (print-int id) (print-line "]")))))
        (compile-args-reverse (lambda (args) (if args (begin (compile-args-reverse (safe-cdr args)) (compile-expr (safe-car args)) (print-line "  push rax")) 0)))
        (compile-apply (lambda (func-node args) (begin (compile-args-reverse args) (compile-expr func-node) (print-line "  call rax"))))

        ;; --- 5. Hardcoded Hardware Primitives ---
        (compile-primitive-nullary (lambda (op-name) (begin (print-string "  ;; inline ") (print-line op-name) (if (string-eq op-name "read-char") (print-line "  call read_char") 0))))
        
        (compile-primitive-unary (lambda (op-name arg) (begin (compile-expr arg) (print-string "  ;; inline ") (print-line op-name) (if (string-eq op-name "print-int") (print-line "  call print_int") (if (string-eq op-name "print-char") (print-line "  call print_char") (if (string-eq op-name "print-chunk") (print-line "  call print_chunk") (if (or (string-eq op-name "car") (string-eq op-name "peek")) (print-line "  mov rax, [rax]") (if (string-eq op-name "cdr") (print-line "  mov rax, [rax+8]") 0))))))))
        
        (compile-primitive-binary (lambda (op-name arg1 arg2) (begin (compile-expr arg1) (print-line "  push rax") (compile-expr arg2) (print-line "  pop rcx") (print-string "  ;; inline ") (print-line op-name) (if (string-eq op-name "add") (print-line "  add rax, rcx") (if (string-eq op-name "sub") (begin (print-line "  sub rcx, rax") (print-line "  mov rax, rcx")) (if (string-eq op-name "mul") (print-line "  imul rax, rcx") (if (string-eq op-name "div") (begin (print-line "  mov r8, rax") (print-line "  mov rax, rcx") (print-line "  cqo") (print-line "  idiv r8")) (if (string-eq op-name "eql") (begin (print-line "  cmp rcx, rax") (print-line "  mov rax, 0") (print-line "  sete al")) (if (string-eq op-name "lt") (begin (print-line "  cmp rcx, rax") (print-line "  mov rax, 0") (print-line "  setl al")) (if (string-eq op-name "gt") (begin (print-line "  cmp rcx, rax") (print-line "  mov rax, 0") (print-line "  setg al")) (if (string-eq op-name "le") (begin (print-line "  cmp rcx, rax") (print-line "  mov rax, 0") (print-line "  setle al")) (if (string-eq op-name "ge") (begin (print-line "  cmp rcx, rax") (print-line "  mov rax, 0") (print-line "  setge al")) (if (string-eq op-name "ash") (begin (print-line "  xchg rax, rcx") (print-line "  shl rax, cl")) (if (or (string-eq op-name "logior") (string-eq op-name "or")) (print-line "  or rax, rcx") (if (string-eq op-name "and") (print-line "  and rax, rcx") (if (string-eq op-name "poke") (print-line "  mov [rcx], rax") (if (string-eq op-name "cons") (begin (print-line "  mov [r15], rcx") (print-line "  mov [r15+8], rax") (print-line "  mov rax, r15") (print-line "  add r15, 16")) 0)))))))))))))))))
        
        (compile-if (lambda (args) (let ((cnd (safe-car args)) (thn (safe-car (safe-cdr args))) (els (safe-car (safe-cdr (safe-cdr args)))) (id (get-id))) (begin (print-line "  ;; if block") (compile-expr cnd) (print-line "  test rax, rax") (print-string "  jz IF_ELS_") (print-int id) (print-char 10) (compile-expr thn) (print-string "  jmp IF_END_") (print-int id) (print-char 10) (print-string "IF_ELS_") (print-int id) (print-line ":") (compile-expr els) (print-string "IF_END_") (print-int id) (print-line ":")))))
        (compile-sequence (lambda (stmts) (if (eql stmts 0) 0 (begin (compile-expr (safe-car stmts)) (compile-sequence (safe-cdr stmts))))))
        
        (compile-list (lambda (ast-list)
          (let ((func-node (safe-car ast-list)) (args (safe-cdr ast-list)))
            (if (eql (safe-car func-node) 1)
                (let ((func-name (safe-cdr func-node)))
                  (if (string-eq func-name "begin") (compile-sequence args)
                  (if (string-eq func-name "let") (compile-let args)
                  (if (string-eq func-name "lambda") (compile-lambda args)
                  (if (string-eq func-name "read-char") (compile-primitive-nullary func-name)
                  (if (or (string-eq func-name "print-int") (or (string-eq func-name "print-char") (or (string-eq func-name "print-chunk") (or (string-eq func-name "car") (or (string-eq func-name "cdr") (string-eq func-name "peek"))))))
                      (compile-primitive-unary func-name (safe-car args))
                  (if (or (string-eq func-name "add") (or (string-eq func-name "sub") (or (string-eq func-name "mul") (or (string-eq func-name "div") (or (string-eq func-name "eql") (or (string-eq func-name "lt") (or (string-eq func-name "gt") (or (string-eq func-name "le") (or (string-eq func-name "ge") (or (string-eq func-name "ash") (or (string-eq func-name "logior") (or (string-eq func-name "or") (or (string-eq func-name "and") (or (string-eq func-name "poke") (string-eq func-name "cons")))))))))))))))
                      (compile-primitive-binary func-name (safe-car args) (safe-car (safe-cdr args)))
                  (if (string-eq func-name "if") (compile-if args)
                      (compile-apply func-node args)))))))))
                (compile-apply func-node args)))))
                
        (compile-expr (lambda (ast) (if (eql ast 0) 0 (let ((tag (safe-car ast)) (val (safe-cdr ast))) (if (eql tag 0) (compile-number val) (if (eql tag 1) (begin (print-string "  ;; lookup ") (print-string val) (print-char 10) (emit-inline-symbol val) (print-line "  call lookup_env")) (if (eql tag 2) (compile-list val) (if (eql tag 3) (begin (print-line "  ;; string") (emit-inline-symbol val) (print-line "  mov rax, rdi")) 0)))))))))
       
    ;; --- 6. THE MAIN ENTRY POINT ---
    (begin
      (print-line "format ELF64 executable 3")
      (print-line "segment readable executable")
      (print-line "include 'runtime.fasm'")
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
