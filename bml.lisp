(defparameter *primitives*
  '( add  sub  mul  div   ; Integer math
    fadd fsub fmul fdiv   ; Float math (SSE)
    cons car cdr          ; List/Env ops
    print-int  print-hex print-char print-chunk
    peek poke
    eql lt gt
    read-char itof
    ash logand logior)
  "Standard assembly mnemonics mapping 1-to-1 with hardware instructions.")

(defvar *used-symbols* nil)

(defvar *symbols* nil "Alist mapping symbol names (strings) to FASM labels.")
(defvar *strings* nil "Alist mapping string literals to FASM labels.")
(defvar *used-floats* nil "Alist mapping float values to FASM labels.")
(defvar *label-counter* 0 "Counter for generating safe labels.")

(defun track-float (val)
  "Registers a float and returns its safe FASM label."
  (let ((existing (assoc val *used-floats* :test #'=)))
    (if existing
        (cdr existing)
        (let ((label (string-left-trim "#:" (symbol-name (gensym "FLT_")))))
          (push (cons val label) *used-floats*)
          label))))

(defun fasm-label (sym)
  "Converts a Lisp symbol into a valid FASM identifier."
  (map 'string (lambda (c) (if (alphanumericp c) c #\_)) (symbol-name sym)))

(defun reset-compiler-state ()
  (setf *symbols* nil
        *strings* nil
        *label-counter* 0))

(defun register-symbol (sym)
  "Registers a symbol if not seen, and returns its safe FASM label."
  (let ((name (symbol-name sym)))
    (or (cdr (assoc name *symbols* :test #'string=))
        (let ((label (format nil "sym_~a" (incf *label-counter*))))
          (push (cons name label) *symbols*)
          label))))

(defun register-string (str)
  "Registers a string literal if not seen, and returns its safe FASM label."
  (or (cdr (assoc str *strings* :test #'string=))
      (let ((label (format nil "str_~a" (incf *label-counter*))))
        (push (cons str label) *strings*)
        label)))

(defun track-symbol (sym)
  (pushnew sym *used-symbols*))

(defun implicit-lambda-p (expr)
  "An implicit lambda is a list where the first element is a list of symbols (parameters)."
  (let ((head (car expr)))
    (and (consp expr)
         (listp head)
         (every #'symbolp head))))

(defun compile-lambda (expr)
  (let* ((params (car expr))
         (arity (length params))
         (clauses (cdr expr))
         (id (string-left-trim "#:" (symbol-name (gensym "L_"))))
         (start-label    (format nil "~a_start" id))
         (end-label      (format nil "~a_end" id))
         (epilogue-label (format nil "~a_epilogue" id)))
    
    (with-output-to-string (out)
      (format out "  jmp ~a~%" end-label)
      (format out "~a:~%" start-label)
      
      ;; --- PROLOGUE ---
      (format out "  push rbp~%")
      (format out "  mov rbp, rsp~%")
      (format out "  push r14 ; Save old environment at [rbp - 8]~%")
      
      ;; --- INLINE STACK ENVIRONMENT BINDING ---
      ;; Arg 1 is at [rbp + 16], Arg 2 at [rbp + 24], etc.
      (loop for param in params
            for offset from 16 by 8
            do 
            (track-symbol param)
            (format out "  ;; Bind '~a' on STACK~%" param)
            
            ;; FIX: Apply fasm-label to the parameter label
            (format out "  lea rdi, [sym_~a]~%" (fasm-label param))
            
            (format out "  mov rsi, [rbp + ~a]~%" offset)
            ;; Allocate 32 bytes on the stack for the two cons cells
            (format out "  sub rsp, 32~%")
            (format out "  mov [rsp], rdi      ; inner CAR = symbol~%")
            (format out "  mov [rsp+8], rsi    ; inner CDR = value~%")
            (format out "  mov [rsp+16], rsp   ; outer CAR = inner pair ptr~%")
            (format out "  mov [rsp+24], r14   ; outer CDR = old env~%")
            (format out "  lea r14, [rsp+16]   ; R14 = new env head~%"))
            
      ;; --- BODY ---
      (loop for clause in clauses
            for clause-idx from 1
            for next-label = (format nil "~a_next_~a" id clause-idx)
            do
            (let ((test (first clause))
                  (body (second clause)))
              (format out "~a" (compile-expr test nil arity))
              (format out "  test rax, rax~%")
              (format out "  jz ~a~%" next-label)
              (format out "~a" (compile-expr body t arity))
              (format out "  jmp ~a~%" epilogue-label)
              (format out "~a:~%" next-label)))
              
      (format out "  mov rax, 0~%")
      
      ;; --- EPILOGUE ---
      (format out "~a:~%" epilogue-label)
      (format out "  mov r14, [rbp - 8] ; Restore caller environment~%")
      (format out "  mov rsp, rbp       ; O(1) STACK CLEANUP! (Destroys local env)~%")
      (format out "  pop rbp~%")
      (format out "  ret ~a             ; STDCALL clean caller args~%" (* 8 arity))
      
      (format out "~a:~%" end-label)
      (format out "  lea rax, [~a]~%" start-label))))

(defun compile-sse (out asm-op)
  "Helper for SSE floating point primitives. Writes directly to stream."
  (format out "  movq xmm0, rcx~%")
  (format out "  movq xmm1, rax~%")
  ;; For fsub/fdiv, order matters: XMM0 (left) op XMM1 (right)
  (format out "  ~a xmm0, xmm1~%" asm-op)
  (format out "  movq rax, xmm0~%"))

(defun compile-inline-primitive (expr parent-arity)
  (let ((op (car expr))
        (args (cdr expr)))
    (with-output-to-string (out)
      (cond
        ;; --- NULLARY (0 Arguments) ---
        ((eq op 'read-char)
         (format out "  ;; inline read-char~%")
         (format out "  call read_char~%"))
        
        ;; --- UNARY ---
        ((member op '(car cdr print-int print-hex print-char print-chunk peek itof))
         (format out "~a" (compile-expr (first args) nil parent-arity))
         (format out "  ;; inline ~a~%" op)
         (case op
           (car         (format out "  mov rax, [rax]~%"))
           (cdr         (format out "  mov rax, [rax+8]~%"))
           (peek        (format out "  mov rax, [rax]~%"))
           (itof        (format out "  cvtsi2sd xmm0, rax~%")
                        (format out "  movq rax, xmm0~%"))
           (print-int   (format out "  call print_int~%"))
           (print-hex   (format out "  call print_hex~%"))
           (print-char  (format out "  call print_char~%"))
           (print-chunk (format out "  call print_chunk~%"))))

        ;; --- BINARY ---
        ((member op '(add sub mul div fadd fsub fmul fdiv cons poke eql lt gt
                      ash logand logior))
         (format out "~a" (compile-expr (first args) nil parent-arity))
         (format out "  push rax~%")
         (format out "~a" (compile-expr (second args) nil parent-arity))
         (format out "  pop rcx~%")
         (format out "  ;; inline ~a~%" op)
         (case op
           (ash
            (let ((lbl-left (string-left-trim "#:" (symbol-name (gensym "ASH_LEFT_"))))
                  (lbl-done (string-left-trim "#:" (symbol-name (gensym "ASH_DONE_")))))
              (format out "  test rax, rax~%")
              (format out "  jns ~a~%" lbl-left)
              ;; Negative Count: Arithmetic Shift Right
              (format out "  neg rax~%")       ; Make count positive
              (format out "  xchg rax, rcx~%") ; Swap: RAX=value, RCX=count
              (format out "  sar rax, cl~%")   ; Shift Arithmetic Right by CL
              (format out "  jmp ~a~%" lbl-done)
              ;; Positive Count: Shift Left
              (format out "~a:~%" lbl-left)
              (format out "  xchg rax, rcx~%") ; Swap: RAX=value, RCX=count
              (format out "  shl rax, cl~%")   ; Shift Left by CL
              (format out "~a:~%" lbl-done)))
           (logand (format out "  and rax, rcx~%"))
           (logior (format out "  or rax, rcx~%"))
           (eql
            (format out "  cmp rcx, rax~%")
            (format out "  mov rax, 0~%")    ; Clears RAX without destroying EFLAGS
            (format out "  sete al~%"))      ; Sets lowest byte (AL) to 1 if equal
           (lt
            (format out "  cmp rcx, rax~%")
            (format out "  mov rax, 0~%")
            (format out "  setl al~%"))      ; Sets AL to 1 if RCX < RAX (signed)
           (gt
            (format out "  cmp rcx, rax~%")
            (format out "  mov rax, 0~%")
            (format out "  setg al~%"))      ; Sets AL to 1 if RCX > RAX (signed)
           (poke 
            (format out "  mov [rcx], rax~%")) ; Overwrite memory at [RCX] with RAX
           (cons
            (format out "  mov [r15], rcx~%")
            (format out "  mov [r15+8], rax~%")
            (format out "  mov rax, r15~%")
            (format out "  add r15, 16~%"))
           (add
            (format out "  add rax, rcx~%"))
           (sub
            (format out "  sub rcx, rax~%")
            (format out "  mov rax, rcx~%"))
           (mul
            (format out "  imul rax, rcx~%"))
           (div
            (format out "  mov r8, rax~%")
            (format out "  mov rax, rcx~%")
            (format out "  cqo  ; sign-extend RAX into RDX~%")
            (format out "  idiv r8~%")) ; Result in RAX

           ;; SSE Floating Point Math (using double precision: 64-bit)
           (fadd (compile-sse out "addsd"))
           (fsub (compile-sse out "subsd"))
           (fmul (compile-sse out "mulsd"))
           (fdiv (compile-sse out "divsd"))))

        (t (error "Unsupported primitive: ~a" op))))))

(defun compile-string-literal (expr)
  "Evaluates to the pointer of a statically allocated string list."
  (let ((label (register-string expr)))
    (format nil "  lea rax, [~a]~%" label)))

(defun compile-float-literal (expr)
  (let ((label (track-float expr)))
    ;; Dereference the memory address to load the float into RAX
    (format nil "  mov rax, [~a]~%" label)))

(defun compile-syscall (expr parent-arity)
  "Compiles (syscall num arg1 arg2 ...) up to 6 args."
  (let ((args (cdr expr))
        ;; Linux x86_64 syscall registers in order
        (regs '("rax" "rdi" "rsi" "rdx" "r10" "r8" "r9"))) 
    (when (> (length args) 7)
      (error "Linux syscalls take a maximum of 6 arguments + 1 syscall number"))
      
    (with-output-to-string (out)
      (format out "  ;; syscall~%")
      ;; Evaluate right-to-left and push to stack (not in tail position)
      (loop for arg in (reverse args) do 
            (format out "~a" (compile-expr arg nil parent-arity))
            (format out "  push rax~%"))
            
      ;; Pop into the correct kernel registers left-to-right
      (loop for i from 0 below (length args) do 
            (format out "  pop ~a~%" (nth i regs)))
            
      (format out "  syscall~%"))))

(defun compile-lookup (expr)
  "Compiles a variable lookup. The symbol name is passed to lookup_env."
  (track-symbol expr)
  (with-output-to-string (out)
    (format out "  ;; Lookup variable '~a'~%" expr)
    (format out "  lea rdi, [sym_~a]~%" (fasm-label expr))
    (format out "  call lookup_env~%")))

(defun chunk-to-int (chunk)
  "Converts a string chunk (up to 8 chars) into a little-endian 64-bit integer."
  (let ((val 0))
    (loop for i from 0 below (length chunk)
          for char = (char chunk i)
          for code = (char-code char)
          do (setf val (logior val (ash code (* i 8)))))
    val))

(defun emit-static-list (out label str)
  "Emits a linked list of 8-byte string chunks under LABEL to stream OUT."
  (let ((len (length str)))
    (if (= len 0)
        ;; Edge case: empty string / empty symbol
        (progn
          (format out "~a:~%" label)
          (format out "  dq 0~%")
          (format out "  dq 0~%"))
        ;; Normal case: slice into chunks of 8
        (loop for i from 0 below len by 8
              for chunk-idx from 0
              for chunk = (subseq str i (min len (+ i 8)))
              for is-first = (= i 0)
              for is-last = (>= (+ i 8) len)
              do
              (if is-first
                  (format out "~a:~%" label)
                  (format out "~a_~a:~%" label chunk-idx))
                  
              ;; Print the CAR (the 8-byte hex characters)
              (format out "  dq 0x~16,'0X  ; '~a'~%" (chunk-to-int chunk) chunk)
              
              ;; Print the CDR (pointer to next chunk, or 0)
              (if is-last
                  (format out "  dq 0~%")
                  (format out "  dq ~a_~a~%" label (1+ chunk-idx)))))))

(defun expand-macro (expr)
  "Expands syntactic sugar down into our native special forms."
  (if (not (consp expr))
      expr
      (let ((head (car expr)))
        (case head
          ;; (not a) -> (if a 0 1)
          (not `(if ,(expand-macro (second expr)) 0 1))
          
          ;; (and a b) -> (if a b 0)
          (and `(if ,(expand-macro (second expr)) ,(expand-macro (third expr)) 0))
          
          ;; (or a b) -> (let ((tmp a)) (if tmp tmp b))
          (or
           (let ((tmp (gensym "OR_")))
             (expand-macro 
              `(let ((,tmp ,(second expr)))
                 (if ,tmp ,tmp ,(third expr))))))

          ;; (le a b) -> (not (gt a b)) -> (if (gt a b) 0 1)
          (le `(if (gt ,(expand-macro (second expr)) ,(expand-macro (third expr))) 0 1))
          
          ;; (ge a b) -> (not (lt a b)) -> (if (lt a b) 0 1)
          (ge `(if (lt ,(expand-macro (second expr)) ,(expand-macro (third expr))) 0 1))
          
          ;; (if cnd thn els) -> pure sugar for a cond block!
          (if
           `(cond (,(expand-macro (second expr)) ,(expand-macro (third expr)))
                  (1 ,(expand-macro (fourth expr)))))
                  
          ;; (defun name (args) body) -> pure sugar for let + lambda!
          (defun
           (let ((name (second expr))
                 (args (third expr))
                 (body (cdddr expr)))
             (expand-macro `(let ((,name (lambda ,args ,@body)))))))

          ;; (lambda (args) body) -> expands to implicit lambda syntax
          (lambda
           `(,(second expr) (1 (begin ,@(mapcar #'expand-macro (cddr expr))))))

          ;; (cond ...), (let ...), (begin ...) just recursively expand their bodies
          (cond  `(cond ,@(mapcar (lambda (c) (mapcar #'expand-macro c)) (cdr expr))))
          ;; (let   `(let ,(second expr) ,@(mapcar #'expand-macro (cddr expr))))
          (let
           (let ((bindings (second expr))
                 (body (cddr expr)))
             ;; FIX: Recursively expand the values inside the let bindings!
             `(let ,(mapcar (lambda (b) 
                              (list (first b) (expand-macro (second b)))) 
                            bindings)
                ,@(mapcar #'expand-macro body))))
          (begin `(begin ,@(mapcar #'expand-macro (cdr expr))))

          (otherwise
           (mapcar #'expand-macro expr))))))

(defun emit-data-section (out)
  "Generates floats, symbols, and string literals in the .data segment."
  (format out "segment readable writeable~%~%")

  (format out "  ;; --- FLOATS ---~%")
  (loop for (val . label) in *used-floats* do
        (format out "~a: dq ~f~%" label val))

  (format out "~%  ;; --- SYMBOLS ---~%")
  (loop for sym in *used-symbols*
        for str = (symbol-name sym)
        for safe-name = (format nil "sym_~a" (fasm-label sym))
        do (emit-static-list out safe-name str))

  (format out "~%  ;; --- STRINGS ---~%")
  (loop for (str . label) in *strings* do
        (emit-static-list out label str))

  (format out "~%  ;; Uninitialized heap memory requested from OS~%")
  (format out "heap_start: rb 1024 * 1024 * 8~%~%"))

(defun compile-apply (expr tail-p parent-arity)
  (let* ((func (car expr))
         (args (cdr expr))
         (num-args (length args))
         (tce-regs '("r8" "r9" "r10" "r11" "r12" "r13")))
    
    (with-output-to-string (out)
      ;; FIX: Safe, single-line debug comment
      (format out "  ;; ~a ~a~%" 
              (if tail-p "tail-call" "call") 
              (if (symbolp func) func "<lambda>"))
      
      (loop for arg in (reverse args) do 
            (format out "~a" (compile-expr arg nil parent-arity))
            (format out "  push rax~%"))
            
      (format out "~a" (compile-expr func nil parent-arity))
      
      (if tail-p
          ;; --- TRUE O(1) TAIL CALL ---
          (progn
            (when (> num-args 6) (error "TCE stack shuffle restricted to 6 args."))
            
            ;; 1. Pop new args into temporary hardware registers
            (loop for i from 1 to num-args do 
                  (format out "  pop ~a~%" (nth (1- i) tce-regs)))
                  
            ;; 2. REWIND THE STACK (O(1) Environment destruction!)
            (format out "  mov r14, [rbp - 8] ; Restore old env~%")
            (format out "  mov rsp, rbp       ; Destroy current frame and env~%")
            (format out "  pop rbp            ; Restore caller RBP~%")
            (format out "  pop rcx            ; Pop return address~%")
            (format out "  add rsp, ~a        ; Clean parent args~%" (* 8 parent-arity))
            
            ;; 3. Push the new args
            (loop for i from num-args downto 1 do 
                  (format out "  push ~a~%" (nth (1- i) tce-regs)))
                  
            ;; 4. Push return address and jump
            (format out "  push rcx~%")
            (format out "  jmp rax~%"))
          
          ;; --- STANDARD STDCALL ---
          (format out "  call rax~%")))))

(defun compile-expr (expr &optional tail-p parent-arity)
  "Translates a single S-expression. Tracks tail position and parent arity for TCE."
  (cond
    ((null     expr) "  mov rax, 0~%")
    ((integerp expr) (format nil "  mov rax, ~a~%" expr))
    ((floatp   expr) (compile-float-literal expr)) 
    ((stringp  expr) (compile-string-literal expr))
    ((symbolp  expr) (compile-lookup expr))
    ((consp expr)
     (let ((head (car expr)))
       (cond
         ;; 1. Syscall Special Form
         ((eq head 'syscall)
          (compile-syscall expr parent-arity))

         ;; 2. Begin Special Form
         ((eq head 'begin)
          (with-output-to-string (out)
            (format out "  ;; begin block~%")
            (let ((stmts (cdr expr)))
              (loop for rest on stmts
                    for stmt = (car rest)
                    for is-last = (null (cdr rest))
                    do (format out "~a" (compile-expr stmt (and tail-p is-last) parent-arity))))))

         ;; 3. If Special Form (Zero-cost native branching)
         ((eq head 'if)
          (let ((cnd (second expr))
                (thn (third expr))
                (els (fourth expr))
                (els-label (string-left-trim "#:" (symbol-name (gensym "IF_ELS_"))))
                (end-label (string-left-trim "#:" (symbol-name (gensym "IF_END_")))))
            (with-output-to-string (out)
              (format out "  ;; if block~%")
              (format out "~a" (compile-expr cnd nil parent-arity))
              (format out "  test rax, rax~%")
              (format out "  jz ~a~%" els-label)
              (format out "~a" (compile-expr thn tail-p parent-arity))
              (format out "  jmp ~a~%" end-label)
              (format out "~a:~%" els-label)
              (format out "~a" (compile-expr els tail-p parent-arity))
              (format out "~a:~%" end-label))))

         ;; 4. Cond Special Form
         ((eq head 'cond)
          (let ((clauses (cdr expr))
                (end-label (string-left-trim "#:" (symbol-name (gensym "COND_END_")))))
            (with-output-to-string (out)
              (format out "  ;; cond block~%")
              (loop for clause in clauses
                    for next-label = (string-left-trim "#:" (symbol-name (gensym "COND_NEXT_"))) do
                    (format out "~a" (compile-expr (first clause) nil parent-arity))
                    (format out "  test rax, rax~%")
                    (format out "  jz ~a~%" next-label)
                    (format out "~a" (compile-expr (second clause) tail-p parent-arity))
                    (format out "  jmp ~a~%" end-label)
                    (format out "~a:~%" next-label))
              (format out "  mov rax, 0~%")
              (format out "~a:~%" end-label))))

         ;; 5. Let Special Form (Native stack allocation - sequential 'let*')
         ((eq head 'let)
          (let* ((bindings (second expr))
                 (body (cddr expr))
                 (num-bindings (length bindings)))
            (with-output-to-string (out)
              (format out "  ;; let block~%")
              (format out "  push r14 ; Save env~%")
              
              ;; Evaluate and bind sequentially! (let* semantics)
              (loop for binding in bindings
                    for var = (first binding)
                    for val = (second binding) do
                    (track-symbol var)
                    
                    ;; 1. Evaluate the value into RAX
                    (format out "~a" (compile-expr val nil parent-arity))
                    
                    ;; 2. Allocate and bind immediately
                    (format out "  lea rdi, [sym_~a]~%" (fasm-label var))
                    (format out "  sub rsp, 32~%")
                    (format out "  mov [rsp], rdi~%")
                    (format out "  mov [rsp+8], rax ; Bind the evaluated value!~%")
                    (format out "  mov [rsp+16], rsp~%")
                    (format out "  mov [rsp+24], r14~%")
                    (format out "  lea r14, [rsp+16]~%"))
                    
              ;; Evaluate body (inherits tail-p gracefully!)
              (loop for rest on body
                    for stmt = (car rest)
                    for is-last = (null (cdr rest)) do
                    (format out "~a" (compile-expr stmt (and tail-p is-last) parent-arity)))
                    
              ;; Cleanup if we didn't tail-call out of it
              (format out "  add rsp, ~a ; Pop let bindings~%" (* 32 num-bindings))
              (format out "  pop r14 ; Restore env~%"))))

         ;; 6. Lambda Special Form (Translates syntax cleanly to implicit array)
         ((eq head 'lambda)
          (compile-lambda `(,(second expr) (1 (begin ,@(cddr expr))))))

         ;; 7. Implicit Lambda
         ((implicit-lambda-p expr)
          (compile-lambda expr))

         ;; 8. Inline Primitives
         ((and (symbolp head) (member head *primitives*))
          (compile-inline-primitive expr parent-arity))

         ;; 9. Standard Function Application
         (t
          (compile-apply expr tail-p parent-arity)))))

    (t (error "Compiler error: Unrecognized expression type: ~a" expr))))

(defun compile-program (ast filepath)
  "Compiles a single, self-contained Lisp AST into a FASM executable."
  (let ((*used-symbols* nil)
        (*used-floats* nil)
        (*symbols* nil)
        (*strings* nil)
        (*label-counter* 0))
    (with-open-file (out filepath :direction :output :if-exists :supersede)
      (format out "format ELF64 executable 3~%")
      (format out "segment readable executable~%")
      (format out "include 'runtime.fasm'~%~%")
      
      (format out "entry _start~%")
      (format out "_start:~%")
      
      ;; System Initialization
      (format out "  mov r14, 0~%") 
      (format out "  lea r15, [heap_start]~%~%")
      
      ;; FIX 1: The AST must be expanded BEFORE compilation!
      (format out "~a~%" (compile-expr (expand-macro ast)))
      
      ;; System Exit
      (format out "  mov rdi, rax~%")
      (format out "  mov rax, 60~%")
      (format out "  syscall~%~%")
      
      ;; Data section
      (emit-data-section out))))

(defun build-program (input-filepath output-filepath)
  "Reads a Lisp source file and compiles it into a FASM executable."
  (let* ((ast (with-open-file (in input-filepath :direction :input)
                (loop for expr = (read in nil :eof)
                      until (eq expr :eof)
                      collect expr)))
         ;; Wrap the entire file's contents in an implicit begin block
         (program `(begin ,@ast)))
    (compile-program program output-filepath)))

(build-program "boot.lisp" "boot.fasm")
