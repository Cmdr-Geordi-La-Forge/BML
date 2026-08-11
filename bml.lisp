(defparameter *primitives*
  '( add  sub  mul  div   ; Integer math
    fadd fsub fmul fdiv   ; Float math (SSE)
    cons car cdr          ; List/Env ops
    print-int  print-hex print-char print-chunk
    peek poke
    alloc peek-idx poke-idx
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

(defvar *hoisted-functions* nil "List of raw assembly strings for compiled lambdas.")

(defun compile-lambda (expr comp-env)
  (let* ((params (car expr))
         (arity (length params))
         (clauses (cdr expr))
         (id (string-left-trim "#:" (symbol-name (gensym "L_"))))
         (start-label (format nil "~a_start" id))
         (lambda-env (remove-if-not (lambda (b) (stringp (cdr b))) comp-env)))

    ;; Map parameters to their hardware stack locations
    (loop for param in params
          for offset from 16 by 8
          do (push (cons param offset) lambda-env))

    ;; 1. Generate the function body, but DO NOT return it!
    (let ((func-asm
           (with-output-to-string (out)
             (format out "~a:~%" start-label)
             (format out "  push rbp~%")
             (format out "  mov rbp, rsp~%")

             (loop for clause in clauses
                   for clause-idx from 1
                   for next-label = (format nil "~a_next_~a" id clause-idx) do
                   (let ((test (first clause))
                         (body (cdr clause))) ; Grab the rest as a sequence

                     (format out "~a" (compile-expr test lambda-env nil arity))
                     (format out "  test rax, rax~%")
                     (format out "  jz ~a~%" next-label)

                     ;; --- THE IMPLICIT BEGIN ---
                     (loop for rest on body
                           for stmt = (car rest)
                           for is-last = (null (cdr rest)) do
                           ;; The last statement is ALWAYS in tail position
                           (format out "~a" (compile-expr stmt lambda-env is-last arity)))

                     ;; Inline epilogue (O(1) stack cleanup)
                     (format out "  mov rsp, rbp~%")
                     (format out "  pop rbp~%")
                     (format out "  ret ~a~%" (* 8 arity))
                     (format out "~a:~%" next-label)))

             ;; Default return 0 if no conditions match
             (format out "  mov rax, 0~%")
             (format out "  mov rsp, rbp~%")
             (format out "  pop rbp~%")
             (format out "  ret ~a~%" (* 8 arity)))))

      ;; 2. Hoist it to the global list
      (push func-asm *hoisted-functions*)

      ;; 3. Return ONLY the pointer load to the caller!
      (format nil "  lea rax, [~a]~%" start-label))))

(defun compile-sse (out asm-op)
  "Helper for SSE floating point primitives. Writes directly to stream."
  (format out "  movq xmm0, rcx~%")
  (format out "  movq xmm1, rax~%")
  ;; For fsub/fdiv, order matters: XMM0 (left) op XMM1 (right)
  (format out "  ~a xmm0, xmm1~%" asm-op)
  (format out "  movq rax, xmm0~%"))

(defun compile-inline-primitive (expr comp-env parent-arity)
  (let ((op (car expr))
        (args (cdr expr)))
    (with-output-to-string (out)
      (cond
        ((eq op 'read-char)
         (format out "  ;; inline read-char~%")
         (format out "  call read_char~%"))

        ;; unary
        ((member op '(car cdr print-int print-hex print-char print-chunk peek itof alloc))
         (format out "~a" (compile-expr (first args) comp-env nil parent-arity))
         (format out "  ;; inline ~a~%" op)
         (case op
           (car         (format out "  mov rax, [rax]~%"))
           (cdr         (format out "  mov rax, [rax+8]~%"))
           (peek        (format out "  mov rax, [rax]~%"))
           (alloc       (format out "  imul rax, 8~%")
                        (format out "  mov rcx, r15~%")
                        (format out "  add r15, rax~%")
                        (format out "  mov rax, rcx~%"))
           (itof        (format out "  cvtsi2sd xmm0, rax~%")
                        (format out "  movq rax, xmm0~%"))
           (print-int   (format out "  call print_int~%"))
           (print-hex   (format out "  call print_hex~%"))
           (print-char  (format out "  call print_char~%"))
           (print-chunk (format out "  call print_chunk~%"))))

        ;; binary
        ((member op '(add sub mul div fadd fsub fmul fdiv cons poke eql lt gt
                      ash logand logior peek-idx))
         (format out "~a" (compile-expr (first args) comp-env nil parent-arity))
         (format out "  push rax~%")
         (format out "~a" (compile-expr (second args) comp-env nil parent-arity))
         (format out "  pop rcx~%")
         (format out "  ;; inline ~a~%" op)
         (case op
           (peek-idx (format out "  mov rax, [rcx + rax*8]~%"))
           (ash
            (let ((lbl-left (string-left-trim "#:" (symbol-name (gensym "ASH_LEFT_"))))
                  (lbl-done (string-left-trim "#:" (symbol-name (gensym "ASH_DONE_")))))
              (format out "  test rax, rax~%")
              (format out "  jns ~a~%" lbl-left)
              (format out "  neg rax~%")
              (format out "  xchg rax, rcx~%")
              (format out "  sar rax, cl~%")
              (format out "  jmp ~a~%" lbl-done)
              (format out "~a:~%" lbl-left)
              (format out "  xchg rax, rcx~%")
              (format out "  shl rax, cl~%")
              (format out "~a:~%" lbl-done)))
           (logand (format out "  and rax, rcx~%"))
           (logior (format out "  or rax, rcx~%"))
           (eql
            (format out "  cmp rcx, rax~%")
            (format out "  mov rax, 0~%")
            (format out "  sete al~%"))
           (lt
            (format out "  cmp rcx, rax~%")
            (format out "  mov rax, 0~%")
            (format out "  setl al~%"))
           (gt
            (format out "  cmp rcx, rax~%")
            (format out "  mov rax, 0~%")
            (format out "  setg al~%"))
           (poke
            (format out "  mov [rcx], rax~%"))
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
            (format out "  cqo~%")
            (format out "  idiv r8~%"))
           (fadd (compile-sse out "addsd"))
           (fsub (compile-sse out "subsd"))
           (fmul (compile-sse out "mulsd"))
           (fdiv (compile-sse out "divsd"))))

        ;; tertiary
        ((member op '(poke-idx))
         (format out "~a" (compile-expr (first  args) comp-env nil parent-arity))
         (format out "  push rax~%")
         (format out "~a" (compile-expr (second args) comp-env nil parent-arity))
         (format out "  push rax~%")
         (format out "~a" (compile-expr (third  args) comp-env nil parent-arity))
         (format out "  pop rcx~%")
         (format out "  pop r8~%")
         (format out "  ;; inline ~a~%" op)
         (case op
           (poke-idx (format out "  mov [r8 + rcx*8], rax~%"))))
           
        (t (error "Unsupported primitive: ~a" op))))))

(defun compile-string-literal (expr)
  "Evaluates to the pointer of a statically allocated string list."
  (let ((label (register-string expr)))
    (format nil "  lea rax, [~a]~%" label)))

(defun compile-float-literal (expr)
  (let ((label (track-float expr)))
    ;; Dereference the memory address to load the float into RAX
    (format nil "  mov rax, [~a]~%" label)))

(defun compile-syscall (expr comp-env parent-arity)
  (let ((args (cdr expr))
        (regs '("rax" "rdi" "rsi" "rdx" "r10" "r8" "r9")))
    (when (> (length args) 7)
      (error "Linux syscalls take a maximum of 6 arguments + 1 syscall number"))

    (with-output-to-string (out)
      (format out "  ;; syscall~%")
      (loop for arg in (reverse args) do
            (format out "~a" (compile-expr arg comp-env nil parent-arity))
            (format out "  push rax~%"))

      (loop for i from 0 below (length args) do
            (format out "  pop ~a~%" (nth i regs)))

      (format out "  syscall~%"))))

(defun compile-lookup (expr comp-env)
  (track-symbol expr)
  (let ((binding (assoc expr comp-env)))
    (if binding
        (let ((location (cdr binding)))
          (if (stringp location)
              ;; It's a global label!
              (format nil "  mov rax, [~a]~%" location)
              ;; It's a stack offset!
              (if (> location 0)
                  (format nil "  mov rax, [rbp + ~a]~%" location)
                  (format nil "  mov rax, [rbp - ~a]~%" (- location)))))
        (error "Compile error: Unbound variable '~a'" expr))))

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
           `(,(second expr) (1 ,@(mapcar #'expand-macro (cddr expr)))))

          ;; (cond ...), (let ...) just recursively expand their bodies
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

          (otherwise
           (mapcar #'expand-macro expr))))))

(defun emit-data-section (out)
  "Generates floats, symbols, string literals, and globals in the data segment."
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

  (format out "~%  ;; --- GLOBALS ---~%")
  (loop for label in *global-vars* do
        (format out "~a: dq 0~%" label))

  (format out "~%  ;; Uninitialized heap memory requested from OS~%")
  (format out "heap_start: rb 1024 * 1024 * 8~%~%"))

(defun compile-apply (expr comp-env tail-p parent-arity)
  (let* ((func (car expr))
         (args (cdr expr))
         (num-args (length args))
         (tce-regs '("r8" "r9" "r10" "r11" "r12" "r13")))

    (with-output-to-string (out)
      (format out "  ;; ~a ~a~%"
              (if tail-p "tail-call" "call")
              (if (symbolp func) func "<lambda>"))

      (loop for arg in (reverse args) do
            (format out "~a" (compile-expr arg comp-env nil parent-arity))
            (format out "  push rax~%"))

      (format out "~a" (compile-expr func comp-env nil parent-arity))

      (if tail-p
          (progn
            (when (> num-args 6) (error "TCE stack shuffle restricted to 6 args."))
            (loop for i from 1 to num-args do
                  (format out "  pop ~a~%" (nth (1- i) tce-regs)))
            (format out "  mov rsp, rbp       ; Destroy current frame and env~%")
            (format out "  pop rbp            ; Restore caller RBP~%")
            (format out "  pop rcx            ; Pop return address~%")
            (format out "  add rsp, ~a        ; Clean parent args~%" (* 8 parent-arity))
            (loop for i from num-args downto 1 do
                  (format out "  push ~a~%" (nth (1- i) tce-regs)))
            (format out "  push rcx~%")
            (format out "  jmp rax~%"))
          (format out "  call rax~%")))))

(defun compile-expr (expr comp-env &optional tail-p parent-arity)
  "Translates a single S-expression. Tracks tail position and parent arity for TCE."
  (cond
    ((null     expr) "  mov rax, 0~%")
    ((integerp expr) (format nil "  mov rax, ~a~%" expr))
    ((floatp   expr) (compile-float-literal expr))
    ((stringp  expr) (compile-string-literal expr))
    ((symbolp  expr) (compile-lookup expr comp-env))
    ((consp expr)
     (let ((head (car expr)))
       (cond
         ;; 1. Syscall Special Form
         ((eq head 'syscall)
          (compile-syscall expr comp-env parent-arity))

         ;; 2. If Special Form
         ((eq head 'if)
          (let ((cnd (second expr))
                (thn (third expr))
                (els (fourth expr))
                (els-label (string-left-trim "#:" (symbol-name (gensym "IF_ELS_"))))
                (end-label (string-left-trim "#:" (symbol-name (gensym "IF_END_")))))
            (with-output-to-string (out)
              (format out "  ;; if block~%")
              (format out "~a" (compile-expr cnd comp-env nil parent-arity))
              (format out "  test rax, rax~%")
              (format out "  jz ~a~%" els-label)
              (format out "~a" (compile-expr thn comp-env tail-p parent-arity))
              (format out "  jmp ~a~%" end-label)
              (format out "~a:~%" els-label)
              (format out "~a" (compile-expr els comp-env tail-p parent-arity))
              (format out "~a:~%" end-label))))

         ;; 3. Cond Special Form
         ((eq head 'cond)
          (let ((clauses (cdr expr))
                (end-label (string-left-trim "#:" (symbol-name (gensym "COND_END_")))))
            (with-output-to-string (out)
              (format out "  ;; cond block~%")
              (loop for clause in clauses
                    for next-label = (string-left-trim "#:" (symbol-name (gensym "COND_NEXT_"))) do
                    
                    (if (eql (first clause) 1)
                        (format out "  mov rax, 1~%")
                        (format out "~a" (compile-expr (first clause) comp-env nil parent-arity)))
                        
                    (format out "  test rax, rax~%")
                    (format out "  jz ~a~%" next-label)
                    
                    (let ((body (cdr clause)))
                      (if (null body)
                          (format out "  ;; implicit true return~%")
                          (loop for rest on body
                                for stmt = (car rest)
                                for is-last = (null (cdr rest))
                                ;; FIX: Pass comp-env and handle implicit sequence correctly
                                do (format out "~a" (compile-expr stmt comp-env (and tail-p is-last) parent-arity)))))
                                
                    (format out "  jmp ~a~%" end-label)
                    (format out "~a:~%" next-label))
              (format out "  mov rax, 0~%")
              (format out "~a:~%" end-label))))

         ;; 4. Let Special Form
         ((eq head 'let)
          (let* ((bindings (second expr))
                 (body (cddr expr))
                 (num-bindings (length bindings)))
            (with-output-to-string (out)
              (format out "  ;; let block~%")

              (let* ((current-env comp-env)
                     ;; FIX: Filter out global string labels, keep only numeric stack offsets!
                     (numeric-offsets (remove-if-not #'numberp (mapcar #'cdr comp-env)))
                     (current-offset (if numeric-offsets
                                         (let ((min-off (apply #'min numeric-offsets)))
                                           (if (< min-off 0) (- min-off 8) -8))
                                         -8)))

                ;; 1. Evaluate and push bindings sequentially
                (loop for binding in bindings
                      for var = (first binding)
                      for val = (second binding) do
                      (track-symbol var)

                      (format out "~a" (compile-expr val current-env nil parent-arity))
                      (format out "  push rax ; Bind ~a~%" var)

                      (push (cons var current-offset) current-env)
                      (decf current-offset 8))

                ;; 2. Evaluate the body with the fully augmented environment!
                (loop for rest on body
                      for stmt = (car rest)
                      for is-last = (null (cdr rest)) do
                      (format out "~a" (compile-expr stmt current-env (and tail-p is-last) parent-arity)))

                ;; 3. Cleanup the stack
                (format out "  add rsp, ~a ; Pop ~a let bindings~%"
                        (* 8 num-bindings) num-bindings)))))

         ;; 5. Lambda Special Form
         ((eq head 'lambda)
          (compile-lambda `(,(second expr) (1 ,@(cddr expr))) comp-env))

         ;; 6. Implicit Lambda
         ((implicit-lambda-p expr)
          (compile-lambda expr comp-env))

         ;; 7. Inline Primitives
         ((and (symbolp head) (member head *primitives*))
          (compile-inline-primitive expr comp-env parent-arity))

         ;; 8. Standard Function Application
         (t
          (compile-apply expr comp-env tail-p parent-arity)))))

    (t (error "Compiler error: Unrecognized expression type: ~a" expr))))

(defvar *global-vars* nil "List of labels to allocate in .bss")

(defun compile-global-let (bindings body)
  (let ((global-env nil))
    (with-output-to-string (out)
      (format out "  ;; --- INITIALIZE GLOBALS ---~%")

      ;; PASS 1: Forward Declarations! (Register every label first)
      (loop for binding in bindings
            for var = (first binding)
            for label = (format nil "global_~a" (fasm-label var)) do
            (push label *global-vars*)
            (push (cons var label) global-env))

      ;; PASS 2: Compile the values using the fully populated environment!
      (loop for binding in bindings
            for val = (second binding)
            for label = (format nil "global_~a" (fasm-label (first binding))) do
            (format out "~a" (compile-expr val global-env nil 0))
            (format out "  mov [~a], rax~%" label))

      (format out "  ;; --- MAIN EXECUTION ---~%")
      (loop for stmt in body do
            (format out "~a" (compile-expr stmt global-env nil 0))))))

(defun compile-program (ast filepath)
  (let ((*used-symbols* nil)
        (*used-floats* nil)
        (*hoisted-functions* nil) ; Reset state
        (*global-vars* nil))

    (with-open-file (out filepath :direction :output :if-exists :supersede)
      (format out "format ELF64 executable 3~%")

      ;; --- 1. CODE SECTION ---
      (format out "segment readable executable~%")
      (format out "include 'runtime.asm'~%~%")

      (format out "entry _start~%")
      (format out "_start:~%")
      (format out "  lea r15, [heap_start]~%~%")

      ;; Iterate through all top-level nodes (implicit sequence)
      (loop for raw-expr in ast for expanded = (expand-macro raw-expr) do

        ;; Intercept top-level 'let' blocks and treat them as global envs!
        (if (and (consp expanded) (eq (car expanded) 'let))
            (format out "~a" (compile-global-let (second expanded)
                                                 (cddr expanded)))

            ;; Otherwise, compile it as a normal top-level expression
            (format out "~a~%" (compile-expr expanded nil))))

      ;; System Exit
      (format out "  mov rdi, rax~%")
      (format out "  mov rax, 60~%")
      (format out "  syscall~%~%")
      
      ;; Emit hoisted lambdas AFTER exit, keeping execution flow clean!
      (format out "  ;; --- COMPILED FUNCTIONS ---~%")
      (loop for func-asm in (reverse *hoisted-functions*) do
            (format out "~a~%" func-asm))
      
      ;; --- 2. DATA SECTION ---
      (emit-data-section out))))

(defun build-program (input-filepath output-filepath)
  "Reads a Lisp source file and compiles it into a FASM executable."
  (let ((ast (with-open-file (in input-filepath :direction :input)
                (loop for expr = (read in nil :eof)
                      until (eq expr :eof)
                      collect expr))))
    ;; Pass the raw list of expressions directly! No more wrapping in begin!
    (compile-program ast output-filepath)))

(build-program "boot.lisp" "boot.fasm")
