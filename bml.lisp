(defparameter *primitives*
  '( add  sub  mul  div   ; Integer math
    fadd fsub fmul fdiv   ; Float math (SSE)
    cons                  ; List/Env ops
    putint puthex putchar putchunk
    peek poke
    alloc peekidx pokeidx
    eql lt gt le ge
    getchar itof
    and or
    ash logand logior not
    getheap setheap pokebyte dict)
  "Standard assembly mnemonics mapping 1-to-1 with hardware instructions.")

(defvar *used-symbols* nil)
(defvar *symbols* nil "Alist mapping symbol names (strings) to FASM labels.")
(defvar *strings* nil "Alist mapping string literals to FASM labels.")
(defvar *used-floats* nil "Alist mapping float values to FASM labels.")
(defvar *label-counter* 0 "Counter for generating safe labels.")
(defvar *global-vars* nil "List of labels to allocate in .bss")
(defvar *global-env* nil "Alist mapping global variables to labels.")
(defvar *hoisted-functions* nil "List of raw assembly strings for compiled lambdas.")
(defvar *macenv* nil "Alist mapping user-defined macros to their AST definitions.")

(defun track-float (val)
  "Registers a float and returns its safe FASM label."
  (let ((existing (assoc val *used-floats* :test #'=)))
    (if existing
        (cdr existing)
        (let ((label (string-left-trim "#:" (symbol-name (gensym "FLT_")))))
          (push (cons val label) *used-floats*)
          label))))

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

(defun compile-inline-primitive (expr comp-env parent-arity)
  (let* ((op (car expr))
         (args (cdr expr)))
    (with-output-to-string (out)
      (flet ((emit-ir (name)
               (format out "  ir_~a~%" (string-downcase (symbol-name name)))
               t))
        (cond
          ;; nullary
          ((member op '(getchar getheap dict))
           (emit-ir op))

          ;; unary
          ((member op '(putint puthex putchar putchunk peek itof alloc setheap not))
           (format out "~a" (compile-expr (first args) comp-env nil parent-arity))
           (emit-ir op))

          ;; binary variadic
          ((member op '(add sub mul div fadd fsub fmul fdiv logior or logand and))
           (if (null args)
               (if (member op '(mul fmul)) (format out "  mov rax, 1~%") (format out "  mov rax, 0~%"))
               (progn
                 (format out "~a" (compile-expr (first args) comp-env nil parent-arity))
                 (loop for arg2 in (cdr args) do
                       (format out "  push rax~%")
                       (format out "~a" (compile-expr arg2 comp-env nil parent-arity))
                       (format out "  ir_set_arg2~%") ;; Use our new stack-swap macro!
                       (if (member op '(logior or logand and))
                           (emit-ir (if (member op '(logior or)) 'or 'and))
                           (emit-ir op))))))

          ;; binary strict
          ((member op '(cons poke eql lt gt le ge ash peekidx pokebyte))
           (format out "~a" (compile-expr (first args) comp-env nil parent-arity))
           (format out "  push rax~%")
           (format out "~a" (compile-expr (second args) comp-env nil parent-arity))
           (format out "  ir_set_arg2~%")
           (emit-ir op))

          ;; tertiary
          ((member op '(pokeidx))
           (format out "~a" (compile-expr (first args) comp-env nil parent-arity))
           (format out "  push rax~%")
           (format out "~a" (compile-expr (second args) comp-env nil parent-arity))
           (format out "  push rax~%")
           (format out "~a" (compile-expr (third args) comp-env nil parent-arity))
           (format out "  pop rcx~%")
           (format out "  ir_pop_r8~%")
           (emit-ir op))

          (t (error "Unsupported primitive: ~a" op)))))))

(defun compile-cadr (expr comp-env parent-arity)
  "Dynamically chains macro calls for any valid c*r sequence."
  (let* ((op (car expr))
         (arg (second expr))
         (s (string-downcase (symbol-name op))))
    (with-output-to-string (out)
      (format out "~a" (compile-expr arg comp-env nil parent-arity))
      (format out "  ;; inline ~a~%" op)
      (loop for i from (- (length s) 2) downto 1
            for c = (char s i)
            do (case c
                 (#\a (format out "  ir_car~%"))
                 (#\d (format out "  ir_cdr~%")))))))

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

(defun parse-escapes (str)
  "Translates raw escape sequences like '\\n' into actual byte values."
  (let ((bytes nil)
        (i 0)
        (len (length str)))
    (loop while (< i len) do
          (let ((c (char str i)))
            (if (and (char= c #\\) (< (1+ i) len))
                (let ((next-c (char str (1+ i))))
                  (case next-c
                    (#\n (push 10 bytes))
                    (#\t (push 9 bytes))
                    (#\r (push 13 bytes))
                    (#\\ (push 92 bytes))
                    (#\" (push 34 bytes))
                    (t   (push (char-code c) bytes)
                         (push (char-code next-c) bytes)))
                  (incf i 2))
                (progn
                  (push (char-code c) bytes)
                  (incf i 1)))))
    (reverse bytes)))

(defun emit-contiguous-data (out label str &key parse-escapes)
  (let* ((bytes (if parse-escapes (parse-escapes str)
                    (loop for i from 0 below (length str) collect (char-code (char str i)))))
         (len (length bytes)))
    (format out "align 8~%")
    (format out "~a: dq ~a_data, ~a~%" label label len)
    (format out "~a_data:~%" label)
    (when (> len 0)
      (format out "  db ")
      (let ((state 0)) ; 0=start, 1=in-str, 2=out-str
        (loop for byte in bytes do
              (let ((is-printable (and (>= byte 32) (<= byte 126) (/= byte 34))))
                (if is-printable
                    (case state
                      (0 (format out "\"~a" (code-char byte)) (setf state 1))
                      (1 (format out "~a" (code-char byte)))
                      (2 (format out ", \"~a" (code-char byte)) (setf state 1)))
                    (case state
                      (0 (format out "~a" byte) (setf state 2))
                      (1 (format out "\", ~a" byte) (setf state 2))
                      (2 (format out ", ~a" byte))))))
        (when (= state 1)
          (format out "\"")))
      (format out "~%"))))

(defun mini-eval (ast env)
  "A compile-time interpreter to execute macro definitions."
  (cond
    ((null ast) nil)
    ((integerp ast) ast)
    ((stringp ast) ast)
    ((symbolp ast)
     (let ((b (assoc ast env)))
       (if b (cdr b) (error "mini-eval: unbound variable ~a" ast))))
    ((consp ast)
     (let ((func (car ast))
           (args (cdr ast)))
       (case func
         (quote (car args))

         ;; Translate 0 to nil for safe traversal
         (car (let ((lst (mini-eval (car args) env))) (if (null lst) 0 (car lst))))
         (cdr (let ((lst (mini-eval (car args) env))) (if (null lst) 0 (cdr lst))))

         (cons
          (let ((a (mini-eval (car args) env))
                (d (mini-eval (second args) env)))
            ;; MAGIC TRICK: If the cdr is 0, make it nil so it forms a proper CL list!
            (cons a (if (eql d 0) nil d))))

         (eql (if (eql (mini-eval (car args) env) (mini-eval (second args) env)) 1 0))
         (if (if (not (eql 0 (mini-eval (car args) env)))
                 (mini-eval (second args) env)
                 (mini-eval (third args) env)))
         (add (+ (mini-eval (car args) env) (mini-eval (second args) env)))
         (sub (- (mini-eval (car args) env) (mini-eval (second args) env)))
         (mul (* (mini-eval (car args) env) (mini-eval (second args) env)))
         (div (truncate (mini-eval (car args) env) (mini-eval (second args) env)))
         (ash (ash (mini-eval (car args) env) (mini-eval (second args) env)))
         (let (let* ((bindings (car args))
                     (new-env (copy-alist env)))
                (dolist (b bindings)
                  (push (cons (first b) (mini-eval (second b) new-env)) new-env))
                (mini-eval (second args) new-env)))
         (otherwise (error "mini-eval unsupported function ~a" func)))))
    (t ast)))

(defun expand-macro-list (lst)
  "Recursively expands a list, gracefully dropping omitted nodes (like defmacro)."
  (loop for item in lst
        for expanded = (expand-macro item)
        when expanded
          collect expanded))

(defun expand-macro (expr)
  "Expands syntactic sugar and intercepts macro definitions."
  (if (not (consp expr))
      expr
      (let* ((head (car expr))
             (mdef (assoc head *macenv*)))
        (if mdef
            ;; 1. User-Defined Macro! Evaluate it, then recursively expand the result.
            (let* ((params (second mdef))
                   (body (third mdef))
                   (env (pairlis params (cdr expr))))
              (expand-macro (mini-eval body env)))

            ;; 2. Standard Syntactic Sugar & Special Forms
            (case head
              (le `(if (gt ,(expand-macro (second expr)) ,(expand-macro (third expr))) 0 1))
              (ge `(if (lt ,(expand-macro (second expr)) ,(expand-macro (third expr))) 0 1))
              (if `(cond (,(expand-macro (second expr)) ,(expand-macro (third expr)))
                         (1 ,(expand-macro (fourth expr)))))
              (defun
               (let ((name (second expr))
                     (args (third expr))
                     (body (cdddr expr)))
                 (expand-macro `(let ((,name (lambda ,args ,@body)))))))
              (lambda
               `(,(second expr) (1 ,@(expand-macro-list (cddr expr)))))
              (cond `(cond ,@(mapcar (lambda (c) (expand-macro-list c)) (cdr expr))))

              (let
               (let ((bindings (second expr))
                     (body (cddr expr))
                     (runtime-bnds nil))
                 ;; Scan bindings: If it's a macro, save to macenv. Otherwise, keep it for runtime.
                 (dolist (b bindings)
                   (let ((sym (first b))
                         (val (second b)))
                     (if (and (consp val) (eq (first val) 'macro))
                         (push (list sym (second val) (third val)) *macenv*)
                         (let ((exp-val (expand-macro val)))
                           (when exp-val
                             (push (list sym exp-val) runtime-bnds))))))
                 `(let ,(reverse runtime-bnds)
                    ,@(expand-macro-list body))))

              (quote expr) ;; Do NOT expand inside quotes!

              ;; 3. Standard calls
              (otherwise (expand-macro-list expr)))))))

(defun emit-data-section (out)
  "Generates floats, symbols, string literals, and globals in the data segment."
  (format out "segment readable writeable~%~%")

  (format out "  ;; --- FLOATS ---~%")
  (loop for (val . label) in *used-floats* do
        (format out "~a: dq ~f~%" label val))

  (format out "~%  ;; --- SYMBOLS ---~%")
  (loop for sym in *used-symbols*
        for str = (symbol-name sym)
        for safe-name = (format nil "sym_~a" (symbol-name sym))
        do (emit-contiguous-data out safe-name str :parse-escapes nil))

  (format out "~%  ;; --- STRINGS ---~%")
  (loop for (str . label) in *strings* do
        (emit-contiguous-data out label str :parse-escapes t))

  (format out "~%  ;; --- GLOBALS ---~%")
  (loop for label in *global-vars* do
        (format out "align 8~%")
        (format out "~a: dq 0~%" label))

  ;; (format out "~%  ;; --- DICTIONARY ---~%")
  ;; (format out "align 8~%")
  ;; (format out "global_dict: file 'dictionary.bin'~%")

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

(defun is-cadr-sym (sym)
  "Checks if a symbol matches the c[ad]+r pattern."
  (let* ((s (string-downcase (symbol-name sym)))
         (len (length s)))
    (and (>= len 3)
         (char= (char s 0) #\c)
         (char= (char s (1- len)) #\r)
         (loop for i from 1 to (- len 2)
               always (let ((c (char s i)))
                        (or (char= c #\a) (char= c #\d)))))))

(defun pack-symbol (sym)
  "Packs up to 8 characters of a symbol's name into a 64-bit integer,
   matching the self-hosted compiler's symbol representation."
  (let* ((name (symbol-name sym))
         ;; Common Lisp reads symbols as uppercase by default,
         ;; so we downcase it to match the self-hosted Lisp's behavior.
         (name-lower (string-downcase name))
         (acc 0))
    (loop for i from 0 below (min (length name-lower) 8)
          for char = (char name-lower i)
          for code = (char-code char)
          do (setf acc (logior acc (ash code (* i 8)))))
    acc))

(defun compile-expr (expr comp-env &optional tail-p parent-arity)
  "Translates a single S-expression. Tracks tail position and parent arity for TCE."
  (cond
    ((null     expr) "  mov rax, 0~%")
    ((integerp expr) (format nil "  mov rax, ~a~%" expr))
    ((floatp   expr) (compile-float-literal expr))
    ((stringp  expr) (compile-string-literal expr))
    ((symbolp  expr) (compile-lookup expr comp-env))
    ((consp    expr)
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

         ;; 8. Dynamic c[ad]+r
         ((and (symbolp head) (is-cadr-sym head))
          (compile-cadr expr comp-env parent-arity))

         ;; 9. quote
         ((eq head 'quote)
          (let ((arg (cadr expr)))
            (if (symbolp arg)
                (format nil "  mov rax, ~a~%" (pack-symbol arg))
                (compile-expr arg comp-env)))) ; If it's a number/string, just compile it normally

         ;; 10. Standard Function Application
         (t
          (compile-apply expr comp-env tail-p parent-arity)))))

    (t (error "Compiler error: Unrecognized expression type: ~a" expr))))

(defun compile-global-let (bindings body)
  (with-output-to-string (out)
    (format out "  ;; --- INITIALIZE GLOBALS ---~%")

    ;; PASS 1: Forward Declarations! (Register every label first)
    (loop for binding in bindings
          for var = (first binding)
          for label = (format nil "global_~a" (symbol-name var)) do
          (push label *global-vars*)
          (push (cons var label) *global-env*)) ; <-- Mutates the persistent environment!

    ;; PASS 2: Compile the values using the fully populated environment!
    (loop for binding in bindings
          for val = (second binding)
          for label = (format nil "global_~a" (symbol-name (first binding))) do
          (format out "~a" (compile-expr val *global-env* nil 0))
          (format out "  mov [~a], rax~%" label))

    (format out "  ;; --- MAIN EXECUTION ---~%")
    (loop for stmt in body do
          (format out "~a" (compile-expr stmt *global-env* nil 0)))))

(defun compile-program (ast filepath)
  (let ((*used-symbols* nil)
        (*used-floats* nil)
        (*hoisted-functions* nil)
        (*global-vars* nil)
        (*global-env* nil)
        (*macenv* nil))

    (with-open-file (out filepath :direction :output :if-exists :supersede)
      (format out "format ELF64 executable 3~%")
      (format out "segment readable executable~%")
      (format out "include 'ir_macros.asm'~%~%")
      (format out "entry _start~%")
      (format out "_start:~%")
      (format out "  push rbp~%")
      (format out "  mov rbp, rsp~%")
      (format out "  lea r15, [heap_start]~%~%")

      (loop for raw-expr in ast
          for expanded = (expand-macro raw-expr) do ;; <-- Fixed: expand-macro
      (when expanded
        (if (and (consp expanded) (eq (car expanded) 'let))
            (format out "~a" (compile-global-let (second expanded) (cddr expanded)))
            (format out "~a~%" (compile-expr expanded *global-env*)))))

      ;; System Exit
      (format out "  mov rdi, rax~%")
      (format out "  mov rax, 60~%")
      (format out "  syscall~%~%")

      ;; Emit hoisted lambdas AFTER exit
      (format out "  ;; --- COMPILED FUNCTIONS ---~%")
      (loop for func-asm in (reverse *hoisted-functions*) do
            (format out "~a~%" func-asm))

      (emit-data-section out))))

(defun build-program (input-filepaths output-filepath)
  "Reads multiple Lisp source files and compiles them into a single FASM executable."
  (let ((ast (loop for filepath in input-filepaths
                   append (with-open-file (in filepath :direction :input)
                            (loop for expr = (read in nil :eof)
                                  until (eq expr :eof)
                                  collect expr)))))
    (compile-program ast output-filepath)))

;; Feed both files to build step 1
(build-program '("stdlib.lisp" "boot.lisp") "boot.fasm")
