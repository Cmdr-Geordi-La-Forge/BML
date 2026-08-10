# Bare-Metal Lisp (BML)

![Project Logo](./assets/logo.svg)

A completely self-hosting, dynamically-scoped Lisp compiler that targets raw x86-64 Linux assembly. Built from scratch in ~18 hours.

Bare-Metal Lisp bypasses traditional C dependencies, LLVM IRs, and virtual machines. It is a single-pass compiler that reads Abstract Syntax Trees (AST) and instantly emits native `ELF64` FASM instructions. It features an untyped memory model where data, code, pointers, and booleans are mathematically unified.

[👉 Read the full 18-hour development journey and AI pairing chat here] *(https://share.gemini.google/DSdvvf4BaXrP)*

## Core Philosophy & Features

### 1. The Untyped Singularity (1:1 Hardware Translation)
There are no software data types. The hardware is the type system. Every variable, pointer, and literal is strictly a raw 64-bit integer.
Because the architecture is untyped, logical concepts are mathematically unified: `0`, `()`, `nil`, and `false` are literally just the 64-bit integer `0`. The compiler translates operations into bare-metal x86-64 hardware instructions (`add` becomes `add rax, rcx`, `car` becomes `mov rax, [rax]`).

### 2. Multi-Level Homoiconicity
Lisp's famous "code is data" philosophy is taken to the absolute extreme:
* The AST is the Control Flow: There is no separate main or global state. The entire program is just one giant function application.
* The Syntax is the Data: Homoiconicity at its finest. The Lisp lists you write map directly to the execution structure.
* The Environment is the Call Stack: By moving dynamic bindings off the heap and directly into the RBP frame, local variables become zero-cost abstractions that clean themselves up instantly.
* Strings: There is no standard string type. Strings and symbols are stored as linked lists of 8-byte chunks (64-bit integers), seamlessly fitting into the exact same memory model as the AST. 

### 3. Dynamic Scoping
Variables are resolved dynamically on the active call stack at runtime. There are no lexical closures or complex environment captures. If a function needs a variable, it asks the CPU to walk backwards down the execution stack until it finds it. This elegantly permits mutual recursion without forward declarations.

### 4. Single-Pass Compilation & Inline Data
The bootstrapped compiler evaluates and compiles simultaneously. To handle strings and variables without a multi-pass data collection step, the compiler uses an **Inline Data Jump** hack. It injects memory chunks directly into the execution flow and instructs the CPU to physically jump over them:
```fasm
  jmp SYM_AFTER_1
SYM_1_0:
  dq 8026308904485020012
  dq 0
SYM_AFTER_1:
  lea rdi, [SYM_1_0]
```

### 5. Perfect Bootstrapping Stabilization
The compiler is proven to be perfectly stable via the Stage 3 Bootstrapping validation. 
`boot2.fasm` (Stage 2, compiled by the host environment) and `boot3.fasm` (Stage 3, compiled by Stage 2 itself) produce a 100% empty `diff`.

---

## Language Reference

### Memory Model
*   **Heap:** A statically allocated 8MB uninitialized heap (`rb 1024 * 1024 * 8`).
*   **Cons Cells:** 16-byte pairs. The `car` is at `[ptr]`, the `cdr` is at `[ptr+8]`.
*   **Stack:** Pure x86-64 hardware stack, used for dynamic variable environment (`push r14`).

### Syntax & Special Forms
*   `(...)` - List definition / Function application
*   `"..."` - String literal (compiles to a linked list of 8-byte chunks)
*   `; ...` - Single-line comment
*   `(let ((var1 val1) (var2 val2)) body)` - Sequential stack allocation.
*   `(if condition then else)` - Native branching using `test rax, rax` and `jz`.
*   `(begin stmt1 stmt2 ...)` - Sequential evaluation.
*   `(lambda (arg1 arg2) body)` - Anonymous function definition.

### Hardware Primitives
The following functions map perfectly to their respective x86-64 assembly instructions or runtime syscalls:

**Memory / Pointers:**
*   `(cons a b)` - Allocates 16 bytes on the heap and returns the pointer.
*   `(car lst)` / `(peek ptr)` - Dereferences memory: `mov rax, [rax]`
*   `(cdr lst)` - Dereferences memory + offset: `mov rax, [rax+8]`
*   `(poke ptr val)` - Mutates memory at pointer: `mov [rcx], rax`

**Math (64-bit Integer):**
*   `(add a b)`, `(sub a b)`, `(mul a b)`, `(div a b)`

**Logical / Relational:**
*   `(and a b)` / `(or a b)` - Pure inline bitwise logic (`and rax, rcx`, `or rax, rcx`) used for fast short-circuiting.
*   `(logior a b)` - Bitwise OR.
*   `(ash val count)` - Arithmetic shift left/right.
*   `(eql a b)`, `(lt a b)`, `(gt a b)`, `(le a b)`, `(ge a b)` - Evaluates to `1` or `0`.

**I/O (Syscalls):**
*   `(read-char)` - Reads a single ASCII character from `stdin`.
*   `(print-char c)` - Prints an ASCII character to `stdout`.
*   `(print-int n)` - Prints a base-10 integer to `stdout`.
*   `(print-chunk c)` - Prints a raw 8-byte integer chunk as ASCII text.

---

## How to Bootstrap

To replicate the singularity event from scratch, you need `fasm` and a Common Lisp interpreter (like `sbcl`) for the initial Stage 1 build.

**1. Build the Stage 1 Compiler**
Use the CL host to generate the first native binary:
```bash
sbcl --script bml.lisp
fasm boot.fasm
```

**2. The Singularity (Stage 2)**
Pipe the compiler's source code into its own compiled binary. It will compile itself without the host language:
```bash
cat boot.lisp | ./boot > boot2.fasm
fasm boot2.fasm
```

**3. Validation (Stage 3)**
Prove that the compiler is perfectly self-hosted by compiling it again using Stage 2. The output must be identical:
```bash
cat boot.lisp | ./boot2 > boot3.fasm
diff boot2.fasm boot3.fasm
```
