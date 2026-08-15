# Bare-Metal Lisp (BML)

![Project Logo](./assets/logo.svg)

A completely self-hosting, lexically-scoped Lisp compiler that targets raw x86-64 Linux assembly. Built from scratch and iteratively upgraded into a high-performance systems language.

Bare-Metal Lisp bypasses traditional C dependencies, LLVM IRs, and virtual machines. It is a streamlined compiler that reads Abstract Syntax Trees (AST), resolves a Turing-complete macro expansion pass, and instantly emits native `ELF64` FASM instructions. 

[👉 Read the development journey up until singularity and AI pairing chat here] *(https://share.gemini.google/DSdvvf4BaXrP)*

[👉 Shortening symbol names and JIT idea discussion] *(https://share.gemini.google/7wLaCL0Rq7RC)*

[👉 Read the JIT-Architecture and Macro Engine overhaul chat here] *(https://share.gemini.google/JINv4il7VjK6)*

## Core Philosophy & Architectural Features

### 1. The Untyped Singularity & Hardware Mapping
There are no software data types. The hardware is the type system. Every variable, pointer, and literal is strictly a raw 64-bit integer, with native support for x86-64 SSE registers for floating-point operations.
Because the architecture is untyped, logical concepts are mathematically unified: `0`, `()`, `nil`, and `false` are literally just the 64-bit integer `0`. The compiler translates operations directly into bare-metal x86-64 hardware instructions (`add` becomes `add rax, rcx`, `car` becomes `mov rax, [rax]`). Dynamic composition of `c[ad]+r` functions (like `caddr`) is recursively compiled into inline memory dereferences.

### 2. Lexical Scoping & C ABI Compatibility
We matured past dynamic environment lookups. Variables are resolved lexically at compile-time and mapped directly to hardware stack frame offsets (`rbp - X` for locals, `rbp + X` for arguments).
Furthermore, functions strictly adhere to the System V AMD64 ABI. Function arguments are passed via hardware registers (`rdi`, `rsi`, `rdx`, `rcx`, `r8`, `r9`) before spilling to the stack, making BML natively interoperable with C libraries and Linux kernel syscalls.

### 3. Hardware Tail-Call Elimination (TCE)
BML guarantees zero-overhead recursion. If a function call is in the tail position, the compiler destroys the current stack frame and emits a raw hardware `jmp rax` instead of a `call`. This allows infinite state-machine loops and recursive functions (like `sumto`) without ever blowing out the call stack.

### 4. Turing-Complete AST Rewriter (Macros)
The compiler features a fully isolated, pure AST rewriter pass. Using `defmacro`, developers can write compile-time metaprograms. The internal compile-time interpreter (`evalast` / `mini-eval`) is Turing-complete, supporting recursive macro expansion, compile-time arithmetic, and local `let` bindings. 

### 5. Zero-Overhead Scoped Memory Arenas
Memory is managed via a blisteringly fast bump allocator on register `r15`. Using the `with` macro, developers can create scoped memory arenas that capture the `r15` pointer, execute a block of heavily allocating code, and instantly restore the pointer upon exit, freeing all memory in `O(1)` time.

### 6. Fail-Fast Compilation
The code generator strictly tracks global environments and local stack frames. Referencing an unbound variable immediately halts compilation with a zero-overhead compile-time error, preventing illegal instructions or segfaults in the resulting executable.

---

## Language Reference

### Memory Model
*   **Heap:** A statically allocated 8MB uninitialized heap (`rb 1024 * 1024 * 8`).
*   **Cons Cells:** 16-byte pairs. The `car` is at `[ptr]`, the `cdr` is at `[ptr+8]`.
*   **Data Segments:** Strings, symbols, and global variables are cleanly hoisted and emitted into a standard `segment readable writeable` ELF64 data block.

### Syntax & Special Forms
*   `(...)` - List definition / Function application
*   `"..."` - String literal (allocates a contiguous byte-array in the data segment)
*   `; ...` - Single-line comment
*   `(let ((var1 val1) (var2 val2)) body)` - Sequential stack allocation.
*   `(cond (cnd1 thn1) (1 els))` - Native branching. Translates to efficient `test rax, rax` and `jz` jump chains.
*   `(if condition then else)` - Syntactic sugar for `cond`.
*   `(lambda (args) body)` - Anonymous function definition. Implicitly treats the body as a sequence.
*   `(defmacro (name args) body)` - Compile-time AST manipulation.
*   `(with expr)` - Scoped memory arena execution.

### Hardware Primitives
The following functions map perfectly to their respective x86-64 assembly instructions or runtime syscalls:

**Memory / Pointers:**
*   `(cons a b)` - Allocates 16 bytes on the `r15` heap and returns the pointer.
*   `(car lst)` / `(peek ptr)` - Dereferences memory: `mov rax, [rax]`.
*   `(cdr lst)` - Dereferences memory + offset: `mov rax, [rax+8]`.
*   `(poke ptr val)` - Mutates memory at pointer: `mov [rax], rcx`.
*   `(pokeidx ptr idx val)` / `(peekidx ptr idx)` - Contiguous array access.

**Math & Logic (64-bit Integer):**
*   `(add a b)`, `(sub a b)`, `(mul a b)`, `(div a b)`.
*   `(and a b)`, `(or a b)`, `(logand a b)`, `(logior a b)` - Pure inline bitwise logic.
*   `(ash val count)` - Arithmetic shift left/right (`shl`, `sar`).
*   `(eql a b)`, `(lt a b)`, `(gt a b)`, `(le a b)`, `(ge a b)` - Evaluates to `1` or `0`.

**Floating Point (SSE):**
*   `(fadd a b)`, `(fsub a b)`, `(fmul a b)`, `(fdiv a b)` - Maps to `addsd`, `subsd`, etc.
*   `(itof a)` - Converts integer to float (`cvtsi2sd`).

**I/O (Syscalls):**
*   `(syscall num arg1 arg2 ...)` - Native Linux CFFI bridge. Maps up to 6 arguments to registers, automatically handling the `rcx` to `r10` translation required by the Linux kernel.
*   `(getchar)` - Reads a single ASCII character from `stdin`.
*   `(putchar c)`, `(putint n)`, `(puthex n)` - Standard output streams.

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
Pipe the compiler's standard library and source code into its own compiled binary. It will compile itself without the host language:
```bash
cat stdlib.lisp boot.lisp | ./boot > boot2.fasm
fasm boot2.fasm
```

**3. Validation (Stage 3 & Test Suite)**
Prove that the compiler is perfectly self-hosted by compiling it again using Stage 2. The output must be identical:
```bash
cat stdlib.lisp boot.lisp | ./boot2 > boot3.fasm
diff boot2.fasm boot3.fasm
```

Execute the full regression test suite to validate macro expansions, TCE, and hardware logic:
```bash
./test.sh
```
