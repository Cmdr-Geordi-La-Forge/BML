    .include "inc/vm.inc"
    .arch armv8-a
    .data
str_prompt: .asciz "\n=> "
str_bye:    .asciz "\nGoodbye!\n"

    .text
    .global _start

    .equ SYS_MMAP, 222
    .equ SYS_EXIT, 93
    .equ PROT_RWX, 7       
    .equ MAP_ANON_PRIV, 34 
    .equ ARENA_SIZE, 0x00100000 
    .equ HALF_ARENA, 0x00080000 

_start:
    // 1. Allocate & Setup Pivot
    mov x8, #SYS_MMAP
    mov x0, #0
    mov x1, #ARENA_SIZE
    mov x2, #PROT_RWX
    mov x3, #MAP_ANON_PRIV
    mov x4, #-1
    mov x5, #0
    svc #0

    mov x9, #HALF_ARENA
    add x27, x0, x9            // NIL_CAR
    add x28, x27, #8           // NIL_CDR

    // Put JIT_HEAD at NIL_CAR + 256KB (offset 0x40000)
    add JIT_HEAD, x27, #0x40000

    // 2. Tag Native Primitives (TAG_FUNC)
    ldr x0, =env_root
    ldr x1, =env_prims_end
    mov x2, #0x04              // TAG_FUNC
    lsl x2, x2, #56
tag_func_loop:
    cmp x0, x1
    b.eq tag_func_done
    ldr x3, [x0, #8]
    orr x3, x3, x2
    str x3, [x0, #8]
    add x0, x0, #16
    b tag_func_loop
tag_func_done:

    // 3. Tag Macros (TAG_MACRO)
    ldr x0, =env_macros
    ldr x1, =env_macros_end
    mov x2, #TAG_MACRO
    lsl x2, x2, #56
tag_macro_loop:
    cmp x0, x1
    b.eq tag_macro_done
    ldr x3, [x0, #8]
    orr x3, x3, x2
    str x3, [x0, #8]
    add x0, x0, #16
    b tag_macro_loop
tag_macro_done:

    // --- THE REPL LOOP ---
repl_loop:
    // 1. The Ultimate Garbage Collector!
    // Instantly reclaim 100% of memory from the last evaluation.
    mov AST_OFF, #0
    mov DAT_OFF, #0
    add JIT_HEAD, x27, #0x40000 // Reset JIT compiler head back to 256KB offset

    // 2. Print Prompt
    ldr x0, =str_prompt
    bl print_str

    // 3. Read Input from Stdin
    sub sp, sp, #256            // Allocate 256 byte buffer on the stack
    mov x0, #STDIN
    mov x1, sp                  // Buffer pointer
    mov x2, #255                // Max bytes to read
    mov x8, #SYS_READ
    svc #0

    // Check for EOF (Ctrl+D) or Error
    cmp x0, #0
    b.le exit_repl

    // Null-terminate the string by replacing the '\n'
    sub x0, x0, #1
    strb wzr, [sp, x0]

    // 4. Parse 
    mov x0, sp
    bl parse
    mov x19, x0                 // Save tagged AST

    // 5. Evaluate
    ldr x1, =env_root
    bl eval

    // 6. Print Result
    bl print_node

    add sp, sp, #256            // Clean up read buffer
    b repl_loop

exit_repl:
    ldr x0, =str_bye
    bl print_str
    mov x0, #0
    mov x8, #SYS_EXIT
    svc #0
