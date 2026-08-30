.include "inc/vm.inc"
.arch armv8-a
.text

.global eval

// --- eval ---
// Input: 
//   x0 = Tagged AST Node (Expression)
//   x1 = Environment Pointer (e.g., env_root)
// Output:
//   x0 = Evaluated Result
eval:
    // Extract Tag (Bits 56-63)
    lsr x9, x0, #56

    cmp x9, #TAG_INT
    b.eq eval_done      // Integers evaluate to themselves

    cmp x9, #TAG_SYM
    b.eq eval_sym       // Look up symbol in environment

    cmp x9, #TAG_LIST
    b.eq eval_list      // Function / Macro application

eval_done:
    ret


// --- eval_sym ---
eval_sym:
    // We branch here from eval, so we need to save the link register 
    // because we make a 'bl' call to strcmp inside the loop.
    stp x29, x30, [sp, #-16]!

    and x2, x0, #0x00FFFFFFFFFFFFFF // untag symbol
    
    mov x3, x1          // x3 = Current Environment traversal pointer

sym_loop:
    ldr x4, [x3, #0]    // Load symbol string pointer from env
    cbz x4, sym_not_found // 0 means end of environment list

    cmp x4, #1
    b.eq follow_chain   // 1 means "Chain Link to Parent Environment!"

    // Setup for strcmp: x5 = str1, x6 = str2
    mov x5, x2
    mov x6, x4
    
    // Save volatile registers before calling our internal strcmp
    stp x2, x3, [sp, #-16]!
    bl strcmp
    mov x7, x0          // x7 = result (0 if match)
    ldp x2, x3, [sp], #16

    cbz x7, sym_found

    add x3, x3, #16     // Advance to next environment entry (16 bytes)
    b sym_loop

follow_chain:
    ldr x3, [x3, #8]    // Load Parent Environment Pointer
    b sym_loop          // Continue searching in parent!

sym_found:
    ldr x0, [x3, #8]    // Load the bound value!
    ldp x29, x30, [sp], #16
    ret

sym_not_found:
    // Return NIL (0 tagged as list) if symbol not found
    mov x0, #(TAG_LIST << 56)
    ldp x29, x30, [sp], #16
    ret


// --- strcmp ---
// Internal helper: compares strings in x5 and x6
// Returns: x0 = 0 if equal, 1 if not.
strcmp:
strcmp_loop:
    ldrb w9, [x5], #1
    ldrb w10, [x6], #1
    cmp w9, w10
    b.ne strcmp_diff
    cbz w9, strcmp_match
    b strcmp_loop
strcmp_match:
    mov x0, #0
    ret
strcmp_diff:
    mov x0, #1
    ret


// --- eval_list ---
eval_list:
    // Check if it's the empty list 'nil' (Offset 0)
    EXTRACT_OFFSET x9, x0
    cbz x9, eval_done   // 'nil' evaluates to 'nil'

    // Save frame and non-volatile registers (16-byte aligned)
    stp x29, x30, [sp, #-64]!
    stp x19, x20, [sp, #16]
    stp x21, x22, [sp, #32]
    
    mov x19, x0         // x19 = Current expression list
    mov x20, x1         // x20 = Environment

    // 1. Evaluate the operator (CAR)
    bl prim_car         // x0 = car(expr)
    mov x1, x20         // env
    bl eval             // x0 = eval(car(expr))
    mov x21, x0         // x21 = Evaluated Operator (TAG_FUNC or TAG_MACRO)

    // 2. Get the operands (CDR)
    mov x0, x19
    bl prim_cdr
    mov x19, x0         // x19 = Operands list

    // Check if operator is a MACRO / JIT Generator
    lsr x9, x21, #56
    cmp x9, #TAG_MACRO
    b.eq apply_macro

    // 3. Normal Function Application
    // We use the stack to hold up to 8 evaluated arguments temporarily.
    sub sp, sp, #64
    mov x22, #0         // x22 = Argument index

arg_loop:
    EXTRACT_OFFSET x9, x19
    cbz x9, arg_done    // Reached end of arguments list

    mov x0, x19
    bl prim_car         // Get arg expression
    mov x1, x20         // Environment
    bl eval             // Evaluate arg
    
    // Store evaluated argument into stack array [sp + x22 * 8]
    str x0, [sp, x22, lsl #3]
    add x22, x22, #1

    mov x0, x19
    bl prim_cdr         // Move to next argument cell
    mov x19, x0
    b arg_loop

arg_done:
    // Load evaluated arguments directly into AArch64 ABI registers!
    // This effortlessly bridges Lisp to native AArch64 C-style functions.
    ldp x0, x1, [sp, #0]
    ldp x2, x3, [sp, #16]
    ldp x4, x5, [sp, #32]
    ldp x6, x7, [sp, #48]
    add sp, sp, #64     // Pop argument array

    // Extract the tag of the Operator (so we know if it was FUNC or JIT)
    lsr x10, x21, #56

    // Untag the pointer and call it
    and x9, x21, #0x00FFFFFFFFFFFFFF

    cmp x10, #TAG_JIT
    b.eq call_jit

call_native:
    blr x9              // Call Native (prim_cons, etc). Let it mutate x25!
    b list_return

call_jit:
    // Protect VM context
    stp x27, x28, [sp, #-32]!  // Save NIL_CAR, NIL_CDR
    stp x25, x26, [sp, #16]    // Save DAT_OFF, AST_OFF
    
    // Jump into Native/JIT Code!
    blr x9              
    
    // Restore VM context
    ldp x25, x26, [sp, #16]
    ldp x27, x28, [sp], #32
    

    // --- TAG CLEANUP (Only runs if x10 == TAG_JIT) ---
    and x0, x0, #0x00FFFFFFFFFFFFFF
    mov x9, #TAG_INT
    lsl x9, x9, #56
    orr x0, x0, x9
    b list_return       // Now we unconditionally jump OVER apply_macro.

apply_macro:
    // Macros (Generators) receive the raw, unevaluated CDR in x0
    mov x0, x19
    mov x1, x20         // Environment
    and x9, x21, #0x00FFFFFFFFFFFFFF
    blr x9              // Call Macro! (Result in x0)

list_return:
    // Restore frame
    ldp x19, x20, [sp, #16]
    ldp x21, x22, [sp, #32]
    ldp x29, x30, [sp], #64
    ret
