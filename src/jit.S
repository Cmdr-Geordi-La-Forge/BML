.include "inc/vm.inc"
.arch armv8-a
.text

.global handler_lambda
.global handler_add

// --- handler_lambda ---
// Input: x0 = Tagged List: ((args...) (body...))
// Output: x0 = TAG_FUNC | JIT_Address
handler_lambda:
    stp x29, x30, [sp, #-32]!
    stp x19, x20, [sp, #16]

    mov x20, JIT_HEAD       // Save the entry point of our new function

    // 1. Emit Function Prologue (Self-Cleaning Memory Boundary)
    // AArch64: stp x26, x25, [sp, #-16]! (Machine code: 0xA9BF67FA)
    ldr w9, =0xA9BF67FA
    str w9, [JIT_HEAD], #4

    // 2. Skip the arguments list (we assume they arrive in ABI registers x0, x1...)
    bl prim_cdr
    mov x19, x0             // x19 = body expressions

    // 3. Compile the Body (Loop through expressions)
compile_loop:
    EXTRACT_OFFSET x9, x19
    cbz x9, compile_done    // End of body

    mov x0, x19
    bl prim_car             // x0 = expression to compile (e.g., '(add x0 x1)')
    
    // By passing the expression to `eval`, we trigger our macros!
    // `eval` will look up `add` and call `handler_add` at compile-time.
    ldr x1, =env_root       
    bl eval                 

    mov x0, x19
    bl prim_cdr             // Next expression
    mov x19, x0
    b compile_loop

compile_done:
    // 4. Emit Function Epilogue (Instantly Freeing Memory)
    // AArch64: ldp x26, x25, [sp], #16 (Machine code: 0xA8C167FA)
    ldr w9, =0xA8C167FA
    str w9, [JIT_HEAD], #4

    // AArch64: ret (Machine code: 0xD65F03C0)
    ldr w9, =0xD65F03C0
    str w9, [JIT_HEAD], #4

    // 5. Return the native function pointer!
    mov x0, #TAG_JIT
    lsl x0, x0, #56
    orr x0, x0, x20

    ldp x19, x20, [sp, #16]
    ldp x29, x30, [sp], #32
    ret


// --- handler_add ---
// Input: x0 = Tagged List: (Rd Rn Rm)
// Action: Emits "add Rd, Rn, Rm" to JIT_HEAD
handler_add:
    stp x29, x30, [sp, #-48]!
    stp x19, x20, [sp, #16]
    stp x21, x22, [sp, #32]
    
    mov x19, x0             // x19 = (Rd Rn Rm)

    // 1. Get Rd (Destination)
    mov x0, x19
    bl prim_car
    ldr x1, =env_root
    bl eval                 // Evaluating 'x0' yields the integer 0!
    and x20, x0, #0xFF      // x20 = Rd

    mov x0, x19
    bl prim_cdr
    mov x19, x0

    // 2. Get Rn (Source 1)
    mov x0, x19
    bl prim_car
    ldr x1, =env_root
    bl eval
    and x21, x0, #0xFF      // x21 = Rn

    mov x0, x19
    bl prim_cdr
    mov x19, x0

    // 3. Get Rm (Source 2)
    mov x0, x19
    bl prim_car
    ldr x1, =env_root
    bl eval
    and x22, x0, #0xFF      // x22 = Rm

    // 4. EMIT AARCH64 INSTRUCTION!
    // Base mask for 64-bit ADD: 0x8B000000
    ldr w9, =0x8B000000
    lsl w22, w22, #16       // Shift Rm
    orr w9, w9, w22
    lsl w21, w21, #5        // Shift Rn
    orr w9, w9, w21
    orr w9, w9, w20         // Add Rd
    
    str w9, [JIT_HEAD], #4  // Write instruction and advance JIT write head

    // Macros return NIL during compilation
    mov x0, #(TAG_LIST << 56)

    ldp x21, x22, [sp, #32]
    ldp x19, x20, [sp, #16]
    ldp x29, x30, [sp], #48
    ret
