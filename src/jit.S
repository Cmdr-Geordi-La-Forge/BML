.include "inc/vm.inc"
.arch armv8-a
.text


// --- handler_lambda ---
// Input: x0 = Tagged List: ((args...) (body...))
// Output: x0 = TAG_FUNC | JIT_Address
.global handler_lambda
handler_lambda:
    stp x29, x30, [sp, #-64]!
    stp x19, x20, [sp, #16]
    stp x21, x22, [sp, #32]

    mov x19, x0             
    mov x20, x1             
    mov x21, JIT_HEAD       

    bl prim_car
    mov x22, x0             

    // Allocate CTE safely using the Stack
    add x9, NIL_CAR, DAT_OFF 
    str x9, [sp, #48]       // [sp, #48] = New Env Ptr
    str x9, [sp, #56]       // [sp, #56] = Write Head

    mov x8, #0              
arg_loop:
    EXTRACT_OFFSET x9, x22
    cbz x9, args_done

    mov x0, x22
    bl prim_car
    and x14, x0, #0x00FFFFFFFFFFFFFF 

    bl alloc_reg            

    ldr w9, =0xAA0003E0
    lsl w10, w8, #16
    orr w9, w9, w10
    orr w9, w9, w0
    str w9, [JIT_HEAD], #4  

    mov x9, #TAG_REG
    lsl x9, x9, #56
    orr x9, x9, x0          

    ldr x13, [sp, #56]      // Load Write Head
    str x14, [x13], #8      
    str x9, [x13], #8       
    str x13, [sp, #56]      // Save Write Head

    add x8, x8, #1

    mov x0, x22
    bl prim_cdr
    mov x22, x0
    b arg_loop

args_done:
    ldr x13, [sp, #56]      
    mov x9, #1
    str x9, [x13], #8       
    str x20, [x13], #8      

    sub x9, x13, NIL_CAR
    mov DAT_OFF, x9         

    mov x0, x19
    bl prim_cdr
    mov x19, x0             

    // Args list is dead. x22 is safe to hold the New Env Ptr!
    ldr x22, [sp, #48]      

compile_loop:
    EXTRACT_OFFSET x9, x19
    cbz x9, compile_done

    mov x0, x19
    bl prim_car
    mov x1, x22             // Pass New Env to eval
    bl eval

    mov x0, x19
    bl prim_cdr
    mov x19, x0
    b compile_loop

compile_done:
    ldr w9, =0xD65F03C0
    str w9, [JIT_HEAD], #4

    mov x13, x22            // Read head starts at New Env
free_loop:
    ldr x9, [x13], #8
    cmp x9, #1
    b.eq free_done

    ldr x0, [x13], #8
    and x0, x0, #0xFF
    bl free_reg
    b free_loop

free_done:
    mov x0, #TAG_JIT
    lsl x0, x0, #56
    orr x0, x0, x21

    ldp x21, x22, [sp, #32]
    ldp x19, x20, [sp, #16]
    ldp x29, x30, [sp], #64
    ret

    // --- handler_add ---
    // Input: x0 = Tagged List: (Rd Rn Rm)
    // Action: Emits "add Rd, Rn, Rm" to JIT_HEAD
    .global handler_add
handler_add:
    stp x29, x30, [sp, #-64]!
    stp x19, x20, [sp, #16]
    stp x21, x22, [sp, #32]

    mov x19, x0             // x19 = (Rd Rn Rm)
    str x1, [sp, #48]             // Save local env

    bl prim_car
    ldr x1, [sp, #48]       // Pass local env to eval
    bl eval                 // Evaluating 'x0' yields the integer 0!
    and x20, x0, #0xFF      // x20 = Rd

    mov x0, x19
    bl prim_cdr
    mov x19, x0
    bl prim_car
    ldr x1, [sp, #48]
    bl eval
    and x21, x0, #0xFF      // x21 = Rn

    mov x0, x19
    bl prim_cdr
    mov x19, x0
    bl prim_car
    ldr x1, [sp, #48]
    bl eval
    and x22, x0, #0xFF      // x22 = Rm

    // emit instruction
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
    ldp x29, x30, [sp], #64
    ret

    // --- handler_sub ---
    // Input: x0 = (Rd Rn Rm)
    // Action: Emits "sub Rd, Rn, Rm"
    .global handler_sub
handler_sub:
    stp x29, x30, [sp, #-64]!
    stp x19, x20, [sp, #16]
    stp x21, x22, [sp, #32]

    mov x19, x0
    str x1, [sp, #48]
    
    bl prim_car
    ldr x1, [sp, #48]
    bl eval
    and x20, x0, #0xFF      // Rd

    mov x0, x19
    bl prim_cdr
    mov x19, x0
    bl prim_car
    ldr x1, [sp, #48]
    bl eval
    and x21, x0, #0xFF      // Rn

    mov x0, x19
    bl prim_cdr
    mov x19, x0
    bl prim_car
    ldr x1, [sp, #48]
    bl eval
    and x22, x0, #0xFF      // Rm

    // Base mask for 64-bit SUB: 0xCB000000
    ldr w9, =0xCB000000
    lsl w22, w22, #16
    orr w9, w9, w22
    lsl w21, w21, #5
    orr w9, w9, w21
    orr w9, w9, w20

    str w9, [JIT_HEAD], #4

    mov x0, #(TAG_LIST << 56)
    ldp x21, x22, [sp, #32]
    ldp x19, x20, [sp, #16]
    ldp x29, x30, [sp], #64
    ret

    // --- handler_if ---
    // Input: x0 = (Rt then_expr else_expr)
    .global handler_if
handler_if:
    stp x29, x30, [sp, #-64]!
    stp x19, x20, [sp, #16]
    stp x21, x22, [sp, #32]
    
    mov x19, x0
    str x1, [sp, #48]

    // 1. Parse Rt (The register to test)
    bl prim_car
    ldr x1, [sp, #48]
    bl eval                 // e.g., 'x2' evaluates to 2
    and x20, x0, #0xFF      // x20 = Rt

    // 2. Emit 'CBZ Rt, else_branch'
    mov x21, JIT_HEAD       // Save address P1
    ldr w9, =0xB4000000     // CBZ base mask
    orr w9, w9, w20         // Inject Rt into bits 0-4 of the instruction!
    str w9, [JIT_HEAD], #4

    // 3. Compile THEN branch
    mov x0, x19
    bl prim_cdr
    mov x19, x0
    bl prim_car
    ldr x1, [sp, #48]
    bl eval

    // 4. Emit Unconditional 'B end'
    mov x22, JIT_HEAD       // Save address P2
    ldr w9, =0x14000000     // B base mask
    str w9, [JIT_HEAD], #4

    // 5. Backpatch CBZ to point to ELSE
    sub x9, JIT_HEAD, x21
    lsr x9, x9, #2
    ldr w10, =0x7FFFF
    and w9, w9, w10
    lsl w9, w9, #5          // Shift offset to bits 5-23
    ldr w11, [x21]
    orr w11, w11, w9        // Inject offset without destroying Rt!
    str w11, [x21]

    // 6. Compile ELSE branch
    mov x0, x19
    bl prim_cdr
    mov x19, x0
    bl prim_car
    ldr x1, [sp, #48]
    bl eval

    // 7. Backpatch 'B' to point to END
    sub x9, JIT_HEAD, x22
    lsr x9, x9, #2
    ldr w10, =0x3FFFFFF
    and w9, w9, w10
    ldr w11, [x22]
    orr w11, w11, w9
    str w11, [x22]

    mov x0, #(TAG_LIST << 56) // Return NIL
    ldp x21, x22, [sp, #32]
    ldp x19, x20, [sp, #16]
    ldp x29, x30, [sp], #64
    ret
