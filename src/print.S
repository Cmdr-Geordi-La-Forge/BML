.include "inc/vm.inc"
.arch armv8-a
.data
str_lparen: .asciz "("
str_rparen: .asciz ")"
str_space:  .asciz " "
str_dot:    .asciz " . "
str_nil:    .asciz "nil"
str_native: .asciz "<native>"

.text
.global print_node
.global print_str
.global print_newline

// --- print_str ---
// Input: x0 = null-terminated string
print_str:
    mov x1, x0          // buffer ptr
    mov x2, #0          // length counter
1:  ldrb w3, [x1, x2]
    cbz w3, 2f          // If null, stop counting
    add x2, x2, #1
    b 1b
2:  
    mov x0, #STDOUT
    mov x8, #SYS_WRITE
    svc #0
    ret

// --- print_newline ---
print_newline:
    sub sp, sp, #16
    mov w9, #'\n'
    strb w9, [sp]
    mov x1, sp
    mov x2, #1
    mov x0, #STDOUT
    mov x8, #SYS_WRITE
    svc #0
    add sp, sp, #16
    ret

// --- print_node ---
// Input: x0 = Tagged AST Node
print_node:
    stp x29, x30, [sp, #-16]!
    
    lsr x9, x0, #56             // Extract Tag
    cmp x9, #TAG_INT
    b.eq print_int
    cmp x9, #TAG_SYM
    b.eq print_sym
    cmp x9, #TAG_LIST
    b.eq print_list
    
    ldr x0, =str_native
    bl print_str
    b print_done

// --- print_sym ---
print_sym:
    and x0, x0, #0x00FFFFFFFFFFFFFF // Untag absolute pointer
    bl print_str
    b print_done

// --- print_int ---
print_int:
    sbfx x9, x0, #0, #56        // Sign-extend the 56-bit integer
    
    // Allocate 32 bytes on stack for string buffer
    sub sp, sp, #32
    add x10, sp, #30
    strb wzr, [x10]             // Null terminator
    
    mov x11, #0                 // Negative flag
    cmp x9, #0
    b.ge 1f
    mov x11, #1                 // It is negative
    neg x9, x9                  // Make positive for division loop
1:
    mov x12, #10
2:
    udiv x13, x9, x12           // x13 = x9 / 10
    msub x14, x13, x12, x9      // x14 = Remainder
    add x14, x14, '0'           // Convert to ASCII
    sub x10, x10, #1
    strb w14, [x10]
    mov x9, x13
    cbnz x9, 2b                 // Loop until quotient is 0

    cbz x11, 3f
    sub x10, x10, #1            // Prepend '-' if negative
    mov w14, '-'
    strb w14, [x10]
3:
    mov x0, x10
    bl print_str
    add sp, sp, #32             // Restore stack
    b print_done

// --- print_list ---
print_list:
    EXTRACT_OFFSET x9, x0
    cbnz x9, 1f
    ldr x0, =str_nil            // Offset 0 is 'nil'
    bl print_str
    b print_done
1:
    stp x19, x20, [sp, #-16]!
    mov x19, x0                 // x19 = Current Node
    ldr x0, =str_lparen
    bl print_str

2:  // List iteration loop
    mov x0, x19
    bl prim_car
    bl print_node               // Print CAR
    
    mov x0, x19
    bl prim_cdr
    mov x19, x0                 // x19 = CDR
    
    EXTRACT_OFFSET x9, x19
    cbz x9, 3f                  // If CDR is nil, end of list
    
    lsr x10, x19, #56
    cmp x10, #TAG_LIST
    b.eq 4f                     // If CDR is a list, print space and continue
    
    // If CDR is an atom, it's a dotted pair!
    ldr x0, =str_dot
    bl print_str
    mov x0, x19
    bl print_node
    b 3f

4:
    ldr x0, =str_space
    bl print_str
    b 2b

3:
    ldr x0, =str_rparen
    bl print_str
    ldp x19, x20, [sp], #16

print_done:
    ldp x29, x30, [sp], #16
    ret
