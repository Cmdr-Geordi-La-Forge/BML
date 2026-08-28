.include "inc/vm.inc"
.arch armv8-a
.text

.global parse

// --- parse ---
// Input: x0 = null-terminated string
// Output: x0 = Tagged AST offset
parse:
    // Drop into parse_expr
    b parse_expr


// --- parse_expr ---
// Input: x0 = ptr
// Output: x0 = Tagged Node, x1 = next ptr
parse_expr:
    stp x29, x30, [sp, #-16]!
    bl skip_ws
    
    ldrb w9, [x0]
    cmp w9, '('
    b.eq 1f
    
    // Not '(', must be an atom (Symbol or Integer)
    bl parse_atom
    ldp x29, x30, [sp], #16
    ret

1:  // Is '('
    add x0, x0, #1      // consume '('
    bl parse_list
    ldp x29, x30, [sp], #16
    ret


// --- skip_ws ---
skip_ws:
    ldrb w9, [x0]
    cmp w9, ' '
    b.eq 1f
    cmp w9, '\t'
    b.eq 1f
    cmp w9, '\n'
    b.eq 1f
    ret
1:  add x0, x0, #1
    b skip_ws


// --- parse_list ---
// Input: x0 = ptr
// Output: x0 = Tagged List, x1 = next ptr
parse_list:
    stp x29, x30, [sp, #-32]!
    stp x19, x20, [sp, #16]

    mov x19, x0         // x19 = current ptr
    bl skip_ws
    mov x19, x0

    ldrb w9, [x19]
    cmp w9, ')'
    b.eq 2f             // End of list
    
    // 1. parse_expr() -> CAR
    mov x0, x19
    bl parse_expr
    mov x20, x0         // x20 = node
    mov x19, x1         // x19 = next ptr

    // 2. parse_list() -> CDR (Recursive)
    mov x0, x19
    bl parse_list
    mov x19, x1         // x19 = next ptr2

    // 3. cons(CAR, CDR)
    mov x1, x0          // CDR
    mov x0, x20         // CAR
    bl prim_cons        // Returns new list cell in x0
    
    mov x1, x19         // Return next ptr
    b 3f

2:  // Found ')'
    add x19, x19, #1    // consume ')'
    mov x0, #(TAG_LIST << 56) // 'nil' (Offset 0)
    mov x1, x19
3:
    ldp x19, x20, [sp, #16]
    ldp x29, x30, [sp], #32
    ret


// --- parse_atom ---
// Input: x0 = ptr
// Output: x0 = Tagged Value, x1 = next ptr
parse_atom:
    stp x29, x30, [sp, #-48]!
    stp x19, x20, [sp, #16]
    stp x21, x22, [sp, #32]

    mov x19, x0                     // x19 = Read ptr
    add x20, NIL_CAR, DAT_OFF       // x20 = Write ptr in our Data Arena
    mov x21, x20                    // x21 = Start of interned string
    mov x22, #1                     // x22 = Is_Integer flag (1 = true)

1:
    ldrb w9, [x19]
    // Check Delimiters
    cbz w9, 2f
    cmp w9, ' '
    b.eq 2f
    cmp w9, '\t'
    b.eq 2f
    cmp w9, '\n'
    b.eq 2f
    cmp w9, '('
    b.eq 2f
    cmp w9, ')'
    b.eq 2f

    // Store character in Data Arena
    strb w9, [x20], #1
    add x19, x19, #1
    
    // Validate if it is a digit
    cmp w9, '0'
    b.lt check_minus
    cmp w9, '9'
    b.gt check_alpha
    b 1b

check_minus:
    cmp w9, '-'
    b.ne check_alpha
    // '-' is only valid as the first character of an integer
    add x10, x21, #1
    cmp x20, x10
    b.ne check_alpha
    b 1b

check_alpha:
    mov x22, #0         // Contains alphabet/symbols, not an integer
    b 1b

2:
    // End of atom. Null-terminate it.
    strb wzr, [x20], #1

    // Update Bump Allocator (DAT_OFF) & align to 8 bytes
    sub x9, x20, NIL_CAR // Calculate new byte length
    add x9, x9, #7       // Round up...
    and x9, x9, #~7      // ...to nearest 8 bytes
    mov DAT_OFF, x9

    cbnz x22, 4f         // If Is_Integer != 0, branch to Integer parsing

    // It is a Symbol! Tag absolute pointer and return.
    mov x0, #TAG_SYM
    lsl x0, x0, #56
    orr x0, x0, x21
    mov x1, x19
    b 5f

4:
    // It is an Integer!
    mov x0, x21
    bl atoi
    mov x9, #TAG_INT
    lsl x9, x9, #56
    orr x0, x0, x9      // Tag as INT
    mov x1, x19

5:
    ldp x21, x22, [sp, #32]
    ldp x19, x20, [sp, #16]
    ldp x29, x30, [sp], #48
    ret


// --- atoi ---
// Simple string to integer conversion. Handles negative numbers.
atoi:
    mov x1, #0          // Accumulator
    mov x4, #1          // Sign multiplier
    ldrb w2, [x0]
    cmp w2, '-'
    b.ne 1f
    mov x4, #-1
    add x0, x0, #1
1:
    ldrb w2, [x0], #1
    cbz w2, 2f
    sub w2, w2, '0'
    mov x3, #10
    mul x1, x1, x3
    add x1, x1, x2
    b 1b
2:
    mul x0, x1, x4
    ret
