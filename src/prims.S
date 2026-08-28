.include "inc/vm.inc"
.arch armv8-a
.text

.global prim_car
.global prim_cdr
.global prim_cons

// --- prim_car ---
// Input: x0 (Tagged AST offset)
// Output: x0 (Tagged value at CAR)
prim_car:
    EXTRACT_OFFSET x9, x0       // Sign-extend the 56-bit offset
    ldr x0, [NIL_CAR, x9]       // Load CAR using absolute NIL_CAR base
    ret

// --- prim_cdr ---
// Input: x0 (Tagged AST offset)
// Output: x0 (Tagged value at CDR)
prim_cdr:
    EXTRACT_OFFSET x9, x0       // Sign-extend the 56-bit offset
    ldr x0, [NIL_CDR, x9]       // Load CDR using absolute NIL_CDR base
    ret

// --- prim_cons ---
// Input: x0 (CAR value), x1 (CDR value)
// Output: x0 (Tagged AST offset of new cell)
prim_cons:
    // 1. Bump the AST allocator (grows negatively, 16 bytes per cons cell)
    sub AST_OFF, AST_OFF, #16
    
    // 2. Store CAR and CDR at the new relative offset
    str x0, [NIL_CAR, AST_OFF]
    str x1, [NIL_CDR, AST_OFF]
    
    // 3. Return the new offset, tagged as a LIST
    // We mask out the top 8 bits of the negative offset, then OR the tag
    mov x9, #(TAG_LIST << 56)
    bfi x9, AST_OFF, #0, #56    // Bitfield Insert: put lower 56 bits of AST_OFF into x9
    mov x0, x9
    ret
