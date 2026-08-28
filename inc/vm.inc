    // --- TBI Data Tags (Bits 56-63) ---
    .equ TAG_LIST,  0x00  // List / AST Node (Relative offset)
    .equ TAG_INT,   0x01  // Integer literal
    .equ TAG_SYM,   0x03  // Symbol (Pointer to string)
    .equ TAG_FUNC,  0x04  // Native AArch64 function pointer
    .equ TAG_MACRO, 0x05  // Context-aware generator (receives raw AST)
    .equ TAG_JIT,   0x06  // JIT-Compiled Function (Needs re-tagging)
    .equ SYS_READ, 63
    .equ SYS_WRITE, 64
    .equ STDIN, 0
    .equ STDOUT, 1

    // --- VM Hardware Register Map ---
    // We use AArch64 callee-saved registers so they survive any C calls if needed.
    NIL_CAR  .req x27   // Absolute base address of (car nil)
    NIL_CDR  .req x28   // Absolute base address of (cdr nil)
    AST_OFF  .req x26   // Bump allocator for AST (Grows negatively: 0, -16, -32...)
    DAT_OFF  .req x25   // Bump allocator for Data (Grows positively: 16, 24, 32...)
    ENV_PTR  .req x24   // Pointer to the current active Environment
    JIT_HEAD .req x23   // JIT Write Head (where the next instruction is emitted)

    // --- Helper Macros ---
    // Since AST offsets are negative, if we overwrite the top byte with a tag,
    // we lose the 2's complement sign. This macro extracts a tagged offset
    // and sign-extends it back into a valid 64-bit negative/positive offset.
    .macro EXTRACT_OFFSET dest, src
    sbfx \dest, \src, #0, #56
    .endm
