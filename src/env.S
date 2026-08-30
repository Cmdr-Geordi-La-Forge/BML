.include "inc/vm.inc"
.arch armv8-a
.data
.align 3

.global sym_car, sym_cdr, sym_cons, sym_nil
.global sym_lambda, sym_add
sym_car:    .asciz "car"
sym_cdr:    .asciz "cdr"
sym_cons:   .asciz "cons"
sym_nil:    .asciz "nil"
sym_lambda: .asciz "lambda"
sym_add:    .asciz "add"
sym_sub:    .asciz "sub"
sym_if:     .asciz "if"
sym_def:    .asciz "def"

.irp reg, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31
    .global sym_x\reg
    sym_x\reg: .asciz "x\reg"
.endr

.align 3

.global env_root
env_root:
    .quad sym_car,  prim_car
    .quad sym_cdr,  prim_cdr
    .quad sym_cons, prim_cons
.global env_prims_end
env_prims_end:

.global env_macros
env_macros:
    .quad sym_lambda, handler_lambda
    .quad sym_add,    handler_add
    .quad sym_sub,    handler_sub
    .quad sym_if,     handler_if
    .quad sym_def,    handler_def
    .global env_macros_end

env_macros_end:

    .quad sym_nil,  ((TAG_LIST << 56) | 0)

    .irp reg, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31
        .quad sym_x\reg, ((TAG_INT << 56) | \reg)
    .endr

    .global env_tail
env_tail:
    .quad 0, 0

    .global env_tail_ptr
env_tail_ptr: .quad env_tail
