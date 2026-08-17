format binary
use64

include 'ir_macros.asm'

;; ==========================================
;; Metadata Header (Offset and Size Table)
;; ==========================================
;; We reserve space for 64 opcodes. 
;; Each entry is 16 bytes: [8 bytes: Absolute Offset] [8 bytes: Byte Size]
table_start:
  dq 64 dup (0, 0) 

;; Macro to automatically register an opcode in the metadata table
macro register_op opcode, macro_name {
  local start_addr, end_addr
  
  ;; Go back to the header and write the offset and size for this opcode
  store qword start_addr at table_start + (opcode * 16)
  store qword (end_addr - start_addr) at table_start + (opcode * 16) + 8
  
  ;; Emit the actual machine code bytes here
  start_addr:
    macro_name
  end_addr:
}

;; ==========================================
;; The Machine Code Chunks
;; ==========================================
align 8
chunks_start:

;; --- Control Flow & Stack ---
register_op 6, ir_push
register_op 7, ir_set_arg2
register_op 54, ir_pop_rax
register_op 55, ir_enter

;; --- Math & Logic ---
register_op 20, ir_add
register_op 21, ir_sub
register_op 22, ir_mul
register_op 23, ir_div
register_op 27, ir_eql
register_op 28, ir_lt
register_op 29, ir_gt
register_op 30, ir_le
register_op 31, ir_ge
register_op 32, ir_and
register_op 33, ir_or
register_op 34, ir_ash
register_op 41, ir_not

;; --- Memory & Pointers ---
register_op 24, ir_car
register_op 25, ir_cdr
register_op 26, ir_cons
register_op 36, ir_peek
register_op 37, ir_poke
register_op 38, ir_pokebyte
register_op 39, ir_peekidx
register_op 40, ir_pokeidx
register_op 42, ir_getheap
register_op 43, ir_setheap
register_op 44, ir_alloc

;; --- I/O ---
register_op 45, ir_getchar
register_op 46, ir_putchar
register_op 47, ir_putint
register_op 48, ir_puthex
register_op 49, ir_putchunk
