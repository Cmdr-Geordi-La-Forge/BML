;; ==========================================
;; BML Linear IR - Hardware Abstraction Layer
;; ==========================================

macro ir_load_imm val { mov rax, val }

;; Handles both locals (negative offsets) and args (positive offsets)
macro ir_load_mem offset { mov rax, [rbp + offset] }

macro ir_load_glob label {
  lea rdi, [label]
  mov rax, [rdi]
}

;; --- 3. Stack & Control Flow ---
macro ir_push { push rax }
macro ir_pop_r8 { pop r8 }
macro ir_set_arg2 {
  mov rcx, rax
  pop rax
}

macro ir_label id { label_#id: }

macro ir_jz id {
  test rax, rax
  jz label_#id
}

macro ir_jmp id { jmp label_#id }

macro ir_call { call rax }
macro ir_ret {
  mov rsp, rbp
  pop rbp
  ret
}

;; --- 4. Math & Logic ---
macro ir_add { add rax, rcx }
macro ir_sub { sub rax, rcx }
macro ir_mul { imul rax, rcx }
macro ir_div {
  mov r8, rcx
  cqo
  idiv r8
}

macro ir_and { and rax, rcx }
macro ir_or  { or rax, rcx }

macro ir_eql {
  cmp rax, rcx
  mov rax, 0
  sete al
  }
macro ir_lt  {
  cmp rax, rcx
  mov rax, 0
  setl al
  }
macro ir_gt {
  cmp rax, rcx
  mov rax, 0
  setg al
  }
macro ir_le  {
  cmp rax, rcx
  mov rax, 0
  setle al
  }
macro ir_ge  {
  cmp rax, rcx
  mov rax, 0
  setge al
  }

macro ir_ash {
  local .left, .done
  test rcx, rcx
  jns .left
  neg rcx
  sar rax, cl
  jmp .done
.left:
  shl rax, cl
.done:
}

macro ir_car { mov rax, [rax] }
macro ir_cdr { mov rax, [rax+8] }
macro ir_cons {
  mov [r15], rax
  mov [r15+8], rcx
  mov rax, r15
  add r15, 16
}

;; --- Memory & Heap ---
macro ir_peek { mov rax, [rax] }
macro ir_poke { mov [rax], rcx }
macro ir_pokebyte { mov [rax], cl }
macro ir_peekidx { mov rax, [rax + rcx*8] }
macro ir_pokeidx { mov [r8 + rcx*8], rax }

macro ir_dict { lea rax, [global_dict] }

macro ir_getheap { mov rax, r15 }
macro ir_setheap { mov r15, rax }
macro ir_alloc {
  imul rax, 8
  mov rcx, r15
  add r15, rax
  mov rax, rcx
}

macro ir_load_mem_dyn { mov rax, [rbp + rax] }
macro ir_add_rsp_dyn  { add rsp, rax }

;; --- Logic & I/O ---
macro ir_not {
  test rax, rax
  mov rax, 0
  sete al
}
macro ir_getchar  { call read_char }
macro ir_putchar  { call print_char }
macro ir_putint   { call print_int }
macro ir_puthex   { call print_hex }
macro ir_putchunk { call print_chunk }

;; --- ABI & Function calls ---
macro ir_add_rsp val { add rsp, val }
macro ir_pop_rax { pop rax }

macro ir_enter {
  push rbp
  mov rbp, rsp
}
macro ir_tcall {
  mov rsp, rbp
  pop rbp
  jmp rax
}
macro ir_load_func id { lea rax, [label_#id] }
macro ir_load_str id  { lea rax, [STR_#id] }
macro ir_store_glob label {
  pop rax
  lea rdi, [label]
  mov [rdi], rax
}

;; C ABI Register Mapping
macro ir_pop_arg idx {
  if idx = 0
    pop rdi
  else if idx = 1
    pop rsi
  else if idx = 2
    pop rdx
  else if idx = 3
    pop rcx
  else if idx = 4
    pop r8
  else if idx = 5
    pop r9
  end if
}

macro ir_push_arg idx {
  if idx = 0
    push rdi
  else if idx = 1
    push rsi
  else if idx = 2
    push rdx
  else if idx = 3
    push rcx
  else if idx = 4
    push r8
  else if idx = 5
    push r9
  end if
}

macro ir_syscall numargs {
  if numargs >= 5
    mov r10, rcx
  end if
  syscall
}

;; --- I/O (Position Independent for JIT) ---

macro ir_getchar {
  push 0       ;; Push 8 bytes of zeroes to stack
  mov rax, 0   ;; sys_read
  mov rdi, 0   ;; stdin
  mov rsi, rsp ;; buffer points to the zeroes we pushed
  mov rdx, 1   ;; read 1 byte
  syscall      ;; (Linux syscalls clobber rcx and r11)
  pop rax      ;; Pop the buffer (now containing our char) into rax
}

macro ir_putchar {
  push rax     ;; Push the char to the stack
  mov rax, 1   ;; sys_write
  mov rdi, 1   ;; stdout
  mov rsi, rsp ;; buffer is the stack
  mov rdx, 1   ;; write 1 byte
  syscall
  pop rax      ;; Restore rax
}

macro ir_putint {
  local .loop, .print, .done, .is_zero, .positive, .print_neg_check
  mov rcx, 10
  mov r8, rsp
  mov r9, 0    ;; negative flag
  test rax, rax
  jns .positive
  neg rax
  mov r9, 1
.positive:
  test rax, rax
  jnz .loop
  push 48      ;; '0'
  jmp .print_neg_check
.loop:
  test rax, rax
  jz .print_neg_check
  xor rdx, rdx
  div rcx
  add rdx, 48
  push rdx
  jmp .loop
.print_neg_check:
  test r9, r9
  jz .print
  push 45      ;; '-'
.print:
  cmp rsp, r8
  je .done
  mov rax, 1
  mov rdi, 1
  mov rsi, rsp
  mov rdx, 1
  syscall
  add rsp, 8
  jmp .print
.done:
}

macro ir_puthex {
  local .loop, .print, .done, .is_num, .push_char
  mov rcx, 16
  mov r8, rsp
  test rax, rax
  jnz .loop
  push 48
  jmp .print
.loop:
  test rax, rax
  jz .print
  xor rdx, rdx
  div rcx
  cmp rdx, 9
  jbe .is_num
  add rdx, 87  ;; 'a' - 10
  jmp .push_char
.is_num:
  add rdx, 48
.push_char:
  push rdx
  jmp .loop
.print:
  cmp rsp, r8
  je .done
  mov rax, 1
  mov rdi, 1
  mov rsi, rsp
  mov rdx, 1
  syscall
  add rsp, 8
  jmp .print
.done:
}

macro ir_putchunk {
  local .len_loop, .print
  ;; rax contains pointer to null-terminated string
  mov rsi, rax
  xor rdx, rdx
.len_loop:
  cmp byte [rsi + rdx], 0
  je .print
  inc rdx
  jmp .len_loop
.print:
  mov rax, 1   ;; sys_write
  mov rdi, 1   ;; stdout
  syscall
}

;; --- SELF-DESCRIBING DICTIONARY ---
macro regop opsym, macname {
    local start, end
    align 8
    dq opsym                    ;; FASM evaluates 'add' to 0x646461!
    dq (end - start)            ;; Dynamically calculates the size
    start:
      macname                   ;; The hardware instructions
    end:
}

align 8
global_dict:
  regop 'push', ir_push
  regop 'set2', ir_set_arg2
  regop 'popr8', ir_pop_r8
  regop 'add', ir_add
  regop 'sub', ir_sub
  regop 'mul', ir_mul
  regop 'div', ir_div
  regop 'eql', ir_eql
  regop 'lt', ir_lt
  regop 'gt', ir_gt
  regop 'le', ir_le
  regop 'ge', ir_ge
  regop 'and', ir_and
  regop 'logand', ir_and
  regop 'or', ir_or
  regop 'logior', ir_or
  regop 'ash', ir_ash
  regop 'not', ir_not
  regop 'car', ir_car
  regop 'cdr', ir_cdr
  regop 'cons', ir_cons
  regop 'peek', ir_peek
  regop 'poke', ir_poke
  regop 'pokebyte', ir_pokebyte
  regop 'peekidx', ir_peekidx
  regop 'pokeidx', ir_pokeidx
  regop 'getheap', ir_getheap
  regop 'setheap', ir_setheap
  regop 'alloc', ir_alloc
  regop 'getchar', ir_getchar
  regop 'putchar', ir_putchar
  regop 'putint', ir_putint
  regop 'puthex', ir_puthex
  regop 'putchunk', ir_putchunk
  regop 'popra', ir_pop_rax
  regop 'enter', ir_enter
  regop 'call', ir_call
  regop 'ret', ir_ret
  regop 'tcall', ir_tcall
  regop 'ldmd', ir_load_mem_dyn
  regop 'addsd', ir_add_rsp_dyn
  align 8
  dq 0 ;; Null terminator
