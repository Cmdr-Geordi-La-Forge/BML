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

macro ir_getheap { mov rax, r15 }
macro ir_setheap { mov r15, rax }
macro ir_alloc {
  imul rax, 8
  mov rcx, r15
  add r15, rax
  mov rax, rcx
}

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
