add:	add rax, rcx
sub:	sub rax, rcx
mul:	imul rax, rcx
div:	mov r8, rcx
	cqo
	idiv r8
eql:	cmp rax, rcx
	mov rax, 0
	sete al
lt:	cmp rax, rcx
	mov rax, 0
	setl al
gt:	cmp rax, rcx
	mov rax, 0
	setg al
le:	cmp rax, rcx
	mov rax, 0
	setle al
ge:	cmp rax, rcx
	mov rax, 0
	setge al
logior:	or rax, rcx
or:	or rax, rcx
logand:	and rax, rcx
and:	and rax, rcx
not:	test rax, rax
	mov rax, 0
	sete al
poke:	mov [rax], rcx
poke-byte:	mov [rax], cl
peek-idx:	mov rax, [rax + rcx*8]
poke-idx:	mov [r8 + rcx*8], rax
read-char:	call read_char
get-heap:	mov rax, r15
print-int:	call print_int
print-char:	call print_char
print-chunk:	call print_chunk
peek:	mov rax, [rax]
car:	mov rax, [rax]
cdr:	mov rax, [rax+8]
alloc:	imul rax, 8
	mov rcx, r15
	add r15, rax
	mov rax, rcx
set-heap:	mov r15, rax
cons:	mov [r15], rax
	mov [r15+8], rcx
	mov rax, r15
	add r15, 16
fadd:	movq xmm0, rax
	movq xmm1, rcx
	addsd xmm0, xmm1
	movq rax, xmm0
fsub:	movq xmm0, rax
	movq xmm1, rcx
	subsd xmm0, xmm1
	movq rax, xmm0
fmul:	movq xmm0, rax
	movq xmm1, rcx
	mulsd xmm0, xmm1
	movq rax, xmm0
fdiv:	movq xmm0, rax
	movq xmm1, rcx
	divsd xmm0, xmm1
	movq rax, xmm0
itof:	cvtsi2sd xmm0, rax
	movq rax, xmm0
print-hex:	call print_hex

; --- Optimized Immediate Primitives ---
add_imm:	add rax, %1
sub_imm:	sub rax, %1
mul_imm:	imul rax, %1
logand_imm:	and rax, %1
and_imm:	and rax, %1
logior_imm:	or rax, %1
or_imm:	or rax, %1
ash_left_imm:	shl rax, %1
ash_right_imm:	sar rax, %1

; --- Optimized Local Variables ---
add_local:	add rax, [rbp - %1]
sub_local:	sub rax, [rbp - %1]
mul_local:	imul rax, [rbp - %1]
logand_local:	and rax, [rbp - %1]
and_local:	and rax, [rbp - %1]
logior_local:	or rax, [rbp - %1]
or_local:	or rax, [rbp - %1]

; --- Optimized Arg Variables ---
add_arg:	add rax, [rbp + %1]
sub_arg:	sub rax, [rbp + %1]
mul_arg:	imul rax, [rbp + %1]
logand_arg:	and rax, [rbp + %1]
and_arg:	and rax, [rbp + %1]
logior_arg:	or rax, [rbp + %1]
or_arg:	or rax, [rbp + %1]
