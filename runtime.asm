;;; ----------------------------------------------------------------------------
;;; lookup_env: Finds a dynamically bound variable by uninterned name
;;; Inputs:  RDI = pointer to target symbol string list
;;;          R14 = current environment alist pointer
;;; Outputs: RAX = bound value (or 0 if not found)
;;; ----------------------------------------------------------------------------
lookup_env:
    mov rdx, r14           ; RDX = current environment node

.env_loop:
    test rdx, rdx          ; Is the environment list empty (0)?
    jz .not_found          ; If yes, we've exhausted the alist

    ; Extract the current environment pair
    mov rcx, [rdx]         ; RCX = pointer to inner pair (sym . val)
    mov r8, [rcx]          ; R8 = pointer to environment symbol string
    mov r9, [rcx + 8]      ; R9 = bound value

    ; Set up for the inner string-comparison loop
    mov r10, rdi           ; R10 = target symbol string pointer
    mov r11, r8            ; R11 = env symbol string pointer

.str_cmp_loop:
    ; Optimization & Base Case: Are the pointers identical?
    ; This triggers if they share a tail, OR if both are 0 (end of string).
    cmp r10, r11
    je .match              

    ; If one is 0 but the other isn't, the strings are different lengths
    test r10, r10
    jz .next_env
    test r11, r11
    jz .next_env

    ; Compare the actual 8-byte character chunks (CARs)
    mov rcx, [r10]         ; Reuse RCX to hold target chunk
    cmp rcx, [r11]         ; Compare against env chunk
    jne .next_env          ; Mismatch! Break out of inner loop

    ; Advance both string pointers to their next chunks (CDRs)
    mov r10, [r10 + 8]
    mov r11, [r11 + 8]
    jmp .str_cmp_loop

.match:
    mov rax, r9            ; We found it! Put the bound value in RAX
    ret

.next_env:
    mov rdx, [rdx + 8]     ; Advance to the next environment node (CDR of outer)
    jmp .env_loop

.not_found:
    mov rax, 0             ; Return nil (0)
    ret

;;; -----------------------------------------------------------------
;;; print_num: Prints a 64-bit signed integer to stdout with a newline
;;; Inputs:  RAX = integer to print
;;; Outputs: RAX = original integer (preserved)
;;; -----------------------------------------------------------------
;;; -----------------------------------------------------------------
;;; print_num: Prints a 64-bit signed integer (NO NEWLINE)
;;; -----------------------------------------------------------------
print_int:
    push rax
    push rcx
    push rdx
    push rsi
    push rdi
    push r8
    push r11

    sub rsp, 32
    lea rsi, [rsp + 31]  ; Point to the very end of the buffer

    mov rcx, rax
    test rax, rax
    jns .convert_loop
    neg rax

.convert_loop:
    xor rdx, rdx
    mov r8, 10
    div r8
    add dl, '0'
    mov [rsi], dl
    dec rsi
    test rax, rax
    jnz .convert_loop

    test rcx, rcx
    jns .print
    mov byte [rsi], '-'
    dec rsi

.print:
    inc rsi
    lea rdx, [rsp + 32]
    sub rdx, rsi         ; RDX = exact string length

    mov rax, 1           ; sys_write
    mov rdi, 1           ; stdout
    syscall

    add rsp, 32

    pop r11
    pop r8
    pop rdi
    pop rsi
    pop rdx
    pop rcx
    pop rax
    ret

;;; -----------------------------------------------------------------
;;; print_hex: Prints RAX as a 16-character hex string (NO NEWLINE)
;;; -----------------------------------------------------------------
print_hex:
    push rax
    push rcx
    push rdx
    push rsi
    push rdi
    push r11
    push r8
    push r9

    ; Allocate 18 bytes on stack for: '0', 'x', 16 hex digits (NO NEWLINE)
    sub rsp, 18
    mov byte [rsp], '0'
    mov byte [rsp+1], 'x'

    mov r8, rax
    mov r9, 15
    mov rcx, 16

.hex_loop:
    mov rax, r8
    and rax, 0xF
    cmp al, 9
    jbe .is_digit
    add al, 'A' - 10
    jmp .store
.is_digit:
    add al, '0'
.store:
    lea rsi, [rsp + 2]
    add rsi, r9
    mov [rsi], al

    shr r8, 4
    dec r9
    dec rcx
    jnz .hex_loop

    mov rax, 1
    mov rdi, 1
    mov rsi, rsp
    mov rdx, 18          ; length = exactly 18 bytes
    syscall

    add rsp, 18

    pop r9
    pop r8
    pop r11
    pop rdi
    pop rsi
    pop rdx
    pop rcx
    pop rax
    ret

;;; -----------------------------------------------------------------
;;; print_char: Prints the lowest byte of RAX as an ASCII character
;;; -----------------------------------------------------------------
print_char:
    push rax
    push rcx
    push rdx
    push rsi
    push rdi
    push r11

    dec rsp              ; Allocate 1 byte on stack
    mov [rsp], al        ; Store the lowest 8 bits of RAX

    mov rax, 1           ; sys_write
    mov rdi, 1           ; stdout
    mov rsi, rsp         ; buffer = rsp
    mov rdx, 1           ; length = 1
    syscall

    inc rsp              ; Clean up 1 byte buffer

    pop r11
    pop rdi
    pop rsi
    pop rdx
    pop rcx
    pop rax
    ret

;;; -----------------------------------------------------------------
;;; print_chunk: Prints an 8-byte integer as a string (stops at null)
;;; -----------------------------------------------------------------
print_chunk:
    push rax
    push rcx
    push rdx
    push rsi
    push rdi
    push r11

    sub rsp, 8
    mov [rsp], rax       ; Put the 8 bytes on the stack

    ; Find exact length (0 to 8) to avoid printing trailing null padding
    mov rcx, 8
.len_loop:
    cmp byte [rsp + rcx - 1], 0
    jnz .found_len
    dec rcx
    jnz .len_loop
    
.found_len:
    test rcx, rcx
    jz .done             ; If length is 0, print nothing

    mov rax, 1           ; sys_write
    mov rdi, 1           ; stdout
    mov rsi, rsp         ; buffer
    mov rdx, rcx         ; exact length
    syscall

.done:
    add rsp, 8
    pop r11
    pop rdi
    pop rsi
    pop rdx
    pop rcx
    pop rax
    ret


;;; -----------------------------------------------------------------
;;; read_char: Reads a single byte from stdin into RAX. 
;;; Returns the byte, or 0 on EOF.
;;; -----------------------------------------------------------------
read_char:
    push rcx
    push rdx
    push rsi
    push rdi
    push r11

    dec rsp              ; Allocate 1 byte buffer on stack
    mov byte [rsp], 0    ; Clear it

    mov rax, 0           ; sys_read
    mov rdi, 0           ; stdin (fd 0)
    mov rsi, rsp         ; buffer pointer
    mov rdx, 1           ; read exactly 1 byte
    syscall

    cmp rax, 1           ; Did the kernel successfully read 1 byte?
    jne .eof

    movzx rax, byte [rsp] ; Zero-extend the byte into 64-bit RAX
    jmp .done

.eof:
    mov rax, 0           ; EOF -> return 0 (false)

.done:
    inc rsp              ; Clean up the 1-byte buffer
    pop r11
    pop rdi
    pop rsi
    pop rdx
    pop rcx
    ret
