.model tiny
.code
.386
org 100h

LOCALS @@

start:		jmp main

ARGS    STRUC
    count   db ?
    array   dw 64 dup (0)
ARGS    ENDS

cmd_args ARGS <0>

FLAG_B  equ 0001h	; blink flag
SIMPLE_FLAGS dw 0

PARAM_FLAG_X equ 0
PARAM_FLAG_Y equ 1
PARAM_FLAG_A equ 1

param_values dw 3 dup (0)
param_present db 3 dup (0)


CHECK_SIMPLE_FLAG MACRO flag_bit, label_if_set
    test [SIMPLE_FLAGS], flag_bit
    jne label_if_set
ENDM

GET_PARAM_VALUE MACRO param_index, dest_reg
    mov dest_reg, [param_values + param_index*2]
    cmp [param_present + param_index], 1
ENDM


main		proc

            mov di, 80h
            mov cl, [di]
            xor ch, ch
            inc di

            call parse_args

            ; mov al, [cmd_args.count]

            call parse_flags

            GET_PARAM_VALUE PARAM_FLAG_Y, ax


            ;call print_msg
d

            call exit

main        endp


;-----------------------------------------------------------------------------
; Info:
;   Skip leading spaces in the terminal line.
;
;   This procedure scans the input buffer and advances the pointer (DI) past 
;   any leading space characters (ASCII 0x20). If the entire buffer consists
;   of spaces, DI will point to the last character.
;
; Entry: 
;   DI - pointer to current character in the line buffer
;   CX - number of remaining characters to check (buffer length - current
;   position)
;
; Exit: 
;   DI - updated pointer to the first non-space character (or last character if
;   all spaces)
;
; Expected: 
;   DI - must point to a valid character within the line buffer
;   CX - must contain correct count of remaining characters
;
; Destroys: 
;   AX - used for space character comparison
;
; Notes: 
;   - Uses REPE SCASB for efficient scanning
;   - If CX=0 on entry, procedure returns immediately (no operation)
;   - After scanning all spaces, DI is decremented to point to the last space
;     (or original position if no spaces found)
;-----------------------------------------------------------------------------

skip_spaces proc

            test cx, cx
            je @@return

            mov al, ' '
            xor ah, ah
            repe scasb
            dec di
            
        @@return:
            ret
skip_spaces endp


;-----------------------------------------------------------------------------
; Info:
;   Parse command line arguments.
;
;   This procedure parses the command line string at DS:DI, splitting it into
;   individual arguments separated by spaces. Each argument is null-terminated
;   and its pointer is stored in arg_array. The total argument count is saved
;   in arg_count.
;
; Entry:
;   DS:DI - pointer to command line string (first character after length byte)
;   CX    - length of command line string (from PSP byte at 80h)
;
; Exit:
;   arg_array - filled with word pointers to each argument, terminated by NULL
;   arg_count - number of arguments found
;
; Expected:
;   - Command line string must be in standard PSP format (length at 80h)
;   - arg_array must be large enough for maximum arguments (64 words)
;
; Destroys:
;   AX - used for space character and string operations
;   BX - index into arg_array (final value shifted to count)
;   CX - modified by repne scasb and loop
;   DI - updated to point past the parsed string
;
; Notes:
;   - Uses REPNE SCASB to efficiently find space characters
;   - Leading spaces are skipped by calling skip_spaces
;   - Final argument count = BX/2 (each pointer occupies 2 bytes)
;   - Doesn't modifies the original command line string
;-----------------------------------------------------------------------------

parse_args	proc

            cld
            xor bx, bx

        next_char:
            call skip_spaces
            test cx, cx
            je @@return

            mov [cmd_args.array + bx], di
            add bx, 2

            repne scasb
            test cx, cx
            je @@return

            loop next_char

        @@return:
            shr bx, 1
            mov byte ptr [cmd_args.count], bl

            ret
parse_args  endp


;-----------------------------------------------------------------------------
; Info:
; Entry:
; Exit:
; Expected:
; Destr:
;-----------------------------------------------------------------------------


parse_flags proc

            push bx
            push cx
            push si
            push di
            
            xor cx, cx
            mov cl, [cmd_args.count]
            test cx, cx
            je @@return
            
            mov si, offset cmd_args.array
            xor bx, bx
            
        @@next_arg:
            mov di, [si + bx]
            test di, di
            je @@next
            
            cmp byte ptr [di], '-'
            jne @@next
            
            inc di
            
        @@parse_chars:
            mov al, [di]
            test al, al
            je @@next
            cmp al, ' '
            je @@next
            
            cmp al, 'x'
            je @@flag_with_param
            cmp al, 'y'
            je @@flag_with_param
            cmp al, 'a'
            je @@flag_with_param
            
            cmp al, 'b'
            je @@set_b
            jmp @@next_char
            
        @@set_b:
            or [SIMPLE_FLAGS], FLAG_B
            jmp @@next_char

@@aux_lbl:
    jmp @@next_arg

        @@flag_with_param:
            push ax
            push bx
            push cx
            push si
                        
            mov di, [si + bx + 2]
            test di, di
            je @@no_value

            mov cx, 1
            mov ax, [di]
            cmp al, ' '
            setne al
            xor ah, ah
            add cx, ax 
            call parse_num
            mov dx, ax
            
            pop si
            pop cx
            pop bx
            pop ax

            cmp al, 'x'
            jne @@check_y
            mov [param_values + PARAM_FLAG_X * 2], dx
            mov [param_present + PARAM_FLAG_X], 1
            jmp @@skip_next_arg
            
        @@check_y:
            cmp al, 'y'
            jne @@check_a
            mov [param_values + PARAM_FLAG_Y*2], dx
            mov [param_present + PARAM_FLAG_Y], 1
            jmp @@skip_next_arg
            
        @@check_a:
            cmp al, 'a'
            jne @@unknown
            mov [param_values + PARAM_FLAG_A*2], dx
            mov [param_present + PARAM_FLAG_A], 1
            
        @@skip_next_arg:
            add bx, 2
            dec cx
            jmp @@next_char
            
        @@no_value:
            pop si
            pop bx
            pop ax
            
        @@unknown:
        @@next_char:
            inc di
            jmp @@parse_chars
            
        @@next:
            add bx, 2
            loop @@aux_lbl
            
        @@return:
            pop di
            pop si
            pop cx
            pop bx
            ret

parse_flags endp


;-----------------------------------------------------------------------------
; Info:
;   Parse decimal number from string
;
;   Converts ASCII decimal string at DS:DI to binary number in AX.
;   Stops at first non-digit character.
;
; Entry:
;   DS:DI - pointer to digit string
;   CX    - maximum digits to process (or string length)
;
; Exit:
;   AX    - converted number
;   DI    - updated to first non-digit
;   CX    - remaining characters (decremented for each digit processed)
;
; Expected:
;   - String should contain only digits 0-9
;   - CX should contain the number of characters to check
;   - DI must point to a valid character in the string
;
; Destroys:
;   AX - used for converted number
;   BX - used for teddmporary storage during multiplication
;
; Notes:
;   - Uses efficient multiplication by 10 using shifts: (AX << 3) + (AX << 1)
;   - Actually implemented as: AX = AX*2, save in BX, AX = AX*4, add BX
;   - This gives AX*10 without using slow MUL instruction
;   - Stops processing when non-digit encountered or CX becomes zero
;   - Does NOT check for overflow (AX > 65535)
;-----------------------------------------------------------------------------

parse_num	proc

            xor ax, ax
            
        next_rank:
            mov bl, [di]         ; get character
            cmp bl, '0'
            jb  @@return
            cmp bl, '9'
            ja  @@return
            
            sub bl, '0'          ; convert to digit
            add al, bl
            cmp cx, 2
            jne @@return

            shl ax, 1            ; Multiply AX by 10: AX * 10 = AX * 8 + AX * 2
            mov bx, ax
            shl ax, 2
            add ax, bx
                        
            inc di
            dec cx
            jne next_rank
            
        @@return:
            ret
parse_num	endp


;-----------------------------------------------------------------------------
; Info:
; Entry:
; Exit:
; Expected:
; Destr:
;-----------------------------------------------------------------------------

print_msg	proc

            GET_PARAM_VALUE PARAM_FLAG_X, ax

            ret
		    endp


;-----------------------------------------------------------------------------
; Info:
; Entry:
; Exit:
; Expected:
; Destr:
;-----------------------------------------------------------------------------

draw_frame	proc

            ;mov bx, sp
            ;lea si, [bp+4]

            ;ret
		    endp


exit		proc

		    mov ax, 4c00h
		    int 21h

		    endp

end start