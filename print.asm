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
FLAG_PK equ 0002h	; poker frame flag
SIMPLE_FLAGS dw 0

PARAM_FLAG_X    equ 0
PARAM_FLAG_Y    equ 1
PARAM_FLAG_A    equ 2
PARAM_FLAG_UF   equ 3


param_values    dw 0, 0, 07h, 0
param_present   db 4 dup (0)

temp_num_value dw 0
max_len_str dw 0


CHECK_SIMPLE_FLAG MACRO flag_bit, label_if_set
    test [SIMPLE_FLAGS], flag_bit
    jne label_if_set
ENDM

CHECK_PARAM_FLAG MACRO param_index, label_if_set
    test [param_present + param_index], 1
    jne label_if_set
ENDM

GET_PARAM_VALUE MACRO param_index, dest_reg
    mov dest_reg, [param_values + param_index * 2]
    cmp [param_present + param_index], 1
ENDM


FRAMES      STRUC
    def     db 0CDh, 0BAh, 0C9h, 0BBh, 0BCh, 0C8h
    poker   db 07h, 03h, 04h, 05h, 06h
    user    db ?
FRAMES      ENDS


main		proc

            call parse_cmd_args

            xor bx, bx
            mov bl, [cmd_args.count]

            call parse_flags

            call cls
            call print_msg

            call exit

main        endp


;=============================================================================
; SKIPPING SPACES
;=============================================================================

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

            dec cx
            inc di
            
        @@return:
            ret
skip_spaces endp


;=============================================================================
; TERMINAL COMMAND LINE ARGUMENTS PARSING
;=============================================================================

;-----------------------------------------------------------------------------
; Info:
;   Parse command line arguments.
;
;   This procedure parses the command line string at DS:DI, splitting it into
;   individual arguments separated by spaces. Each argument is null-terminated
;   and its pointer is stored in arg_array. The total argument count is saved
;   in arg_count.
;
; Entry:    None
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

parse_cmd_args	proc

            ; TODO: push
            cld
            mov di, 80h
            mov cl, [di]
            xor ch, ch
            xor bx, bx
            inc di

            mov al, ' '
            xor ah, ah

        @@next_char:
            call skip_spaces
            test cx, cx
            je @@return

            mov [cmd_args.array + bx], di
            add bx, 2

            repne scasb
            dec di
            inc cx
            
            test cx, cx
            jne @@next_char

        @@return:
            shr bx, 1
            mov byte ptr [cmd_args.count], bl

            ret
parse_cmd_args  endp


;=============================================================================
; BINARY NUMBER PARSING
;=============================================================================

;-----------------------------------------------------------------------------
; Info:
;   Parse binary number from string
;
;   Converts ASCII binary string (containing only '0' and '1') at DS:DI 
;   to binary number in AX. Stops at first non-binary character.
;
; Entry:
;   DS:DI - pointer to binary string
;   CX    - maximum digits to process (or string length)
;
; Exit:
;   AX    - converted number
;   DI    - updated to first non-binary character
;   CX    - remaining characters (decremented for each digit processed)
;   CF    - 0 if successful, 1 if error occurred
;
; Expected:
;   - String should contain only characters '0' and '1'
;   - CX should contain the number of characters to check
;   - DI must point to a valid character in the string
;
; Destroys:
;   AX - used for converted number
;   BX - used for temporary storage of current character
;
; Notes:
;   - Uses efficient multiplication by 2 using left shift (SHL)
;   - Handles carry propagation from AL to AH using ADC
;   - Stops processing when non-binary character encountered or CX becomes zero
;   - Does NOT check for overflow (AX > 65535)
;   - Maximum 16 bits can be processed (further digits will overflow)
;-----------------------------------------------------------------------------

parse_bin   proc

            push bx cx
            xor ax, ax
        
        @@next_rank:
            mov bl, [di]
            cmp bl, '0'
            jb  @@bin_error
            cmp bl, '1'
            ja  @@bin_error
            
            sub bl, '0'
            
            shl ax, 1
            add al, bl
            adc ah, 0
            
            inc di
            dec cx
            jne @@next_rank
            
            pop cx bx
            clc
            ret
        
        @@bin_error:
            pop cx bx
            stc
            ret
parse_bin   endp


;=============================================================================
; DECIMAL NUMBER PARSING
;=============================================================================

;-----------------------------------------------------------------------------
; Info:
;   Parse decimal number from string
;
;   Converts ASCII decimal string (containing digits 0-9) at DS:DI 
;   to binary number in AX. Stops at first non-digit character.
;
; Entry:
;   DS:DI - pointer to digit string
;   CX    - maximum digits to process (or string length)
;
; Exit:
;   AX    - converted number
;   DI    - updated to first non-digit
;   CX    - remaining characters (decremented for each digit processed)
;   CF    - 0 if successful, 1 if error occurred
;
; Expected:
;   - String should contain only digits 0-9
;   - CX should contain the number of characters to check
;   - DI must point to a valid character in the string
;
; Destroys:
;   AX - used for converted number
;   BX - used for temporary storage during multiplication
;
; Notes:
;   - Uses efficient multiplication by 10 using shifts: (AX << 3) + (AX << 1)
;   - Actually implemented as: AX = AX*2, save in BX, AX = AX*4, add BX
;   - This gives AX*10 without using slow MUL instruction
;   - Handles carry propagation from AL to AH using ADC
;   - Stops processing when non-digit encountered or CX becomes zero
;   - Does NOT check for overflow (AX > 65535)
;   - Maximum 5 digits can be processed (65535 = FFFFh)
;-----------------------------------------------------------------------------

parse_dec   proc

            push bx cx
            xor ax, ax
        
        @@next_rank:
            mov bl, [di]
            cmp bl, '0'
            jb  @@dec_error
            cmp bl, '9'
            ja  @@dec_error
            
            sub bl, '0'
            push bx
            
            shl ax, 1
            mov bx, ax
            shl ax, 2
            add ax, bx
            
            pop bx
            add al, bl
            adc ah, 0
            
            inc di
            dec cx
            jne @@next_rank
            
            pop cx bx
            clc
            ret
        
        @@dec_error:
            pop cx bx
            stc
            ret
parse_dec   endp


;=============================================================================
; HEXADECIMAL NUMBER PARSING
;=============================================================================

;-----------------------------------------------------------------------------
; Info:
;   Parse hexadecimal number from string
;
;   Converts ASCII hexadecimal string at DS:DI to binary number in AX.
;   Accepts digits 0-9 and letters A-F (both uppercase and lowercase).
;   Stops at first invalid character.
;
; Entry:
;   DS:DI - pointer to hex string
;   CX    - maximum digits to process (or string length)
;
; Exit:
;   AX    - converted number
;   DI    - updated to first invalid character
;   CX    - remaining characters (decremented for each digit processed)
;   CF    - 0 if successful, 1 if error occurred
;
; Expected:
;   - String should contain only valid hex characters (0-9, A-F, a-f)
;   - CX should contain the number of characters to check
;   - DI must point to a valid character in the string
;
; Destroys:
;   AX - used for converted number
;   BX - used for temporary storage during conversion
;
; Notes:
;   - Accepts both uppercase (A-F) and lowercase (a-f) letters
;   - Uses efficient multiplication by 16 using left shift by 4 bits (SHL AX, 4)
;   - Handles carry propagation from AL to AH using ADC
;   - Stops processing when invalid character encountered or CX becomes zero
;   - Does NOT check for overflow (AX > 65535)
;   - Maximum 4 hex digits can be processed (FFFFh = 65535)
;-----------------------------------------------------------------------------

parse_hex   proc

            push bx cx
            xor ax, ax
        
        @@next_rank:
            mov bl, [di]
            cmp bl, '0'
            jb  @@hex_error
            cmp bl, '9'
            jbe @@hex_digit
            
            cmp bl, 'A'
            jb  @@hex_error
            cmp bl, 'F'
            jbe @@hex_upper
            
            cmp bl, 'a'
            jb  @@hex_error
            cmp bl, 'f'
            jbe @@hex_lower
            jmp @@hex_error
        
        @@hex_digit:
            sub bl, '0'
            jmp @@hex_add
        
        @@hex_upper:
            sub bl, 'A' - 10
            jmp @@hex_add
        
        @@hex_lower:
            sub bl, 'a' - 10
        
        @@hex_add:
            shl ax, 4
            add al, bl
            adc ah, 0
            
            inc di
            dec cx
            jne @@next_rank

            pop cx bx
            clc
            ret
        
        @@hex_error:
            pop cx bx
            stc
            ret
parse_hex   endp


;=============================================================================
; AUTO-DETECT NUMBER PARSING
;=============================================================================

;-----------------------------------------------------------------------------
; Info:
;   Parse number with automatic base detection
;
;   Detects number base by prefix and calls appropriate parser:
;     - No prefix or starts with digit 1-9: decimal
;     - '0' prefix only: decimal (single zero)
;     - '0x' or '0X' prefix: hexadecimal
;     - '0b' or '0B' prefix: binary
;
; Entry:
;   DS:DI - pointer to number string
;   CX    - maximum characters to process (including prefix)
;
; Exit:
;   AX    - converted number
;   DI    - updated to first character after the number
;   CX    - remaining characters
;   CF    - 0 if successful, 1 if error occurred
;
; Expected:
;   - String should follow the format for detected base
;   - For hex: after 0x prefix, valid hex digits (0-9, A-F, a-f)
;   - For binary: after 0b prefix, only 0 and 1
;   - For decimal: digits 0-9, optionally starting with 0
;   - CX must be large enough to include prefix and at least one digit
;
; Destroys:
;   AX - used for converted number
;   BX - used for base detection
;
; Notes:
;   - Automatically routes to appropriate specialized parser
;   - Handles prefix detection and removal
;   - Single '0' is treated as decimal zero
;   - Returns error if prefix is detected but no digits follow
;   - Case-insensitive for prefix detection (x/X, b/B)
;-----------------------------------------------------------------------------

parse_num_auto  proc
            push bx cx
            mov bl, [di]
            
            cmp bl, '0'
            jne @@try_dec
            
            inc di
            dec cx
            je  @@parse_error_auto
            
            mov bl, [di]
            cmp bl, 'x'
            je  @@do_hex
            cmp bl, 'X'
            je  @@do_hex
            cmp bl, 'b'
            je  @@do_bin
            cmp bl, 'B'
            je  @@do_bin
            
            dec di
            inc cx
            jmp @@do_dec
        
        @@do_hex:
            inc di
            dec cx
            call parse_hex
            pop cx bx
            ret
        
        @@do_bin:
            inc di
            dec cx
            call parse_bin
            pop cx bx
            ret
        
        @@try_dec:
            cmp bl, '1'
            jae @@do_dec
            jmp @@parse_error_auto
        
        @@do_dec:
            pop cx bx
            call parse_dec
            ret
        
        @@parse_error_auto:
            pop cx bx
            stc
            ret
parse_num_auto  endp


;=============================================================================
; TERMINAL COMMAND LINE FLAGS PARSING
;=============================================================================

;-----------------------------------------------------------------------------
; Info:
;   Parses command line arguments to extract flags and their parameters.
;   Supports simple flags (-b) and flags with parameters (-x, -y, -a).
;   Flags with parameters expect the next argument as a numeric value.
;
; Entry:   
;   cmd_args - global structure with command line arguments
;   cmd_args.count - number of arguments (byte)
;   cmd_args.array - array of pointers to argument strings (word)
;
; Exit:    
;   SIMPLE_FLAGS - appropriate bits set for simple flags
;   param_present - array of presence flags for parameters (-x, -y, -a)
;   param_values - array of parameter values (words)
;
; Expected: 
;   Global variables:
;   SIMPLE_FLAGS - byte for storing simple flags
;   FLAG_B - bit constant for -b flag
;   param_present[3] - byte array for parameter presence
;   param_values[3] - word array for parameter values
;   PARAM_FLAG_X, PARAM_FLAG_Y, PARAM_FLAG_A - parameter indices
;
; Destroys: None (preserves all registers)
;
; Notes:
;   - Only arguments starting with '-' are processed as flags
;   - Multiple flags can be combined in one argument (e.g., "-xb 42")
;   - Flags with parameters (-x, -y, -a) consume the next argument
;   - Parameter values are parsed using parse_auto for auto-detection
;   - Empty parameter values are silently ignored
;   - Unknown flags/characters are skipped without error
;   - The procedure preserves all registers
;-----------------------------------------------------------------------------

parse_flags proc

            push ax bx cx dx si di
            
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
            
            push di bx

            cmp al, 'x'
            je @@flag_with_param
            cmp al, 'y'
            je @@flag_with_param
            cmp al, 'a'
            je @@flag_with_param
            
            cmp al, 'b'
            je @@set_b
            
            mov bx, di
            
            cmp word ptr [bx], 'kp'
            je @@flag_pk

            cmp word ptr [bx], 'fu'
            je @@flag_with_param
            
            pop bx di
            jmp @@next_char

        @@set_b:
            or [SIMPLE_FLAGS], FLAG_B
            jmp @@next_char

        @@flag_pk:
            pop bx di
            or [SIMPLE_FLAGS], FLAG_PK
            inc di
            jmp @@next_char

        @@flag_with_param:
            pop bx di
            push ax bx cx dx si di
            
            mov di, [si + bx + 2]
            test di, di
            je @@no_value

            push di
            mov cx, -1
            mov al, ' '
            repne scasb
            not cx
            dec cx
            pop di
            
            call parse_num_auto
            jc  @@parse_error
            
            mov [temp_num_value], ax
            pop di si dx cx bx ax
            mov dx, [temp_num_value]
            
            cmp al, 'x'
            jne @@check_y
            mov [param_values + PARAM_FLAG_X * 2], dx
            mov [param_present + PARAM_FLAG_X], 1
            jmp @@skip_next_arg
            
        @@check_y:
            cmp al, 'y'
            jne @@check_a
            mov [param_values + PARAM_FLAG_Y * 2], dx
            mov [param_present + PARAM_FLAG_Y], 1
            jmp @@skip_next_arg
            
        @@check_a:
            cmp al, 'a'
            jne @@check_uf
            mov [param_values + PARAM_FLAG_A * 2], dx
            mov [param_present + PARAM_FLAG_A], 1
            jmp @@skip_next_arg

        @@check_uf:
            mov bx, di
            cmp word ptr [bx], 'fu'
            jne @@unknown
            mov [param_values + PARAM_FLAG_UF * 2], dx
            mov [param_present + PARAM_FLAG_UF], 1
            
        @@skip_next_arg:
            add bx, 2
            dec cx
            jmp @@next_char
            
        @@no_value:
            pop di si dx cx bx ax
            
        @@parse_error:
            pop di si dx cx bx ax
            
        @@unknown:
        @@next_char:
            inc di
            jmp @@parse_chars
            
        @@next:
            add bx, 2
            dec cx
            jne @@next_arg
            
        @@return:
            pop di si dx cx bx ax
            ret

parse_flags endp


;=============================================================================
; SCREEN CLEANING
;=============================================================================

;-----------------------------------------------------------------------------
; Info:
;   Clears the entire text screen and resets cursor position to home (0,0).
;   Uses BIOS video services to scroll the entire window up.
;
; Entry:    None
;
; Exit:     Screen cleared, cursor positioned at top-left corner (0,0)
;
; Expected: Video mode must support 80x25 text display (standard text mode)
;
; Destroys: None
;
; Notes:    
;   - Uses BIOS interrupt 10h, function 06h (scroll window up)
;   - Scrolls entire screen (rows 0-24, columns 0-79)
;   - Fills screen with light gray on black attribute (07h)
;   - After scrolling, cursor position is reset by BIOS to (0,0)
;   - The procedure preserves all registers
;-----------------------------------------------------------------------------

cls         proc

            push ax bx cx dx

            mov ah, 06h
            xor al, al
            xor cx, cx
            mov dx, 184Fh
            mov bh, 07h
            int 10h

            pop dx cx bx ax

            ret
cls		    endp


;=============================================================================
; SCREEN CLEANING
;=============================================================================

;-----------------------------------------------------------------------------
; Info:
; Entry:
; Exit:
; Expected:
; Destroys:
; Notes
;-----------------------------------------------------------------------------

strlen      proc

            push cx bx
            
            xor ah, ah
            mov bx, ax
            
            shl ax, 4
            shl bx, 6
            add ax, bx
            
            xor ch, ch
            add ax, cx
            shl ax, 1
            
            pop bx cx
            ret
strlen      endp


;=============================================================================
; SCREEN CLEANING
;=============================================================================

;-----------------------------------------------------------------------------
; Info:
; Entry:
; Exit:
; Expected:
; Destroys:
; Notes
;-----------------------------------------------------------------------------

calc_video_offset proc

            push cx bx
            
            xor ah, ah
            mov bx, ax
            
            shl ax, 4
            shl bx, 6
            add ax, bx
            
            xor ch, ch
            add ax, cx
            shl ax, 1
            
            pop bx cx
            ret
calc_video_offset endp


;=============================================================================
; MESSAGE PRINTING
;=============================================================================

;-----------------------------------------------------------------------------
; Info:
;   Parses command line arguments to extract flags and their parameters.
;   Supports simple flags (-b) and flags with parameters (-x, -y, -a, -uf).
;   Flags with parameters expect the next argument as a numeric value.
;
; Entry:
;   Parameters are retrieved via GET_PARAM_VALUE macros:
;   PARAM_FLAG_A - attribute (color) byte in DL
;   PARAM_FLAG_X - X coordinate in CX
;   PARAM_FLAG_Y - Y coordinate in AX
;
; Exit:
;   Message printed to screen at specified coordinates.
;   Background of printed area is filled using the same attribute's
;   background color (text remains unchanged, becomes black).
;   Maximum line length is stored in [max_len_str] (in characters/2).
;
; Expected:
;   cmd_args.array    - contains pointers to message strings
;   param_present     - byte array, 1 if parameter present, 0 otherwise
;   param_values      - word array of parameter values
;   max_len_str       - word variable to store maximum line length
;   calc_video_offset - function that converts (X,Y) to video offset
;   Video mode must be text mode (80x25)
;
; Destroys:
;   AX, BX, CX, DX, SI, DI, ES
;   Flags
;
; Notes:
;   - Messages are displayed at text video memory segment 0B800h
;   - Backslash character (5Ch) forces line feed (moves to next line,
;     same column)
;   - Carriage return (0Dh) terminates the string
;   - Attribute byte from PARAM_FLAG_A is used for all characters
;   - Index into cmd_args.array is calculated from present flags:
;     index = (param_present_X + param_present_Y + param_present_A +
;     param_present_UF) * 4
;   - Background filling preserves original characters, only attribute is
;     modified
;   - All lines are filled to the same width (max_len_str) for rectangular
;     appearance
;-----------------------------------------------------------------------------

print_msg   proc
            
            ; solve number of flag-on

            GET_PARAM_VALUE PARAM_FLAG_A, dx
            GET_PARAM_VALUE PARAM_FLAG_X, cx
            GET_PARAM_VALUE PARAM_FLAG_Y, ax
            
            inc cx
            inc ax
            call calc_video_offset
            mov di, ax
            
            mov ax, 0B800h
            mov es, ax
            
            xor bx, bx
            add bl, [param_present + PARAM_FLAG_X]
            add bl, [param_present + PARAM_FLAG_Y]
            add bl, [param_present + PARAM_FLAG_A]
            add bl, [param_present + PARAM_FLAG_UF]
            shl bx, 2
            mov si, [cmd_args.array + bx]

            mov ah, dl
            mov bx, di
            push di
            push [param_values + PARAM_FLAG_Y]

    @@next_char:
            lodsb
            cmp al, 0Dh
            je @@end_print_msg

            cmp al, 5Ch
            jne @@put_char

            sub bx, di
            lea di, [di + bx + 160]
            inc [param_values + PARAM_FLAG_Y]

            neg bx
            cmp bx, [max_len_str]
            jbe @@not_new_max
            mov [max_len_str], bx 

        @@not_new_max:
            mov bx, di
            jmp @@next_char

        @@put_char:  
            stosw
            jmp @@next_char

    @@end_print_msg:
            sub bx, di
            neg bx
            cmp bx, [max_len_str]
            jbe @@not_last_max
            mov [max_len_str], bx

    @@not_last_max:
        shr [max_len_str], 1
        jmp @@paint_background

    @@paint_background:
            pop cx di
            sub cx, [param_values + PARAM_FLAG_Y]
            neg cx
            
            mov dx, cx
            mov cx, [max_len_str] 

    @@cycle_by_y:
            push cx di
            
    @@cycle_by_x:
            mov al, es:[di]
            stosw
            loop @@cycle_by_x
            
            pop di
            add di, 160
            pop cx
            dec dx
            jnz @@cycle_by_y
            
            ret

print_msg endp


;=============================================================================
; SCREEN CLEANING
;=============================================================================

;-----------------------------------------------------------------------------
; Info:
; Entry:
; Exit:
; Expected:
; Destr:
;-----------------------------------------------------------------------------

draw_frame	proc

            CHECK_SIMPLE_FLAG FLAG_PK @@draw_poker_frame
            CHECK_PARAM_FLAG PARAM_FLAG_UF @@draw_user_frame

            jmp @@draw_default_frame


    @@draw_poker_frame:
            
            ret


    @@draw_user_frame:
            GET_PARAM_VALUE PARAM_FLAG_UF dx
            ret


    @@draw_default_frame:
            
            ret


draw_frame  endp


;=============================================================================
; EXIT
;=============================================================================

;-----------------------------------------------------------------------------
; Info:
; Entry:
; Exit:
; Expected:
; Destroys:
; Notes
;-----------------------------------------------------------------------------

exit		proc

		    mov ax, 4c00h
		    int 21h

		    endp


end start