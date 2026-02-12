.model tiny
.code
.386
org 100h

CLS macro
    mov ah, 06h
    xor al, al
    xor cx, cx
    mov dx, 184Fh
    mov bh, 07h
    int 10h
endm

EXIT macro
    mov ax, 4c00h
    int 21h
endm

VROFS macro
	dec bx
	shl ax, 01h
	imul bx, bx, 0A0h		; * 160
	add ax, bx
	mov di, ax
endm

TAKEPRMS macro	
	pop bx		; bx = y
	pop ax		; ax = x
	pop cx		; cx = length
	pop dx		; dx = color
	push dx
	push cx;
	push ax		; return to stack
	push bx
endm


Start:
    mov si, 80h
    mov cl, [si]
    xor ch, ch
    mov si, 81h
    
    cmp cl, 00h
	jnz continue
    EXIT
    
continue:
    mov bx, 10          ; multiplier for parsing a number
    xor ax, ax

	push 07h			; default gray/black color

	mov bp, offset end_skip_spaces_x
	jmp skip_spaces

end_skip_spaces_x:
    xor ax, ax
	
	mov bp, offset end_parse_x
	jmp parse_num

end_parse_x:
    push ax

	mov bp, offset end_skip_spaces_y
	jmp skip_spaces

end_skip_spaces_y:
    xor ax, ax
	
	mov bp, offset end_parse_y
	jmp parse_num

end_parse_y:
    push ax

	mov bp, offset end_skip_spaces_msg
	jmp skip_spaces

end_skip_spaces_msg:

	xor ax, ax
	xor bx, bx

	pop bx		; bx = y
	pop ax		; ax = x
	push cx;
	push ax		; return to stack
	push bx

	VROFS		; calculate video-offset

	CLS

	mov al, [si]
	cmp al, 2Dh
	jne end_parce_color
	jmp parse_color

end_parce_color:
	mov ax, 0B800h
	mov es, ax

	jmp print_msg
    EXIT


return:
	jmp bp


skip_spaces:
    cmp byte ptr [si], ' '
    jne return
    inc si
    dec cx
    jnz skip_spaces
    EXIT
    

parse_num:
    mov bl, byte ptr [si]
    cmp bl, '0'
    jb  return
    cmp bl, '9'
    ja  return
    
    sub bl, '0'
    push bx
    mov bx, 0Ah
    mul bx             ; ax = ax * 10
    pop bx
    add ax, bx
    
    inc si
    dec cx
    jz  return
    jmp parse_num

parse_color:
    inc si
    dec cx
    mov bl, byte ptr [si]
    xor dx, dx
    
    ; red
    cmp bl, 'r'
    sete al
    sub al, 2
    and al, 4fh
    mov dl, al
	cmp bl, 'r'
	je color_done
    
    ; green
    cmp bl, 'g'
    sete al
    sub al, 2
    and al, 2fh
    mov dl, al
	cmp bl, 'g'
	je color_done
    
    ; blue
    cmp bl, 'b'
    sete al
    sub al, 2
    and al, 1fh
    mov dl, al
	cmp bl, 'b'
	je color_done
    
color_done:
    pop ax
	pop bx
	pop cx
	pop es
    push dx
	sub cx, 03h
	push cx
	push bx
	push ax

	add si, 02h
    
    mov bp, offset end_skip_spaces_color
    jmp skip_spaces

end_skip_spaces_color:
	jmp end_parce_color


print_msg:
	TAKEPRMS
	lodsb
	cmp al, 0Dh
	je draw_frame

	mov ah, dl		; fill red
	stosw
	jmp print_msg	


draw_frame:
	TAKEPRMS							; take parametrs from stack
	dec bx								; side over message
	jmp draw_up_side

end_draw_up_side:
	TAKEPRMS							; take parametrs from stack
	inc bx								; side under message
	jmp draw_down_side

end_draw_down_side:
	TAKEPRMS							; take parametrs from stack
	dec ax								; left frame message

	VROFS								; VROFS

	mov cx, 01h							; crutch in the algorithm
	mov bp, offset end_draw_left_side
	jmp draw_vside

end_draw_left_side:
	TAKEPRMS							; take parametrs from stack
	dec ax								; left frame message

	VROFS								; VROFS

	shl cx, 01h
	add di, 02h
	add di, cx

	mov cx, 01h							; crutch in the algorithm
	mov bp, offset end_draw_right_side
	jmp draw_vside

end_draw_right_side:
	EXIT


draw_hside:
	mov al, 0CDh
	rep stosw
	jmp bp


draw_up_side:
	VROFS								; VROFS
	sub di, 02h
	mov al, 0C9h
	mov ah, dl
	stosw

	mov bp, offset tmp_lbl_1
	jmp draw_hside

tmp_lbl_1:
	mov al, 0BBh
	mov ah, dl
	stosw
	jmp end_draw_up_side


draw_down_side:
	VROFS								; VROFS
	sub di, 02h
	mov al, 0C8h	
	mov ah, dl
	stosw

	mov bp, offset tmp_lbl_2
	jmp draw_hside

tmp_lbl_2:
	mov al, 0BCh	
	mov ah, dl
	stosw
	jmp end_draw_down_side


draw_vside:
	mov al, 0BAh						; set attribute
	mov ah, dl
loop_lbl:
	stosw
	add di, 80h							; break to a new line
	add di, 20h
	loop loop_lbl
	jmp bp



end Start